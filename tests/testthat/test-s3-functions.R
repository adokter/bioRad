# ---- helper: temporarily replace httr2::req_perform (no withr needed) -------
mock_httr2_perform <- function(fun) {
  ns <- asNamespace("httr2")
  was_locked <- bindingIsLocked("req_perform", ns)
  if (was_locked) unlockBinding("req_perform", ns)
  orig <- get("req_perform", envir = ns)
  assign("req_perform", fun, envir = ns)
  if (was_locked) lockBinding("req_perform", ns)
  function() {
    if (bindingIsLocked("req_perform", ns)) unlockBinding("req_perform", ns)
    assign("req_perform", orig, envir = ns)
    lockBinding("req_perform", ns)
  }
}

# ---- minimal httr2 response constructor --------------------------------------
mk_resp <- function(status = 200L, body = "") {
  structure(
    list(
      url = "https://bucket.s3.amazonaws.com",
      status_code = status,
      headers = list(`content-type` = "application/xml"),
      body = charToRaw(body)
    ),
    class = "httr2_response"
  )
}

# ---- XML fixtures -------------------------------------------------------------
list_xml_one <- "
<ListBucketResult xmlns='http://s3.amazonaws.com/doc/2006-03-01/'>
  <IsTruncated>false</IsTruncated>
  <Contents>
    <Key>1998/01/20/KABR/KABR19980120_000821_V06.gz</Key>
    <LastModified>2015-11-01T04:23:58.000Z</LastModified>
    <ETag>\"deadbeef\"</ETag>
    <Size>835954</Size>
    <StorageClass>STANDARD</StorageClass>
  </Contents>
</ListBucketResult>"

list_xml_page1 <- "
<ListBucketResult xmlns='http://s3.amazonaws.com/doc/2006-03-01/'>
  <IsTruncated>true</IsTruncated>
  <NextContinuationToken>abc</NextContinuationToken>
  <Contents>
    <Key>a.gz</Key>
    <LastModified>2020-01-01T00:00:00.000Z</LastModified>
    <ETag>\"1\"</ETag>
    <Size>1</Size>
    <StorageClass>STANDARD</StorageClass>
  </Contents>
</ListBucketResult>"

list_xml_page2 <- "
<ListBucketResult xmlns='http://s3.amazonaws.com/doc/2006-03-01/'>
  <IsTruncated>false</IsTruncated>
  <Contents>
    <Key>b.gz</Key>
    <LastModified>2020-01-02T00:00:00.000Z</LastModified>
    <ETag>\"2\"</ETag>
    <Size>2</Size>
    <StorageClass>STANDARD</StorageClass>
  </Contents>
</ListBucketResult>"

empty_xml <- "
<ListBucketResult xmlns='http://s3.amazonaws.com/doc/2006-03-01/'>
  <IsTruncated>false</IsTruncated>
</ListBucketResult>"

one_xml <- "
<ListBucketResult xmlns='http://s3.amazonaws.com/doc/2006-03-01/'>
  <IsTruncated>false</IsTruncated>
  <Contents><Key>x</Key></Contents>
</ListBucketResult>"

# ---- pure helpers -------------------------------------------------------------
test_that(".s3 endpoint helpers work", {
  expect_equal(.s3_base("noaa-nexrad-level2"), "https://noaa-nexrad-level2.s3.amazonaws.com")
  expect_equal(.s3_strip_endpoint("s3://aloftdata"), "aloftdata")
  expect_equal(.s3_endpoint("s3://bucket"), "https://bucket.s3.amazonaws.com")
  expect_equal(.s3_endpoint("bucket", region = "eu-west-1"),
               "https://bucket.s3.eu-west-1.amazonaws.com")
})

# ---- s3_get_bucket_df: one page ----------------------------------------------
test_that("s3_get_bucket_df parses one page", {
  restore <- mock_httr2_perform(function(req, ...) mk_resp(200L, list_xml_one))
  on.exit(restore(), add = TRUE)

  df <- s3_get_bucket_df("noaa-nexrad-level2", prefix = "1998/01/20/KABR/", max_keys = 5)
  expect_equal(nrow(df), 1)
  expect_true(inherits(df$LastModified, "POSIXct"))
  expect_equal(df$Size, 835954)
  expect_equal(df$Key, "1998/01/20/KABR/KABR19980120_000821_V06.gz")
})

# ---- s3_get_bucket_df: pagination --------------------------------------------
test_that("s3_get_bucket_df paginates", {
  i <- 0L
  restore <- mock_httr2_perform(function(req, ...) {
    i <<- i + 1L
    if (i == 1L) mk_resp(200L, list_xml_page1) else mk_resp(200L, list_xml_page2)
  })
  on.exit(restore(), add = TRUE)

  df <- s3_get_bucket_df("noaa-nexrad-level2", prefix = "x/", max_keys = 1)
  expect_equal(sort(df$Key), c("a.gz", "b.gz"))
  expect_equal(nrow(df), 2)
})

# ---- s3_get_bucket_df: non-2xx error -----------------------------------------
test_that("s3_get_bucket_df errors on non-2xx", {
  restore <- mock_httr2_perform(function(req, ...) mk_resp(403L, "<Error>Forbidden</Error>"))
  on.exit(restore(), add = TRUE)

  expect_error(s3_get_bucket_df("noaa-nexrad-level2", prefix = "x/"), "S3 list error")
})

# ---- s3_prefix_exists ---------------------------------------------------------
test_that("s3_prefix_exists TRUE/FALSE", {
  restore <- mock_httr2_perform(function(req, ...) mk_resp(200L, one_xml))
  on.exit(restore(), add = TRUE)
  expect_true(s3_prefix_exists("bucket", "k/"))

  restore <- mock_httr2_perform(function(req, ...) mk_resp(200L, empty_xml))
  on.exit(restore(), add = TRUE)
  expect_false(s3_prefix_exists("bucket", "k/"))
})

# ---- s3_bucket_exists ---------------------------------------------------------
test_that("s3_bucket_exists true on 200, false on 404", {
  restore <- mock_httr2_perform(function(req, ...) mk_resp(200L, ""))
  on.exit(restore(), add = TRUE)
  expect_true(s3_bucket_exists("bucket"))

  restore <- mock_httr2_perform(function(req, ...) mk_resp(404L, ""))
  on.exit(restore(), add = TRUE)
  expect_false(s3_bucket_exists("bucket"))
})

# ---- s3_save_object -----------------------------------------------------------
test_that("s3_save_object writes and respects overwrite", {
  tmp <- tempfile(); on.exit(unlink(tmp), add = TRUE)

  restore <- mock_httr2_perform(function(req, ...) mk_resp(200L, "ABC"))
  on.exit(restore(), add = TRUE)
  s3_save_object("a.gz", "bucket", tmp, overwrite = TRUE)
  expect_true(file.exists(tmp))
  expect_equal(readBin(tmp, "raw", 3L), charToRaw("ABC"))

  # existing file + overwrite=FALSE -> no request, content unchanged
  restore <- mock_httr2_perform(function(req, ...) stop("should not be called"))
  on.exit(restore(), add = TRUE)
  s3_save_object("a.gz", "bucket", tmp, overwrite = FALSE)
  expect_equal(readBin(tmp, "raw", 3L), charToRaw("ABC"))
})

test_that("s3_save_object errors on non-2xx", {
  tmp <- tempfile(); on.exit(unlink(tmp), add = TRUE)

  restore <- mock_httr2_perform(function(req, ...) mk_resp(403L, "<Error>Forbidden</Error>"))
  on.exit(restore(), add = TRUE)
  expect_error(s3_save_object("deny", "bucket", tmp, overwrite = TRUE), "S3 GET error")
})
