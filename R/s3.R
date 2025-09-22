#' @description Removes s3:// prefix from bucket string
#' @param bucket Character. Bucket name or URL-like string (e.g., "s3://my-bucket")
#' @return Character string (bucket name)
#' @keywords nternal
#' @noRd
.s3_strip_endpoint <- function(bucket) sub("^s3://", "", bucket %||% "")

#' @description Builds S3 HTTPS endpoint URL
#' @param bucket Character. Bucket name
#' @param region Character or NULL. AWS region (e.g., "us-east-1"). If NULL/empty, uses global endpoint
#' @return Character string (URL)
#' @keywords internal
#' @noRd
.s3_endpoint <- function(bucket, region = NULL) {
  bucket <- .s3_strip_endpoint(bucket)
  if (is.null(region) || !nzchar(region)) sprintf("https://%s.s3.amazonaws.com", bucket)
  else sprintf("https://%s.s3.%s.amazonaws.com", bucket, region)
}

# ---- HTTP status -------------------------------------------------------
#   2xx = success
#   3xx = redirect. For bucket existence via the global endpoint 3xx is accepted
#         (S3 may 301 to the regional endpoint), but for LIST/GET 2xx is required
#         because 3xx usually signals a wrong endpoint/region.
#   4xx/5xx = failure (after httr2 retry logic).

#' @description Checks if bucket exists and returns no error codes
#' @param bucket Character. Bucket name (may include "s3://")
#' @param prefix Character. Key prefix to check (e.g., "1998/01/20/KABR/")
#' @param region Character or NULL. AWS region for the bucket
#' @param timeout_s Numeric. Request timeout in seconds
#' @return logical (TRUE/FALSE)
#' @keywords internal
#' @noRd
s3_bucket_exists <- function(bucket) {
  httr2::request(.s3_endpoint(bucket)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_status() |>
    {\(s) s >= 200 && s < 400}()  #2xx codes indicate success and 3xx codes indicate redirect
}


#' @description Checks if prefix exists in bucket by verifying ListObjectsV2 request is successfully answered (2xx)
#' @param max_tries Integer. Max retries for the HTTP request (default 5).
#' @return logical (TRUE/FALSE)
#' @keywords internal
#' @noRd
s3_prefix_exists <- function(bucket, prefix, region = NULL, timeout_s = 10, max_tries = 5) {
  resp <- httr2::request(.s3_endpoint(bucket, region)) |>
    httr2::req_url_query(`list-type` = 2, prefix = prefix, `max-keys` = 1) |>
    httr2::req_timeout(timeout_s) |>
    httr2::req_retry(max_tries = max_tries) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(resp)
  if (status < 200L || status >= 300L) return(FALSE)
  x <- xml2::read_xml(httr2::resp_body_string(resp))
  length(xml2::xml_find_all(x, ".//*[local-name()='Contents']/*[local-name()='Key']")) > 0
}

#' @description List bucket contents via ListObjectsV2 (handles pagination).
#' @param bucket Character. Bucket name (may include "s3://").
#' @param prefix Character. Key prefix to filter (default: "").
#' @param delimiter Character or NULL. If set (e.g., "/"), groups keys by common prefixes
#'   (note: this returns only object `Contents`, not `CommonPrefixes`).
#' @param max Integer or Inf. **Total** cap on objects returned across pages. Use `Inf` for no cap.
#' @param max_keys Integer. **Per-request page size** passed to S3 as `max-keys`
#'   (range 1–1000; S3 defaults to 1000 if omitted). Controls objects per page, not total objects returned.
#' @param region Character or NULL. AWS region for the bucket endpoint.
#' @param max_tries Integer. Max retries for the HTTP request (default 5).
#' @return Data frame with columns: `Key`, `LastModified` (POSIXct UTC), `Size`, `ETag`, `StorageClass`.
#' @keywords internal
#' @noRd
s3_get_bucket_df <- function(bucket, prefix = "", delimiter = NULL,
                             max = Inf, max_keys = 1000, region = NULL,
                             max_tries = 5) {

  # Clamp per-page size to S3's 1..1000 rule
  max_keys_in <- max_keys
  max_keys <- as.integer(max_keys)
  if (is.na(max_keys) || !is.finite(max_keys)) max_keys <- 1000L
  if (max_keys > 1000L) {
    warning("S3 'max-keys' has a hard limit of 1000; ",
            "clamping 'max_keys' from ", max_keys_in, " to 1000. ",
            "Use 'max' to cap the total across pages.")
    max_keys <- 1000L
  }
  if (max_keys < 1L) {
    warning("'max_keys' must be >= 1; clamping to 1.")
    max_keys <- 1L
  }

  # Total cap across pages
  remaining <- if (is.finite(max)) as.integer(max) else Inf
  if (is.finite(remaining) && remaining < 1L) {
    return(data.frame(Key=character(), LastModified=as.POSIXct(character()),
                      Size=double(), ETag=character(), StorageClass=character(),
                      check.names = FALSE))
  }

  token <- NULL
  out <- list()

  repeat {
    # Don’t over-fetch on the final page when a total cap is set
    this_page <- if (is.finite(remaining)) min(max_keys, remaining) else max_keys

    req <- httr2::request(.s3_endpoint(bucket, region)) |>
      httr2::req_url_query(`list-type` = 2, prefix = prefix, `max-keys` = this_page)
    if (!is.null(delimiter)) req <- req |> httr2::req_url_query(delimiter = delimiter)
    if (!is.null(token))     req <- req |> httr2::req_url_query(`continuation-token` = token)

    resp <- req |>
      httr2::req_retry(max_tries = max_tries) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(resp)
    assertthat::assert_that(
      status >= 200 && status < 300,
      msg = sprintf(
        "S3 list error: HTTP %s\n%s",
        status,
        substr(httr2::resp_body_string(resp), 1, 500)
      )
    )

    x <- xml2::read_xml(httr2::resp_body_string(resp))
    nodes <- xml2::xml_find_all(x, ".//*[local-name()='Contents']")
    if (length(nodes)) {
      page_df <- data.frame(
        Key          = xml2::xml_text(xml2::xml_find_all(nodes, "*[local-name()='Key']")),
        LastModified = as.POSIXct(
          xml2::xml_text(xml2::xml_find_all(nodes, "*[local-name()='LastModified']")),
          tz = "UTC"
        ),
        Size         = as.numeric(
          xml2::xml_text(xml2::xml_find_all(nodes, "*[local-name()='Size']"))
        ),
        ETag         = xml2::xml_text(xml2::xml_find_all(nodes, "*[local-name()='ETag']")),
        StorageClass = xml2::xml_text(xml2::xml_find_all(nodes, "*[local-name()='StorageClass']")),
        check.names = FALSE
      )
      out[[length(out) + 1L]] <- page_df
      if (is.finite(remaining)) {
        remaining <- remaining - nrow(page_df)
        if (remaining <= 0L) break
      }
    }

    is_truncated <- identical(
      tolower(xml2::xml_text(xml2::xml_find_first(x, ".//*[local-name()='IsTruncated']"))),
      "true"
    )
    if (!is_truncated) break

    token <- xml2::xml_text(xml2::xml_find_first(x, ".//*[local-name()='NextContinuationToken']"))
    if (!nzchar(token)) break
  }

  if (length(out)) {
    res <- do.call(rbind, out)
    if (is.finite(max) && nrow(res) > max) res <- res[seq_len(max), , drop = FALSE]
    res
  } else {
    data.frame(Key=character(), LastModified=as.POSIXct(character()),
               Size=double(), ETag=character(), StorageClass=character(),
               check.names = FALSE)
  }
}


#' @description download S3 object to local file. # aws.s3::save_object() replacement
#' @param object Character. Object key (path inside the bucket), e.g. `"baltrad/monthly/bejab_202305.zip"`.
#' @param bucket Character. Bucket name (may include `"s3://"`; trailing slashes ignored).
#' @param file Character. Destination file path on disk.
#' @param overwrite Logical. If `FALSE` and `file` exists, the download is skipped.
#' @param region Character or NULL. AWS region for the endpoint (e.g., `"eu-west-1"`). If `NULL`,
#'   the global endpoint is used (S3 may redirect as needed).
#' @param max_tries Integer. Max automatic retries for the HTTP request (default `5`).
#' @return character string (file path, invisibly)
#' @keywords internal
#' @noRd
s3_save_object <- function(object, bucket, file, overwrite = FALSE, region = NULL, max_tries = 5) {

  assertthat::assert_that(assertthat::is.string(object) && nzchar(object),
              msg = "'object' must be a non-empty character string (S3 key)")
   assertthat::assert_that(assertthat::is.string(bucket) && nzchar(bucket),
              msg = "'bucket' must be a non-empty character string (may include 's3://')")
   assertthat::assert_that(assertthat::is.string(file) && nzchar(file),
              msg = "'file' must be a non-empty destination path (character string)")
   assertthat::assert_that(assertthat::is.flag(overwrite),
              msg = "'overwrite' must be a single TRUE/FALSE value")
   assertthat::assert_that(is.null(region) || (assertthat::is.string(region) && nzchar(region)),
              msg = "'region' must be NULL or a non-empty AWS region string like 'eu-west-1'")
   assertthat::assert_that(assertthat::is.count(max_tries),
              msg = "'max_tries' must be a positive integer")

  if (file.exists(file) && !overwrite) return(invisible(file))
  url <- paste0(.s3_endpoint(bucket, region), "/", utils::URLencode(object, reserved = TRUE))
  resp <- httr2::request(url) |>
    httr2::req_retry(max_tries = max_tries) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  status <- httr2::resp_status(resp)
status <- httr2::resp_status(resp)
assertthat::assert_that(
  status >= 200 && status < 300,
  msg = sprintf(
    "S3 GET error %s for %s\n%s",
    status, object, substr(httr2::resp_body_string(resp), 1, 400)
  )
)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeBin(httr2::resp_body_raw(resp), file)
  invisible(file)
}
