# Helper function to add references to a vpts object
add_references <- function(
  obj,
  references = c(bibentry(
    bibtype = 'Article',
    key = 'test2024',
    author = person(given = 'John', family = 'Doe'),
    title = 'Test Article',
    year = '2024',
    journal = 'Test Journal'
  ))
) {
  obj$attributes$references <- references
  obj
}


# Tests for get_bibliography()

test_that("get_bibliography returns NULL when no references", {
  # vpts doesn't have references by default
  result <- get_bibliography(example_vpts)
  expect_null(result)
})

test_that("get_bibliography returns R objects by default", {
  vpts_test <- add_references(
    example_vpts
  )

  result <- get_bibliography(vpts_test)
  expect_equal(length(result), 1)
  expect_s3_class(result, "bibentry")
})

test_that("get_bibliography returns BibTeX format", {
  vpts_test <- add_references(
    example_vpts
  )

  result <- get_bibliography(vpts_test, format = "bibtex")
  expect_equal(length(result), 6)
  expect_true(is.character(result))
  expect_true(grepl("@Article", result[1]))
})

test_that("get_bibliography handles multiple references", {
  vpts_test <- add_references(
    example_vpts,
    c(
      bibentry(
        bibtype = 'Article',
        key = 'ref1',
        author = person(given = 'John', family = 'Doe'),
        title = 'First Article',
        year = '2024',
        journal = 'Journal A'
      ),
      bibentry(
        bibtype = 'Article',
        key = 'ref2',
        author = person(given = 'Jane', family = 'Smith'),
        title = 'Second Article',
        year = '2025',
        journal = 'Journal B'
      )
    )
  )

  # Test R format
  result_r <- get_bibliography(vpts_test)
  expect_equal(length(result_r), 2)

  # Test BibTeX format
  result_bibtex <- get_bibliography(vpts_test, format = "bibtex")
  expect_equal(length(result_bibtex), 13)
  expect_true(sum(grepl("@Article", result_bibtex)) == 2)
})

test_that("get_bibliography handles citation objects", {
  vpts_test <- add_references(example_vpts, c(citation('bioRad')))

  # Test R format
  result_r <- get_bibliography(vpts_test)
  expect_equal(length(result_r), 1)
  expect_s3_class(result_r, c("citation", "bibentry"))

  # Test BibTeX format
  result_bibtex <- get_bibliography(vpts_test, format = "bibtex")
  expect_gt(length(result_bibtex), 6)
  expect_true(grepl("@Article", result_bibtex)[1])
})

test_that("get_bibliography validates input", {
  expect_error(get_bibliography("not a vpts"))
  expect_error(get_bibliography(list()))
})

test_that("get_bibliography validates format argument", {
  vpts_test <- add_references(
    example_vpts
  )

  expect_error(get_bibliography(vpts_test, format = "invalid"))
})

# Tests for get_bibliography with pvol objects

test_that("get_bibliography returns NULL when no references", {
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  pvol_test <- read_pvolfile(pvolfile)

  result <- get_bibliography(pvol_test)
  expect_null(result)
})

test_that("get_bibliography returns R objects by default", {
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  pvol_test <- add_references(
    read_pvolfile(pvolfile)
  )

  result <- get_bibliography(pvol_test)
  expect_equal(length(result), 1)
  expect_s3_class(result, "bibentry")
})

test_that("get_bibliography returns BibTeX format", {
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  pvol_test <- add_references(
    read_pvolfile(pvolfile)
  )
  result <- get_bibliography(pvol_test, format = "bibtex")
  expect_equal(length(result), 6)
  expect_true(is.character(result))
  expect_true(grepl("@Article", result)[1])
})

test_that("get_bibliography handles multiple references", {
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  pvol_test <- add_references(
    read_pvolfile(pvolfile),
    c(
      bibentry(
        bibtype = 'Article',
        key = 'ref1',
        author = person(given = 'John', family = 'Doe'),
        title = 'First Article',
        year = '2024',
        journal = 'Journal A'
      ),
      bibentry(
        bibtype = 'Article',
        key = 'ref2',
        author = person(given = 'Jane', family = 'Smith'),
        title = 'Second Article',
        year = '2025',
        journal = 'Journal B'
      )
    )
  )

  # Test R format
  result_r <- get_bibliography(pvol_test)
  expect_equal(length(result_r), 2)

  # Test BibTeX format
  result_bibtex <- get_bibliography(pvol_test, format = "bibtex")
  expect_equal(length(result_bibtex), 13)
  expect_equal(sum(grepl("@Article", result_bibtex)), 2)
})

test_that("get_bibliography validates input", {
  expect_error(get_bibliography("not a pvol"))
  expect_error(get_bibliography(list()))
})

test_that("get_bibliography validates format argument", {
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  pvol_test <- add_references(
    read_pvolfile(pvolfile)
  )

  expect_error(get_bibliography(pvol_test, format = "invalid"))
})
