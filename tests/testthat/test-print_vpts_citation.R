# Helper function to add references to a vpts object
# This is needed because vpts may not have references
add_references_to_vpts <- function(
  vpts_obj,
  references = c(bibentry(
    bibtype = 'Article',
    key = 'test2024',
    author = person(given = 'John', family = 'Doe'),
    title = 'Test Article',
    year = '2024',
    journal = 'Test Journal'
  ))
) {
  vpts_obj$attributes$references <- references
  vpts_obj
}

test_that("print.vpts prints citation by default when references exist", {
  vpts_test <- add_references_to_vpts(
    example_vpts
  )

  output <- capture.output(print(vpts_test))

  expect_true(any(grepl("references:", output)))
  expect_true(any(grepl("Doe.*2024", output)))
})

test_that("print.vpts can suppress citation with citation=FALSE", {
  vpts_test <- add_references_to_vpts(
    example_vpts
  )

  output <- capture.output(print(vpts_test, references = FALSE))

  expect_false(any(grepl("references:", output)))
})

test_that("print.vpts handles multiple references", {
  vpts_test <- add_references_to_vpts(
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

  output <- capture.output(print(vpts_test))

  expect_true(any(grepl("references:", output)))
  expect_true(any(grepl("Doe.*2024", output)))
  expect_true(any(grepl("Smith.*2025", output)))
  expect_true(any(grepl(";", output)))
})

test_that("print.vpts handles citation objects", {
  vpts_test <- add_references_to_vpts(example_vpts, list(citation('bioRad')))

  output <- capture.output(print(vpts_test))

  expect_true(any(grepl("references:", output)))
  expect_true(any(grepl("et al.*2019", output)))
})

test_that("print.vpts handles vpts without references", {
  # vpts doesn't have references by default

  output <- capture.output(print(example_vpts))

  expect_false(any(grepl("references:", output)))
})

test_that("print.vpts aligns references with other fields", {
  vpts_test <- add_references_to_vpts(
    example_vpts
  )

  output <- capture.output(print(vpts_test))

  ref_line <- output[grepl("references:", output)]
  expect_equal(substr(ref_line, 1, 5), "     ")
})

test_that("summary.vpts passes citation parameter", {
  vpts_test <- add_references_to_vpts(
    example_vpts
  )

  output <- capture.output(summary(vpts_test))
  expect_true(any(grepl("references:", output)))

  output <- capture.output(summary(vpts_test, references = FALSE))
  expect_false(any(grepl("references:", output)))
})

test_that("summary.vpts passes citation parameter", {
  vpts_test <- add_references_to_vpts(
    example_vpts
  )
  withr::local_options(width = 100L)

  output <- capture.output(print(vpts_test))
  expect_false(any(grepl("... (use get_bibliography())", output, fixed = T)))
  expect_lt(max(nchar(output)), 100L)
  withr::local_options(width = 58L)
  output <- capture.output(print(vpts_test))
  expect_true(any(grepl("... (use get_bibliography())", output, fixed = T)))
  expect_identical(nchar(grep(pattern = "references", value = T, output)), 58L)

  withr::local_options(width = 50L)

  output <- capture.output(print(vpts_test))
  expect_true(any(grepl(
    "references:  use get_bibliography()",
    output,
    fixed = T
  )))
})
