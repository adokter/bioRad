test_that("extract_string() can extract a string from a vector", {
  expect_identical(
    extract_string(
      "These are the voyages of the starship Enterprise",
      "[a-z]{8}(?= Enterprise)",
      perl = TRUE
    ),
    "starship"
  )
})
