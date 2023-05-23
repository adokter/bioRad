test_that("extract_string() can extract a string from a vector", {
  expect_true(all(sapply(letters, function(pattern)
    extract_string(letters, pattern) == pattern)))
})
