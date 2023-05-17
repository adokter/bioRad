test_that("list_vpts_aloft() returns a character vector", {
  expect_type(
    list_vpts_aloft(date_min = "1990-01-01",
                    date_max = "2050-01-01",
                    radars = c("bejab","bewid")),
    "character"
  )
})

test_that("list_vpts_aloft() returns all data when no dates are provided", {

})
