precip_fixture <- function(type) {
  x <- if (type == "vp") example_vp else example_vpts[1:3]
  x$attributes$where$height <- 0
  x$data$DBZH[] <- 30
  quantities <- c("dens", "eta", "dbz")
  for (i in seq_along(quantities)) {
    quantity <- quantities[i]
    x$data[[quantity]][] <- 10
    if (type == "vp") {
      x$data[[quantity]][i] <- NA_real_
      x$data[[quantity]][i + 3] <- NaN
    } else {
      x$data[[quantity]][i, 1] <- NA_real_
      x$data[[quantity]][i + 3, 1] <- NaN
      x$data[[quantity]][, 3] <- NA_real_
    }
  }
  if (type == "vpts") x$data$DBZH[, 2] <- 0
  x
}

for (type in c("vp", "vpts")) {
  for (all_heights in c(TRUE, FALSE)) {
    test_that(paste("filter_precip() preserves missing values for", type,
                    "with filter_all_heights =", all_heights), {
      x <- precip_fixture(type)
      result <- filter_precip(x, dbz = 20, range = 600, alt_min = 200,
                              alt_max = 1200, filter_all_heights = all_heights)
      expected <- x
      heights <- if (all_heights) seq_len(25) else 1:6
      replacements <- c(dens = 0, eta = 0, dbz = -Inf)
      for (quantity in names(replacements)) {
        for (height in heights) {
          if (type == "vp") {
            if (!is.na(x$data[[quantity]][height])) {
              expected$data[[quantity]][height] <- replacements[[quantity]]
            }
          } else {
            for (profile in c(1, 3)) {
              if (!is.na(x$data[[quantity]][height, profile])) {
                expected$data[[quantity]][height, profile] <- replacements[[quantity]]
              }
            }
          }
        }
      }
      expect_identical(result, expected)
    })
  }

  test_that(paste("filter_precip() leaves dry or unknown profiles unchanged for", type), {
    x <- precip_fixture(type)
    for (reflectivity in c(0, NA_real_)) {
      x$data$DBZH[] <- reflectivity
      expect_identical(filter_precip(x), x)
    }
  })
}

test_that("filter_precip() can still drop rainy profiles", {
  x <- precip_fixture("vpts")
  expect_identical(filter_precip(x, dbz = 20, drop = TRUE), x[2])
})
