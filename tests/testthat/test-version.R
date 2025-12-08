library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("version returns package version", {
  v <- version(pkg_shared)

  expect_type(v, "character")
  expect_true(nchar(v) > 0)
})
