library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("print.geolocatordp displays package information", {
  pkg <- pkg_shared

  # Should print without error
  expect_output(print(pkg))
})
