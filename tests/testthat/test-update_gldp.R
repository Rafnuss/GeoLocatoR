library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("update_gldp updates package metadata", {
  pkg <- pkg_shared

  # Update package
  pkg_updated <- GeoLocatoR:::update_gldp(pkg)

  # Should still be a valid geolocatordp
  expect_s3_class(pkg_updated, "geolocatordp")

  # Should have updated metadata
  expect_true("title" %in% names(pkg_updated))
})
