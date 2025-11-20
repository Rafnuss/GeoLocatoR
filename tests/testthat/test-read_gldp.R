library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("read_gldp correctly reads a GeoLocator Data Package", {
  pkg <- pkg_shared

  # Check class
  expect_s3_class(pkg, "geolocatordp")
  expect_s3_class(pkg, "datapackage")

  # Check that it has required fields
  expect_true("title" %in% names(pkg))
  expect_true("resources" %in% names(pkg))
  expect_true("contributors" %in% names(pkg))
})

test_that("read_gldp handles invalid input", {
  # Test with non-existent file
  expect_error(read_gldp("nonexistent.json"))

  # Test with invalid URL
  expect_error(read_gldp("https://invalid.url/nonexistent.json"))
})
