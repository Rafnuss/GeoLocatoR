library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("read_gldp correctly reads a GeoLocator Data Package", {
  expect_true("title" %in% names(pkg_shared))
  expect_true("resources" %in% names(pkg_shared))
  expect_true("contributors" %in% names(pkg_shared))
})

test_that("read_gldp correctly reads a GeoLocator Data Package from all input types", {
  skip("Skipping test to avoid network download")
  # Local file
  pkg_local <- read_gldp("datapackage.json")
  expect_s3_class(pkg_local, "datapackage")
  # Zenodo DOI
  pkg_doi <- read_gldp("10.5281/zenodo.17257521")
  expect_s3_class(pkg_doi, "datapackage")
  # Zenodo DOI URL
  pkg_doi_url <- read_gldp("https://doi.org/10.5281/zenodo.17257521")
  expect_s3_class(pkg_doi_url, "datapackage")
  # Zenodo record link
  pkg_link <- read_gldp("https://zenodo.org/records/17257521")
  expect_s3_class(pkg_link, "datapackage")
  # Zenodo record number
  pkg_num <- read_gldp("17257521")
  expect_s3_class(pkg_num, "datapackage")
})

test_that("read_gldp handles invalid input", {
  # Test with non-existent file
  expect_error(read_gldp("nonexistent.json"))

  # Test with invalid URL
  expect_error(read_gldp("https://invalid.url/nonexistent.json"))
})
