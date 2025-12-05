library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("gldp_to_eml writes eml.xml and returns EML-like object", {
  skip_if_not_installed("EML")

  pkg <- pkg_shared

  # Allow for HTML entities like &nbsp; in free-text fields
  pkg$description <- paste0(pkg$description, " with non-breaking space: &nbsp;")
  tmp <- withr::local_tempdir()

  eml <- gldp_to_eml(pkg, directory = tmp)

  # File is written
  expect_true(file.exists(file.path(tmp, "eml.xml")))

  # Basic structure of returned object
  expect_type(eml, "list")
  expect_true("dataset" %in% names(eml))

  # Key metadata fields are transferred
  expect_equal(eml$dataset$title, pkg$title)
  # expect_equal(eml$dataset$abstract$para, pkg$description)
  expect_equal(eml$dataset$keywordSet$keyword, pkg$keywords)

  # packageId behaviour: existing ID is reused, otherwise UUID-like
  if (is.null(pkg$id)) {
    expect_type(eml$packageId, "character")
    expect_match(eml$packageId, "^[0-9a-f\\-]+$")
  } else {
    expect_equal(eml$packageId, pkg$id)
  }

  # Coverage element is present as list (may be empty or populated)
  expect_type(eml$dataset$coverage, "list")
})
