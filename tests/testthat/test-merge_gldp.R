library(testthat)
library(GeoLocatoR)

# pkg_shared and pkg2_shared are loaded from setup.R

test_that("merge_gldp merges two packages", {
  pkg <- pkg_shared
  pkg2 <- pkg2_shared

  # Merge packages
  expect_warning({
    pkg_merged <- merge_gldp(pkg, pkg2)
  })

  # Should be a valid geolocatordp
  expect_s3_class(pkg_merged, "geolocatordp")

  # Should validate
  expect_no_error(validate_gldp(pkg_merged))

  # Should have tags from both packages
  merged_tags <- tags(pkg_merged)
  expect_true(nrow(merged_tags) > 0)
})

test_that("merge_gldp errors on packages with duplicate tag_ids", {
  pkg <- pkg_shared

  # Merging package with itself should error due to duplicate tag_ids
  expect_error(merge_gldp(pkg, pkg), "Duplicate.*tag_id")
})
