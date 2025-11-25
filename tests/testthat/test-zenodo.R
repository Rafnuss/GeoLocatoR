test_that("zenodo_to_gldp works with valid token", {
  token <- Sys.getenv("ZENODO_TOKEN")
  if (token == "") {
    skip("ZENODO_TOKEN not set")
  }

  zenodo <- ZenodoManager$new(token = token)

  z <- zenodo$getDepositionByConceptDOI("10.5281/zenodo.17367319")
  pkg <- zenodo_to_gldp(z)
  expect_s3_class(pkg, "geolocatordp")

  expect_warning({
    z2 <- gldp_to_zenodo(pkg, token = token)
  })
  expect_s3_class(z2, "ZenodoRecord")
})
