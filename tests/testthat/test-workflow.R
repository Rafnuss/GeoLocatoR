library(testthat)

test_that("read_gldp correctly reads and processes GeoLocator Data Package", {
  pkg <- read_gldp("https://zenodo.org/records/14099115/files/datapackage.json")
  expect_s3_class(pkg, "geolocatordp")

  version(pkg)

  check_gldp(pkg)

  print(pkg)

  # data manip
  tags(pkg) <- tags(pkg)
  observations(pkg) <- observations(pkg)
  measurements(pkg) <- measurements(pkg)
  staps(pkg) <- staps(pkg)
  twilights(pkg) <- twilights(pkg)
  paths(pkg) <- paths(pkg)
  edges(pkg) <- edges(pkg)
  pressurepaths(pkg) <- pressurepaths(pkg)

  # Update
  pkg <- pkg %>% GeoLocatoR:::update_gldp()
})
