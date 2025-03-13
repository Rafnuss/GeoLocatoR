library(testthat)

test_that("read_gldp correctly reads and processes GeoLocator Data Package", {
  pkg <- read_gldp("https://zenodo.org/records/14641765/files/datapackage.json")
  expect_s3_class(pkg, "geolocatordp")

  version(pkg)

  check_gldp(pkg)

  validate_gldp(pkg)

  plot(pkg)

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

  # write geopressuretemplate
  project_dir <- create_geopressuretemplate(path = tempfile(), pkg = pkg, open = FALSE)

  # Re-create the pkg from geopressuretemplate
  create_gldp_geopressuretemplate(project_dir) %>%
    add_gldp_geopressuretemplate(project_dir)

  pkg2 <- read_gldp("https://zenodo.org/records/14641662/files/datapackage.json")
  pkg_merged <- merge_gldp(pkg, pkg2)

  validate_gldp(pkg_merged)
})
