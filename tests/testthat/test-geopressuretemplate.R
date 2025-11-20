library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("create_gldp_geopressuretemplate and add_gldp_geopressuretemplate work", {
  pkg <- pkg_shared

  # Create project
  project_dir <- create_geopressuretemplate(
    path = tempfile(),
    pkg = pkg,
    open = FALSE
  )

  # Re-create the pkg from geopressuretemplate
  expect_no_error({
    create_gldp_geopressuretemplate(project_dir) %>%
      add_gldp_geopressuretemplate(project_dir)
  })
})
