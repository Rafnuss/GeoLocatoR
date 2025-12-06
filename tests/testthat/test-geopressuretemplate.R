library(testthat)
library(GeoLocatoR)

# pkg_shared is loaded from setup.R

test_that("create_gldp_geopressuretemplate and add_gldp_geopressuretemplate work", {
  # Create project
  suppressMessages({
    project_dir <- create_geopressuretemplate(
      path = tempfile(),
      pkg = pkg_shared,
      open = FALSE
    )
  })

  # Re-create the pkg from geopressuretemplate
  expect_no_error({
    suppressMessages({
      create_gldp_geopressuretemplate(project_dir) %>%
        add_gldp_geopressuretemplate(project_dir)
    })
  })

  t <- config2tibble(file = file.path(project_dir, "config.yml"))
  t <- config2tibble(file = file.path(project_dir, "config.yml"), filter_return = FALSE)

  expect_true(all(c("id", "tag_create.manufacturer") %in% names(t)))
})
