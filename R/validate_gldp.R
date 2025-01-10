#' Validate a GeoLocator Data Package
#'
#' This function validates a GeoLocator Data Package using the `frictionless` command-line tool. It
#' writes the package metadata to a JSON file and performs validation to ensure that the package
#' conformsto the required standards. The function supports two modes: validating only the package
#' metadata or validating the entire package including its resources.
#'
#' The function performs the following steps:
#'
#' 1. If `only_package` is `TRUE` or the package contains no resources, it writes only the metadata
#'    to a JSON file and validates it.
#' 2. If `only_package` is `FALSE` and resources are present, it writes the entire package,
#'    including resources, to a directory and validates it.
#' 3. Executes the `frictionless` validation command using the specified path to the `frictionless`
#'  executable.
#'
#' @param pkg An object of class `"geolocatordp"` representing the GeoLocator Data Package to be
#' validated.
#' @param path A string specifying the path to the directory containing the `frictionless`
#' executable. Defaults to `"/Users/rafnuss/anaconda3/bin/"`.
#' @param only_package A logical indicating whether to validate only the package metadata (TRUE) or
#' the entire package including resources (FALSE). Defaults to `NULL`, in which case it is
#' determined based on the presence of resources in the package.
#' @param pkg_dir A string specifying the directory where the package files will be written for
#' validation. Defaults to a temporary directory created with `tempdir()`.
#'
#' @return NULL. The function performs validation as a side effect and does not return a value.
#'
#' @export
validate_gldp <- function(pkg,
                          path = "/Users/rafnuss/anaconda3/bin/",
                          only_package = NULL,
                          pkg_dir = tempdir()) {
  if (is.null(only_package)) {
    only_package <- length(pkg$resources) == 0
  }

  if (only_package) {
    package_json <- jsonlite::toJSON(
      pkg,
      pretty = TRUE,
      null = "null",
      na = "null",
      auto_unbox = TRUE
    )
    pkg_file <- file.path(pkg_dir, "datapackage.json")
    write(package_json, pkg_file)
  } else {
    frictionless::write_package(pkg, pkg_dir)
  }

  system(glue::glue("{path}frictionless validate {pkg_dir}"))
}
