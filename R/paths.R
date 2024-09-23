#' Get or set paths
#'
#' @description
#' `paths()` gets the paths from a GeoLocator Data Package object.
#'
#' `paths<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with paths
#' @export
paths <- function(x) {
  check_gldp_pkg(x)
  # pluck(x, "data", "paths")
  frictionless::read_resource(x, resource_name = "paths")
}

#' @rdname paths
#' @param value A data frame to assign as paths
#' @export
"paths<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}."
    )
  }
  # pluck(x, "data", "paths") <- as_tibble(value)

  x <- add_gldp_resource(
    package = x,
    resource_name = "paths",
    data = value,
    replace = "paths" %in% frictionless::resources(x)
  )

  return(x)
}
