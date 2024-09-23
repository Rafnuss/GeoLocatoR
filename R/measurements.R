#' Get or set measurements
#'
#' @description
#' `measurements()` gets the measurements from a GeoLocator Data Package object.
#'
#' `measurements<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with measurements
#' @export
measurements <- function(x) {
  check_gldp_pkg(x)
  # pluck(x, "data", "measurements")
  frictionless::read_resource(x, resource_name = "measurements")
}

#' @rdname measurements
#' @param value A data frame to assign as measurements
#' @export
"measurements<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}."
    )
  }

  # pluck(x, "data", "measurements") <- as_tibble(value)
  x <- add_gldp_resource(
    package = x,
    resource_name = "measurements",
    data = value,
    replace = "measurements" %in% frictionless::resources(x)
  )

  return(x)
}
