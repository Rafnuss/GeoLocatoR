#' Get or set observations
#'
#' @description
#' `observations()` gets the observations from a GeoLocator Data Package object.
#'
#' `observations<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with observations
#' @export
observations <- function(x) {
  check_gldp(x)
  frictionless::read_resource(x, resource_name = "observations")
}

#' @rdname observations
#' @param value A data frame to assign as observations. Must conform to the observations schema
#'   specification.
#' @export
"observations<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}."
    )
  }

  x <- add_gldp_resource(
    package = x,
    resource_name = "observations",
    data = value,
    cast_type = TRUE,
    replace = "observations" %in% frictionless::resources(x)
  )

  x <- x %>%
    update_gldp_spatial()

  x
}
