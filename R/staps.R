#' Get or set staps
#'
#' @description
#' `staps()` gets the staps from a GeoLocator Data Package object.
#'
#' `staps<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with staps
#' @export
staps <- function(x) {
  check_gldp(x)
  frictionless::read_resource(x, resource_name = "staps")
}

#' @rdname staps
#' @param value A data frame to assign as staps. Must conform to the staps schema specification.
#' @export
"staps<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}."
    )
  }

  x <- add_gldp_resource(
    package = x,
    resource_name = "staps",
    data = value,
    cast_type = TRUE,
    replace = "staps" %in% frictionless::resources(x)
  )

  x
}
