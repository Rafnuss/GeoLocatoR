#' Get or set pressurepaths
#'
#' @description
#' `pressurepaths()` gets the pressurepaths from a GeoLocator Data Package object.
#'
#' `pressurepaths<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with pressurepaths
#' @export
pressurepaths <- function(x) {
  check_gldp_pkg(x)
  # pluck(x, "data", "pressurepaths")
  frictionless::read_resource(x, resource_name = "pressurepaths")
}

#' @rdname pressurepaths
#' @param value A data frame to assign as pressurepaths
#' @export
"pressurepaths<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}."
    )
  }
  # pluck(x, "data", "pressurepaths") <- as_tibble(value)

  x <- add_gldp_resource(
    package = x,
    resource_name = "pressurepaths",
    data = value,
    replace = "pressurepaths" %in% frictionless::resources(x)
  )

  return(x)
}
