#' Get or set twilights
#'
#' @description
#' `twilights()` gets the twilights from a GeoLocator Data Package object.
#'
#' `twilights<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with twilights
#' @export
twilights <- function(x) {
  check_gldp_pkg(x)
  frictionless::read_resource(x, resource_name = "twilights")
}

#' @rdname twilights
#' @param value A data frame to assign as twilights
#' @export
"twilights<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}."
    )
  }

  x <- add_gldp_resource(
    package = x,
    resource_name = "twilights",
    data = value,
    cast_type = TRUE,
    replace = "twilights" %in% frictionless::resources(x)
  )

  return(x)
}
