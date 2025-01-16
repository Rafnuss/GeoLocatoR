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
  check_gldp(x)
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

  x <- add_gldp_resource(
    package = x,
    resource_name = "paths",
    data = value,
    cast_type = TRUE,
    replace = "paths" %in% frictionless::resources(x)
  )

  x <- x %>%
    update_gldp_spatial() %>%
    update_gldp_number_tags()

  return(x)
}
