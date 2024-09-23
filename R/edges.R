#' Get or set edges
#'
#' @description
#' `edges()` gets the edges from a GeoLocator Data Package object.
#'
#' `edges<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with edges
#' @export
edges <- function(x) {
  check_gldp_pkg(x)
  # pluck(x, "data", "edges")
  frictionless::read_resource(x, resource_name = "edges")
}

#' @rdname edges
#' @param value A data frame to assign as edges
#' @export
"edges<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}."
    )
  }
  # pluck(x, "data", "edges") <- as_tibble(value)

  x <- add_gldp_resource(
    package = x,
    resource_name = "edges",
    data = value,
    replace = "edges" %in% frictionless::resources(x)
  )

  return(x)
}
