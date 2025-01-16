#' Get or set tags
#'
#' @description
#' `tags()` gets the tags from a GeoLocator Data Package object.
#'
#' `tags<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams print.geolocatordp
#' @return [tibble::tibble()] data frame with tags
#' @export
tags <- function(x) {
  check_gldp(x)
  frictionless::read_resource(x, resource_name = "tags")
}

#' @rdname tags
#' @param value A data frame to assign as tags
#' @export
"tags<-" <- function(x, value) {
  if (!is.data.frame(value)) {
    cli_abort(
      "{.arg value} must be a data.frame, not {.type {value}}."
    )
  }

  x <- add_gldp_resource(
    package = x,
    resource_name = "tags",
    data = value,
    cast_type = TRUE,
    replace = "tags" %in% frictionless::resources(x)
  )

  x <- x %>%
    update_gldp_taxonomic() %>%
    update_gldp_number_tags()

  return(x)
}
