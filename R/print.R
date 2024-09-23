#' Print a GeoLocator Data Package
#'
#' Prints a human-readable summary of a GeoLocator Data Package, as an
#' extension of [print.datapackage()].
#'
#' @param x GeoLocator Data Package object, as returned by `read_geolocatordp()`.
#' @param ... Further arguments, they are ignored by this function.
#'
#' @return [print()] with a summary of the GeoLocator Data Package object.
#' @family print functions
#' @export
print.geolocatordp <- function(x, ...) {
  # check_geolocatordp() not necessary: print only triggered for geolocatordp object

  # List resources
  resources <- frictionless::resources(x)
  cat_line(
    format_inline(
      "A GeoLocator Data Package with {length(resources)} resources{?s}{?./:/:}"
    )
  )
  if (length(resources) > 0) {
    purrr::walk(
      resources,
      function(resources) {
        cat_bullet(format_inline("{resources}"))
      }
    )
  }

  # Provide help
  cat_line(
    format_inline(
      "Use {.fun unclass} to print the Data Package as a list."
    ),
    col = "silver"
  )

  invisible(x)
}
