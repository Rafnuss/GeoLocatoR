#' Get GeoLocator DP version
#'
#' Extracts the version number used by a GeoLocator Data Package object.
#' This version number indicates what version of the [GeoLocator DP standard](
#' https://raphaelnussbaumer.com/GeoLocator-DP/) was used.
#'
#' The version number is derived as follows:
#' 1. The `version` attribute, if defined.
#' 2. A version number contained in `x$profile`, which is expected to
#' contain the URL to the used GeoLocator DP standard.
#' 3. `x$profile` in its entirety (can be `NULL`).
#'
#' @param x GeoLocator Data Package object, as returned by
#'   `create_gldp()`.
#'   Also works on a Frictionless Data Package, as returned by
#'   `read_package()`.
#' @return GeoLocator DP version number (e.g. `1.0`).
#' @family misc functions
#' @export
version <- function(x) {
  # Get version from attribute
  attr_version <- attr(x, "version")
  if (!is.null(attr_version)) {
    return(attr_version)
  }

  # Get version from profile
  profile <- purrr::pluck(x, "$schema", .default = NA)

  # Find pattern "GeoLocator-dp/<version>/" in e.g.
  # https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/main/geolocator-dp-profile.json
  pattern <- "GeoLocator-DP\\/([0-9A-Za-z]|\\.|-)+\\/"
  match <- grep(pattern, profile)
  if (length(match) > 0) {
    extracted_version <- regmatches(profile, regexpr(pattern, profile))
    extracted_version <- sub("GeoLocator-DP/", "", extracted_version, fixed = TRUE)
    extracted_version <- sub("/", "", extracted_version, fixed = TRUE)
    extracted_version
  } else {
    profile
  }
}
