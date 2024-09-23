#' Update a GeoLocator Data Package Metadata
#'
#' This function updates the metadata of an existing GeoLocator Data Package by setting or updating
#' several fields based on the data contained within the package. Specifically, it updates the `created`,
#' `temporal`, `taxonomic`, and `reference_location` fields based on the data resources present in the
#' package.
#'
#' The function performs the following updates:
#'
#' 1. Sets the `created` field to the current date and time in RFC3339 format.
#' 2. Updates the `temporal` field with the date range of the `datetime` values from the `measurements`
#'    resource, if available.
#' 3. Updates the `taxonomic` field with unique, non-missing `scientific_name` values from the `tags`
#'    resource, if available.
#' 4. Sets the `reference_location` field to the median latitude and longitude values from the `observations`
#'    resource, if available.
#'
#' @param pkg An object of class `"geolocatordp"` representing the GeoLocator Data Package to be updated.
#'
#' @return An updated GeoLocator Data Package object of class `"geolocatordp"` with modified metadata.
#'
#' @export
update_gldp <- function(pkg) {
  # Get current datetime in RFC3339 format
  pkg$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

  # Set temporal
  if ("measurements" %in% frictionless::resources(pkg)) {
    pkg$temporal <- measurements(pkg) %>%
      select("datetime") %>%
      dplyr::reframe(
        list(
          start = format(min(.data$datetime, na.rm = TRUE), "%Y-%m-%d"),
          end = format(max(.data$datetime, na.rm = TRUE), "%Y-%m-%d")
        )
      ) %>%
      pull(1)
  }

  # Set taxonomic
  if ("tags" %in% frictionless::resources(pkg)) {
    pkg$taxonomic <- tags(pkg) %>%
      select("scientific_name") %>%
      pull("scientific_name") %>%
      unique() %>%
      stats::na.omit()
  }

  # Set reference location
  if ("observations" %in% frictionless::resources(pkg)) {
    pkg$reference_location <- observations(pkg) %>%
      select(c("latitude", "longitude")) %>%
      dplyr::reframe(
        list(
          lat = stats::median(.data$latitude, na.rm = TRUE),
          lon = stats::median(.data$longitude, na.rm = TRUE)
        )
      ) %>%
      pull(1)
  }

  return(pkg)
}
