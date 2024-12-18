#' Write a GeoLocator Data Package
#'
#' This function wraps [`frictionless::write_package()`
#' ](https://docs.ropensci.org/frictionless/reference/write_package.html), with the significant
#' distinction that it first perform `update_gldp()` before writing the package to files.
#'
#' @param pkg An object of class `"geolocatordp"` representing the GeoLocator Data Package to be
#' updated.
#' @inheritDotParams frictionless::write_package
#'
#' @return `package` invisibly, as written to file.
#'
#' @export
write_gldp <- function(pkg, ...) {
  # Update pkg a last time before writing
  pkg <- write_gldp_computed(pkg)

  # Write using the frictionless function
  frictionless::write_package(pkg, ...)
}

#' Add computed metadata to a GeoLocator Data Package Metadata
#'
#' @description
#' This function compute some metadata of an existing GeoLocator Data Package by setting or updating
#' several fields based on the data contained within the package. Specifically, it updates the
#' `created`,`temporal`, `taxonomic`, and `reference_location` fields based on the data resources
#' present in the package.
#'
#' The function performs the following updates:
#'
#' 1. Sets the `created` field to the current date and time in RFC3339 format.
#' 2. Updates the `temporal` field with the date range of the `datetime` values from the
#'    `measurements` resource, if available.
#' 3. Updates the `taxonomic` field with unique, non-missing `scientific_name` values from the
#'    `tags` resource, if available.
#' 4. Sets the `reference_location` field to the median latitude and longitude values from the
#'    `observations` resource, if available.
#'
#' @param pkg An object of class `"geolocatordp"` representing the GeoLocator Data Package to be
#' updated.
#'
#' @return An updated GeoLocator Data Package object of class `"geolocatordp"` with modified
#' metadata.
#'
#' @export
write_gldp_computed <- function(pkg) {
  # Get current datetime in RFC3339 format
  pkg$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

  if ("measurements" %in% frictionless::resources(pkg)) {
    m <- measurements(pkg)
  } else {
    m <- NULL
  }

  if ("tags" %in% frictionless::resources(pkg)) {
    t <- tags(pkg)
  } else {
    t <- NULL
  }

  if ("observations" %in% frictionless::resources(pkg)) {
    o <- observations(pkg)
  } else {
    o <- NULL
  }

  if ("paths" %in% frictionless::resources(pkg)) {
    paths <- paths(pkg)
  } else {
    paths <- NULL
  }


  # Set temporal
  if (!is.null(m)) {
    pkg$temporal <- m %>%
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
  if (!is.null(t)) {
    pkg$taxonomic <- t %>%
      select("scientific_name") %>%
      pull("scientific_name") %>%
      unique() %>%
      stats::na.omit() %>%
      as.vector()
  }

  # Set spatial
  spatial <- c()
  if (!is.null(o)) {
    spatial <- bind_rows(
      spatial,
      o %>% select(c("latitude", "longitude"))
    )
  }
  if (!is.null(paths)) {
    spatial <- bind_rows(
      spatial,
      paths %>% select(c("latitude", "longitude"))
    )
  }
  if ("pressurepaths" %in% frictionless::resources(pkg)) {
    spatial <- bind_rows(
      spatial,
      pressurepaths(pkg) %>% select(c("latitude", "longitude"))
    )
  }
  # Compute the convex hull
  if (nrow(spatial) > 2) {
    # Convert to sf object
    spatial_sf <- spatial %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) # WGS84 CRS

    # Compute convex hull
    convex_hull <- sf::st_convex_hull(st_union(spatial_sf))

    # Write to GeoJSON
    pkg$spatial <- sf::st_as_geojson(convex_hull)
  }

  # Set reference location
  if (!is.null(o)) {
    pkg$reference_location <- o %>%
      select(c("latitude", "longitude")) %>%
      dplyr::reframe(
        list(
          lat = stats::median(.data$latitude, na.rm = TRUE),
          lon = stats::median(.data$longitude, na.rm = TRUE)
        )
      ) %>%
      pull(1)
  }

  # Number of tags
  pkg$numberTags <- list()
  if (!is.null(t)) {
    pkg$numberTags$equiped <- t %>%
      .data$tag_id %>%
      unique() %>%
      n()
  }
  if (!is.null(m)) {
    pkg$numberTags$light <- m %>%
      filter(.data$sensor == "light") %>%
      .data$tag_id %>%
      unique() %>%
      n()
    pkg$numberTags$pressure <- m %>%
      filter(.data$sensor == "pressure") %>%
      .data$tag_id %>%
      unique() %>%
      n()
    pkg$numberTags$acceleration <- m %>%
      filter(.data$sensor == "acceleration") %>%
      .data$tag_id %>%
      unique() %>%
      n()
  }
  if (!is.null(paths)) {
    pkg$numberTags$paths <- paths %>%
      .data$tag_id %>%
      unique() %>%
      n()
  }

  # Order properties according to the schema
  schema <- jsonlite::fromJSON(pkg$`$schema`, simplifyVector = FALSE)
  properties <- names(schema$allOf[[2]]$properties)
  sorted_pkg <- c(pkg[intersect(properties, names(pkg))], pkg[setdiff(names(pkg), properties)])
  # Ensure the class attribute is preserved
  class(sorted_pkg) <- class(pkg)

  return(sorted_pkg)
}
