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
update_metadata <- function(pkg) {
  # Order properties according to the schema
  schema <- jsonlite::fromJSON(pkg$`$schema`, simplifyVector = FALSE)
  properties <- names(schema$allOf[[2]]$properties)
  sorted_pkg <- c(pkg[intersect(properties, names(pkg))], pkg[setdiff(names(pkg), properties)])
  # Ensure the class attribute is preserved
  class(sorted_pkg) <- class(pkg)

  return(sorted_pkg)
}

#' @noRd
update_created <- function(pkg) {
  check_gldp_pkg(x)
  # Get current datetime in RFC3339 format
  pkg$created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  return(pkg)
}

#' Update temporal metadata
#'
#' Sets `pkg$temporal$start` to the earliest measurements datetime and `pkg$temporal$end` to the
#' latest measurements datetime.
#' Sets `pkg$temporal` to `NULL` if there are no deployments.
#'
#'
#' @inheritParams update_metadata
#' @return `pkg` with updated temporal metadata.
#' @noRd
update_temporal <- function(pkg) {
  check_gldp_pkg(pkg)

  m <- measurements(pkg)

  if (nrow(m) == 0) {
    pkg$temporal <- NULL
    return(pkg)
  }

  pkg$temporal <- list(
    start = format(min(m$datetime, na.rm = TRUE), "%Y-%m-%d"),
    end = format(max(m$datetime, na.rm = TRUE), "%Y-%m-%d")
  )

  return(pkg)
}

#' Update spatial metadata
#'
#' Sets `pkg$spatial` to a bounding box (expressed as geojson) that encompasses
#' the deployment coordinates or `NULL` if there are no deployments.
#'
#' @inheritParams update_metadata
#' @return `pkg` with updated spatial metadata.
#' @noRd
update_spatial <- function(pkg) {
  check_gldp_pkg(pkg)

  o <- observations(pkg)

  if (nrow(o) == 0) {
    pkg$spatial <- NULL
    return(pkg)
  }

  spatial <- bind_rows(
    spatial,
    o %>% select(c("latitude", "longitude"))
  )

  if ("paths" %in% frictionless::resources(pkg)) {
    spatial <- bind_rows(
      spatial,
      paths(tags) %>% transmute(
        latitude = .data$lat,
        longitude = .data$lon
      )
    )
  }
  if ("pressurepaths" %in% frictionless::resources(pkg)) {
    spatial <- bind_rows(
      spatial,
      pressurepaths(pkg) %>% transmute(
        latitude = .data$lat,
        longitude = .data$lon
      )
    )
  }

  # Compute the min and max
  lat_min <- min(spatial$latitude)
  lat_max <- max(spatial$latitude)
  long_min <- min(spatial$longitude)
  long_max <- max(spatial$longitude)

  x$spatial <- list(
    type = "Polygon",
    coordinates = array(
      c(
        long_min, long_max, long_max, long_min, long_min,
        lat_min, lat_min, lat_max, lat_max, lat_min
      ),
      dim = c(1, 5, 2)
    )
  )

  return(pkg)
}

#' Update taxonomic metadata
#'
#' Sets `pkg$taxonomic` to unique `tags$scientific_name` found in the observations.
#' Sets `pkg$taxonomic` to `NULL` if there are no taxa/observations.
#'
#' @inheritParams update_metadata
#' @return `pkg` with updated taxonomic metadata.
#' @noRd
update_taxonomic <- function(pkg) {
  check_gldp_pkg(pkg)

  t <- tags(pkg)

  if (nrow(t) == 0) {
    pkg$taxonomic <- NULL
    return(pkg)
  }

  if (!is.null(t)) {
    pkg$taxonomic <- t$scientific_name %>%
      unique() %>%
      stats::na.omit() %>%
      as.vector()
  }

  return(pkg)
}

#' Update number of tags metadata
#'
#' Sets `pkg$numberTags` to unique `tags$numberTags` found in the observations.
#' Sets `pkg$numberTags` to `NULL` if there are no taxa/observations.
#'
#' @inheritParams update_metadata
#' @return `pkg` with updated taxonomic metadata.
#' @noRd
update_number_tags <- function(pkg) {
  check_gldp_pkg(pkg)

  t <- tags(pkg)
  m <- measurements(pkg)

  if (nrow(t) == 0 | nrow(m) == 0) {
    pkg$numberTags <- NULL
    return(pkg)
  }

  # Number of tags
  pkg$numberTags <- list(
    equiped = length(unique(t$tag_id)),
    measurements = length(unique(m$tag_id)),
    light = length(unique(m$tag_id[m$sensor == "light"])),
    pressure = length(unique(m$tag_id[m$sensor == "pressure"])),
    activity = length(unique(m$tag_id[m$sensor == "activity"])),
    temperature_external = length(unique(m$tag_id[m$sensor == "temperature_external"])),
    temperature_internal = length(unique(m$tag_id[m$sensor == "temperature_internal"])),
    magnetic = length(unique(m$tag_id[m$sensor == "magnetic_x"]))
  )

  if ("paths" %in% frictionless::resources(pkg)) {
    pkg$numberTags$paths <- length(unique(paths(pkg)$tag_id))
  }
  if ("pressurepaths" %in% frictionless::resources(pkg)) {
    pkg$numberTags$pressurepaths <- length(unique(pressurepaths(pkg)$tag_id))
  }

  return(pkg)
}

#' Update the bibliographicCitation
#'
#' Sets `pkg$bibliographicCitation` to the formatted bibentry of the current pacakge.
#'
#' @inheritParams update_metadata
#' @return `pkg` with updated taxonomic metadata.
#' @noRd
update_bibliographicCitation <- function(pkg) {
  check_gldp_pkg(pkg)

  bib <- utils::bibentry(
    "Misc", # should be "dataset" but not available
    author = contributors2persons(pkg$contributors),
    doi = pkg$id,
    publisher = ifelse(grepl("zenodo", pkg$id, ignore.case = TRUE), "Zenodo", NULL),
    title = pkg$title,
    year = format(as.Date(pkg$created), "%Y"),
    url = pkg$homepage
  )

  pkg$bibliographicCitation <- format(bib)
}
