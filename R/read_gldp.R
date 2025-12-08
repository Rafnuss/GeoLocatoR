#' Read a GeoLocator Data Package
#'
#' @description
#' This function reads a GeoLocator Data Package from a `"datapackage.json"` file that describes
#' the Data Package metadata and its Data Resources. The function wraps the function
#' [`frictionless::read_package`
#' ](https://docs.ropensci.org/frictionless/reference/read_package.html) and assigns the class
#' `"geolocatordp"` to the datapackage read.
#'
#' @param x A string specifying the path to the JSON file containing the GeoLocator Data Package
#' metadata. Defaults to `"datapackage.json"`. Also accepts direct urls, Zenodo DOI, Zenodo link, or Zenodo record number.
#' @param force_read Logical indicating whether to force the reading of the data from path/URL to
#'   memory. Defaults to `TRUE`.
#'
#' @return A GeoLocator Data Package object created from a file/url
#'
#' @examples
#' \dontrun{
#' # Local file
#' read_gldp("datapackage.json")
#' # Zenodo DOI
#' read_gldp("10.5281/zenodo.15259676")
#' # Zenodo DOI URL
#' read_gldp("https://doi.org/10.5281/zenodo.15259676")
#' # Zenodo record link
#' read_gldp("https://zenodo.org/records/15259676")
#' # Zenodo record number
#' read_gldp("15259676")
#' }
#' # Read a datapackage.json file
#' pkg <- read_gldp("https://zenodo.org/records/14099115/files/datapackage.json")
#'
#' pkg
#' @export
read_gldp <- function(x = "datapackage.json", force_read = TRUE) {
  # If x ends with .json, use as file
  if (grepl("\\.json$", x)) {
    file <- x
  } else {
    doi_pattern <- "^10\\.5281/zenodo\\.(\\d+)$"
    doi_url_pattern <- "^https?://doi\\.org/10\\.5281/zenodo\\.(\\d+)$"
    zenodo_pattern <- "^https?://zenodo\\.org/records/(\\d+)"
    record_pattern <- "^(\\d+)$"
    rec_id <- NULL
    if (grepl(doi_pattern, x)) {
      rec_id <- sub(doi_pattern, "\\1", x)
    } else if (grepl(doi_url_pattern, x)) {
      rec_id <- sub(doi_url_pattern, "\\1", x)
    } else if (grepl(zenodo_pattern, x)) {
      rec_id <- sub(zenodo_pattern, "\\1", x)
    } else if (grepl(record_pattern, x)) {
      rec_id <- sub(record_pattern, "\\1", x)
    }
    if (!is.null(rec_id)) {
      file <- glue::glue(
        "https://zenodo.org/records/{rec_id}/files/datapackage.json"
      )
    } else {
      file <- x
    }
  }

  pkg <- frictionless::read_package(file)

  # Force the read of the data as
  if (force_read) {
    pkg$resources <- purrr::map(pkg$resources, \(r) {
      resource <- frictionless:::get_resource(pkg, r$name)
      if (resource$read_from == "path" || resource$read_from == "url") {
        r$data <- frictionless:::read_from_path(pkg, r$name, col_select = NULL)
        r$data <- cast_table(r$data, r$schema)
        r$path <- NULL
      }
      r
    })
  }

  # Add class
  class(pkg) <- c("geolocatordp", class(pkg))

  if (!grepl("geolocator-dp-profile\\.json$", pkg$`$schema`)) {
    cli_warn(
      "The datapackage provided does not seem to be a GeoLocator Data Package."
    )
  }

  pkg
}
