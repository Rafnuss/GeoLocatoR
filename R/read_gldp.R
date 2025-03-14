#' Read a GeoLocator Data Package
#'
#' This function reads a GeoLocator Data Package from a `"datapackage.json"`, file that describes
#' the Data Package metadata and its Data Resources. The function wraps the function
#' [`frictionless::read_package`
#' ](https://docs.ropensci.org/frictionless/reference/read_package.html) and assigns the class
#' `"geolocatordp"` to the datapackage read.
#'
#' @param file A string specifying the path to the JSON file containing the GeoLocator Data Package
#' metadata. Defaults to `"datapackage.json"`. Can also be a url.
#' @param force_read Logical to force the reading of the data from path/url to memory.
#'
#' @return A GeoLocator Data Package object created from a file/url
#'
#' @examples
#' # Read a datapackage.json file
#' pkg <- read_gldp("https://zenodo.org/records/14099115/files/datapackage.json")
#'
#' pkg
#' @export
read_gldp <- function(file = "datapackage.json", force_read = TRUE) {
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
    cli::cli_warn("The datapackage provided does not seems to be a Geolocator Data Package.")
  }

  pkg
}
