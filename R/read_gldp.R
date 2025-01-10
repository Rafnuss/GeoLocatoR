#' Read a GeoLocator Data Package
#'
#' This function reads a GeoLocator Data Package from a `"datapackage.json"`, file that describes
#' the Data Package metadata and its Data Resources. The function wraps the function
#' [`frictionless::read_package`
#' ](https://docs.ropensci.org/frictionless/reference/read_package.html) and assigns the class
#' `"geolocatordp"` to the datapackage read.
#'
#' @param file A string specifying the path to the JSON file containing the GeoLocator Data Package
#' metadata. Defaults to `"datapackage.json"`.
#'
#' @return An object of class `"geolocatordp"` containing the metadata from the GeoLocator Data
#' Package.
#'
#' @examples
#' # Read a datapackage.json file
#' package <- read_gldp("https://zenodo.org/records/14099115/files/datapackage.json")
#'
#' package
#'
#' # Access the Data Package properties
#' package$name
#' package$created
#' @export
read_gldp <- function(file = "datapackage.json") {
  package <- frictionless::read_package(file)

  package$resources <- purrr::map(package$resources, ~ {
    resource <- frictionless:::get_resource(package, .x$name)
    if (resource$read_from == "path" || resource$read_from == "url") {
      .x$data <- frictionless:::read_from_path(package, .x$name, col_select = NULL)
      .x$path <- NULL
    }
    return(.x)
  })

  # Add class
  class(x) <- c("geolocatordp", class(x))

  # Update temporal and spatial scope in metadata
  x <- x %>%
    update_temporal() %>%
    update_spatial() %>%
    update_taxonomic() %>%
    update_number_tags()

  return(x)
}
