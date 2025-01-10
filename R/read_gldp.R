#' Read a GeoLocator Data Package
#'
#' This function reads a GeoLocator Data Package from a specified JSON file. The file should conform
#' to the GeoLocator Data Package format. The function utilizes the `frictionless` package to read
#' the package metadata and then assigns the class `"geolocatordp"` to the resulting object for
#' proper identification and handling.
#'
#' @param file A string specifying the path to the JSON file containing the GeoLocator Data Package
#' metadata. Defaults to `"datapackage.json"`.
#'
#' @return An object of class `"geolocatordp"` containing the metadata from the GeoLocator Data
#' Package.
#'
#' @export
read_gldp <- function(file = "datapackage.json") {
  # Read package (metadata)
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
