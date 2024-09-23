#' Read a GeoLocator Data Package
#'
#' This function reads a GeoLocator Data Package from a specified JSON file. The file should conform
#' to the GeoLocator Data Package format. The function utilizes the `frictionless` package to read the
#' package metadata and then assigns the class `"geolocatordp"` to the resulting object for proper
#' identification and handling.
#'
#' @param file A string specifying the path to the JSON file containing the GeoLocator Data Package
#' metadata. Defaults to `"datapackage.json"`.
#'
#' @return An object of class `"geolocatordp"` containing the metadata from the GeoLocator Data Package.
#'
#' @export
read_gldp <- function(file = "datapackage.json") {
  # Read package (metadata)
  package <- frictionless::read_package(file)

  class(package) <- c("geolocatordp", class(package))

  return(package)
}
