% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_gldp.R
\name{read_gldp}
\alias{read_gldp}
\title{Read a GeoLocator Data Package}
\usage{
read_gldp(file = "datapackage.json", force_read = TRUE)
}
\arguments{
\item{file}{A string specifying the path to the JSON file containing the GeoLocator Data Package
metadata. Defaults to \code{"datapackage.json"}. Can also be a url.}

\item{force_read}{Logical to force the reading of the data fomr path/url to memory.}
}
\value{
A GeoLocator Data Package object created from a file/url
}
\description{
This function reads a GeoLocator Data Package from a \code{"datapackage.json"}, file that describes
the Data Package metadata and its Data Resources. The function wraps the function
\href{https://docs.ropensci.org/frictionless/reference/read_package.html}{\code{frictionless::read_package} } and assigns the class
\code{"geolocatordp"} to the datapackage read.
}
\examples{
# Read a datapackage.json file
pkg <- read_gldp("https://zenodo.org/records/14099115/files/datapackage.json")

pkg
}
