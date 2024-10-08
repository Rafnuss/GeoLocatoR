% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_gldp_soi.R
\name{add_gldp_soi}
\alias{add_gldp_soi}
\title{Add GLDP SOI Data to a Package}
\usage{
add_gldp_soi(pkg, gdl, directory_data, allow_empty_o = FALSE, replace = FALSE)
}
\arguments{
\item{pkg}{The package object to which the data will be added.}

\item{gdl}{A data frame containing the SOI data. Must include columns like \code{OrderName},
\code{GDL_ID}, and other relevant fields for tags, measurements, and observations.}

\item{directory_data}{A character string specifying the path to the directory where data files
are located. This directory is used to locate and match GDL_IDs to their corresponding
directories.}

\item{allow_empty_o}{A logical value indicating whether to allow observations with missing
datetime values. Default is \code{FALSE}.}

\item{replace}{A logical value indicating whether to replace existing resources in the package
with the new data. Default is \code{FALSE}.}
}
\value{
The updated package object with the added resources.
}
\description{
This function adds data from the Swiss Ornithological Institute (SOI) to a package. It includes
tags, measurements, and observations based on the provided data frame and directory of data. The
function also handles missing directories and updates the package accordingly.
}
\details{
The function performs the following steps:
\itemize{
\item Validates the input package and the data frame.
\item Checks and retrieves the directory information for each GDL_ID.
\item Creates tag data, measurements, and observations based on the provided data.
\item Adds these resources to the package, with options to replace existing resources.
\item Updates the package metadata.
}
}
\examples{
\dontrun{
my_package <- some_package_function()
my_gdl <- data.frame(
  OrderName = c("A", "B"),
  GDL_ID = c("001", "002"),
  # Additional required columns...
)
updated_package <- add_gldp_soi(
  pkg = my_package,
  gdl = my_gdl,
  directory_data = "/path/to/data",
  allow_empty_o = TRUE,
  contributors = list("John Doe" = c("aut", "DataCollector")),
  replace = TRUE
)
}

}
