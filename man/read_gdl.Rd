% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_gdl.R
\name{read_gdl}
\alias{read_gdl}
\alias{read_gdl_orders}
\alias{read_gdl_data}
\alias{read_gdl_access}
\title{Read Geolocator Data and Orders}
\usage{
read_gdl(access_file = NA, data_file = NA, order_file = NA, filter_col = TRUE)

read_gdl_orders(order_file)

read_gdl_data(data_file)

read_gdl_access(
  access_file,
  data_file = tempfile("GDL_Data", fileext = ".csv"),
  order_file = tempfile("GDL_Orders", fileext = ".csv")
)
}
\arguments{
\item{access_file}{A string specifying the path to an Access file containing both the GDL data
and order information.
If provided, it takes precedence over \code{data_file} and \code{order_file}. Defaults to \code{NA}.  §}

\item{data_file}{A string specifying the path to the GDL data file. Required if \code{access_file}
is not provided. Defaults to \code{NA}.}

\item{order_file}{A string specifying the path to the GDL order file. Required if \code{access_file}
is not provided. Defaults to \code{NA}.}

\item{filter_col}{A logical value or a character vector. If \code{TRUE}, only a predefined set of
columns is selected.
If \code{FALSE}, all columns are included. Alternatively, a character vector can be passed to specify
which columns to select. Defaults to \code{TRUE}.}
}
\value{
A data frame combining GDL data and summarized order information. The data frame
includes columns such as \code{OrderName}, \code{Client}, \code{NumberDelivered}, \code{GDL_ID}, \code{Species}, and more,
depending on the value of \code{filter_col}.
}
\description{
This set of functions facilitates the reading and combining of Geolocator Data (GDL) and
associated order information from various sources.
The main function, \code{read_gdl()}, allows for reading data from an Access file or directly from
separate CSV files containing GDL data and order information.
It returns a data frame that integrates GDL data with summarized order details.
}
\details{
The \code{read_gdl()} function is the primary interface for reading and combining GDL data with
orders. It can read from an Access file or from separate data and order files.
If an Access file is provided, the \code{read_gdl_access()} function is used to extract the GDL data
and order information. Otherwise, the \code{read_gdl_data()} and \code{read_gdl_orders()}
functions are employed to read the data from specified CSV files.
\itemize{
\item \strong{\code{read_gdl()}}: Reads GDL and order data, either from an Access file or separate CSV files,
and returns a combined data frame.
\item \strong{\code{read_gdl_orders()}}: Reads GDL order information from a CSV file, including columns like
\code{OrderID}, \code{OrderName}, \code{Species}, \code{Client}, and \code{NumberDelivered}.
\item \strong{\code{read_gdl_data()}}: Reads GDL data from a CSV file, including columns like \code{DataID},
\code{OrderName}, \code{GDL_ID}, \code{Species}, and spatial and temporal information.
\item \strong{\code{read_gdl_access()}}: Extracts GDL data and order information from an Access database file
and exports them to temporary CSV files.
}
}
