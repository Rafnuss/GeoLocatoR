# Read Geolocator Data and Orders

This set of functions facilitates the reading and combining of
Geolocator Data (GDL) and associated order information from various
sources. The main function, `read_gdl()`, allows for reading data from
an Access file or directly from separate CSV files containing GDL data
and order information. It returns a data frame that integrates GDL data
with summarized order details.

## Usage

``` r
read_gdl(access_file = NA, data_file = NA, order_file = NA, filter_col = TRUE)

read_gdl_orders(order_file)

read_gdl_data(data_file)

read_gdl_access(
  access_file,
  data_file = tempfile("GDL_Data", fileext = ".csv"),
  order_file = tempfile("GDL_Orders", fileext = ".csv")
)
```

## Arguments

- access_file:

  A string specifying the path to an Access file containing both the GDL
  data and order information. If provided, it takes precedence over
  `data_file` and `order_file`. Defaults to `NA`. §

- data_file:

  A string specifying the path to the GDL data file. Required if
  `access_file` is not provided. Defaults to `NA`.

- order_file:

  A string specifying the path to the GDL order file. Required if
  `access_file` is not provided. Defaults to `NA`.

- filter_col:

  A logical value or a character vector. If `TRUE`, only a predefined
  set of columns is selected. If `FALSE`, all columns are included.
  Alternatively, a character vector can be passed to specify which
  columns to select. Defaults to `TRUE`.

## Value

A data frame combining GDL data and summarized order information. The
data frame includes columns such as `OrderName`, `Client`,
`NumberDelivered`, `GDL_ID`, `Species`, and more, depending on the value
of `filter_col`.

## Details

The `read_gdl()` function is the primary interface for reading and
combining GDL data with orders. It can read from an Access file or from
separate data and order files. If an Access file is provided, the
`read_gdl_access()` function is used to extract the GDL data and order
information. Otherwise, the `read_gdl_data()` and `read_gdl_orders()`
functions are employed to read the data from specified CSV files.

- **`read_gdl()`**: Reads GDL and order data, either from an Access file
  or separate CSV files, and returns a combined data frame.

- **`read_gdl_orders()`**: Reads GDL order information from a CSV file,
  including columns like `OrderID`, `OrderName`, `Species`, `Client`,
  and `NumberDelivered`.

- **`read_gdl_data()`**: Reads GDL data from a CSV file, including
  columns like `DataID`, `OrderName`, `GDL_ID`, `Species`, and spatial
  and temporal information.

- **`read_gdl_access()`**: Extracts GDL data and order information from
  an Access database file and exports them to temporary CSV files.
