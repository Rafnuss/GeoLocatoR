# Add GLDP SOI Data to a Package

This function adds data from the Swiss Ornithological Institute (SOI) to
a package. It includes tags, measurements, and observations based on the
provided data frame and directory of data. The function also handles
missing directories and updates the package accordingly.

See an example of use [with this
tutorial](https://rpubs.com/rafnuss/geolocator_create_from_soi).

## Usage

``` r
add_gldp_soi(pkg, gdl, directory_data, generate_observations = TRUE)
```

## Arguments

- pkg:

  The package object to which the data will be added.

- gdl:

  A data frame containing the SOI data. Must include columns like
  `OrderName`, `GDL_ID`, and other relevant fields for tags,
  measurements, and observations. See
  [`read_gdl`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gdl.md)
  for more information.

- directory_data:

  A character string specifying the path to the directory where data
  files are located. This directory is used to locate and match GDL_IDs
  to their corresponding directories.

- generate_observations:

  A logical value indicating whether to create pre-filled observations
  with missing values (date, locations, etc... assuming equipment and
  retrieval.

## Value

The updated package object with the added resources.

## Details

The function performs the following steps:

- Checks and retrieves the directory information for each GDL_ID/tag_id.

- Creates GeoPressureR tag data for each of them when possible

- Extract measurements and add them as resources to pkg

- Compute tags.csv and observations.csv from `gdl` and add them as
  resources too.
