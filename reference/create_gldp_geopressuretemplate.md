# Create a GeoLocator Data Package from a GeoPressure Template

This function creates a GeoLocator Data Package based on the information
found in a `DESCRIPTION` file located in the specified directory. It
utilizes the `create_gldp` function and ensures that required fields are
present, converting them into a format compatible with GeoLocator Data
Package standards.

## Usage

``` r
create_gldp_geopressuretemplate(directory = ".")
```

## Arguments

- directory:

  A string specifying the path to the directory containing the
  GeoPressureTemplate, including the `DESCRIPTION` file.

## Value

A list containing the descriptor for the GeoLocator Data Package.

## Details

The function performs the following steps:

1.  Checks if the provided directory exists.

2.  Reads the `DESCRIPTION` file using the `desc` package.

3.  Verifies the presence of mandatory fields (`Title`, `Authors@R`, and
    `License`) and warns about the absence of suggested fields
    (`Version` and `Description`).

4.  Maps author roles to their corresponding GeoLocator roles.

5.  Creates and returns a GeoLocator Data Package with the information
    extracted from the `DESCRIPTION` file.
