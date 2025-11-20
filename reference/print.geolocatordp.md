# Print a GeoLocator Data Package

Prints a human-readable summary of a GeoLocator Data Package, as an
extension of
[`frictionless::print.datapackage()`](https://docs.ropensci.org/frictionless/reference/print.datapackage.html).

## Usage

``` r
# S3 method for class 'geolocatordp'
print(x, ...)
```

## Arguments

- x:

  A GeoLocator Data Package object, as returned by
  [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md).

- ...:

  Further arguments, they are ignored by this function.

## Value

[`print()`](https://rdrr.io/r/base/print.html) with a summary of the
GeoLocator Data Package object.
