# Get GeoLocator DP version

Extracts the version number used by a GeoLocator Data Package object.
This version number indicates what version of the [GeoLocator DP
standard](https://raphaelnussbaumer.com/GeoLocator-DP/) was used.

The version number is derived as follows:

1.  The `version` attribute, if defined.

2.  A version number contained in `x$profile`, which is expected to
    contain the URL to the used GeoLocator DP standard.

3.  `x$profile` in its entirety (can be `NULL`).

## Usage

``` r
version(x)
```

## Arguments

- x:

  A GeoLocator Data Package object, as returned by
  [`create_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/create_gldp.md).
  Also works on a Frictionless Data Package, as returned by
  `read_package()`.

## Value

Character string with the GeoLocator DP version number (e.g. `"v0.2"`).
