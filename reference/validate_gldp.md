# Validate a GeoLocator Data Package

This function performs a comprehensive validation of a GeoLocator Data
Package by checking the package metadata, profile, and resources. The
validation includes verifying that the package conforms to the
GeoLocator Data Package profile and that each resource adheres to its
schema.

If `quiet` is `TRUE`, the function suppresses the output of the `cli`
package's messages.

## Usage

``` r
validate_gldp(pkg, quiet = FALSE)
```

## Arguments

- pkg:

  A GeoLocator Data Package object to be validated.

- quiet:

  A logical indicating whether to suppress messages from the `cli`
  package. Defaults to `FALSE`.

## Value

A logical value indicating whether the package validation was successful
(`TRUE`) or failed (`FALSE`).
