# Get or set measurements

`measurements()` gets the measurements from a GeoLocator Data Package
object.

`measurements<-()` is the assignment equivalent. It should only be used
within other functions, where the expected data structure can be
guaranteed.

## Usage

``` r
measurements(x)

measurements(x) <- value
```

## Arguments

- x:

  A GeoLocator Data Package object, as returned by
  [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md).

- value:

  A data frame to assign as measurements. Must conform to the
  measurements schema specification.

## Value

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with measurements
