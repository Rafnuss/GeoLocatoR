# Get or set paths

`paths()` gets the paths from a GeoLocator Data Package object.

`paths<-()` is the assignment equivalent. It should only be used within
other functions, where the expected data structure can be guaranteed.

## Usage

``` r
paths(x)

paths(x) <- value
```

## Arguments

- x:

  A GeoLocator Data Package object, as returned by
  [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md).

- value:

  A data frame to assign as paths. Must conform to the paths schema
  specification.

## Value

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with paths
