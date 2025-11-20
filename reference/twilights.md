# Get or set twilights

`twilights()` gets the twilights from a GeoLocator Data Package object.

`twilights<-()` is the assignment equivalent. It should only be used
within other functions, where the expected data structure can be
guaranteed.

## Usage

``` r
twilights(x)

twilights(x) <- value
```

## Arguments

- x:

  A GeoLocator Data Package object, as returned by
  [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md).

- value:

  A data frame to assign as twilights. Must conform to the twilights
  schema specification.

## Value

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with twilights
