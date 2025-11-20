# Get or set staps

`staps()` gets the staps from a GeoLocator Data Package object.

`staps<-()` is the assignment equivalent. It should only be used within
other functions, where the expected data structure can be guaranteed.

## Usage

``` r
staps(x)

staps(x) <- value
```

## Arguments

- x:

  A GeoLocator Data Package object, as returned by
  [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md).

- value:

  A data frame to assign as staps. Must conform to the staps schema
  specification.

## Value

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with staps
