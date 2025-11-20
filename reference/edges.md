# Get or set edges

`edges()` gets the edges from a GeoLocator Data Package object.

`edges<-()` is the assignment equivalent. It should only be used within
other functions, where the expected data structure can be guaranteed.

## Usage

``` r
edges(x)

edges(x) <- value
```

## Arguments

- x:

  A GeoLocator Data Package object, as returned by
  [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md).

- value:

  A data frame to assign as edges. Must conform to the edges schema
  specification.

## Value

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with edges
