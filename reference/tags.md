# Get or set tags

`tags()` gets the tags from a GeoLocator Data Package object.

`tags<-()` is the assignment equivalent. It should only be used within
other functions, where the expected data structure can be guaranteed.

## Usage

``` r
tags(x)

tags(x) <- value
```

## Arguments

- x:

  A GeoLocator Data Package object, as returned by
  [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md).

- value:

  A data frame to assign as tags. Must conform to the tags schema
  specification.

## Value

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with tags
