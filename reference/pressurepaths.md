# Get or set pressurepaths

`pressurepaths()` gets the pressurepaths from a GeoLocator Data Package
object.

`pressurepaths<-()` is the assignment equivalent. It should only be used
within other functions, where the expected data structure can be
guaranteed.

## Usage

``` r
pressurepaths(x)

pressurepaths(x) <- value
```

## Arguments

- x:

  A GeoLocator Data Package object, as returned by
  [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md).

- value:

  A data frame to assign as pressurepaths. Must conform to the
  pressurepaths schema specification.

## Value

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with pressurepaths
