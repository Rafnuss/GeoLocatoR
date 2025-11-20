# Generate `tags` from GeoPressureR parameters

This function converts a list of parameters into a single `tags` tibble
according to the [GeoLocator Data Package
specification](https://raphaelnussbaumer.com/GeoLocator-DP/core/tags/).

## Usage

``` r
params_to_tags(params)
```

## Arguments

- params:

  A list of GeoPressureR parameter objects. These parameters should have
  been generated during the GeoPressure workflow. See
  [[`GeoPressureR::param_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/param_create.html)](https://raphaelnussbaumer.com/GeoPressureR/reference/param_create.html)
  for more information.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with columns `tag_id`, `manufacturer`, `scientific_name`,
`ring_number`, `model`, and `firmware` (if `soi_settings` is present).
