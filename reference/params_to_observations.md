# Generate `observations` from GeoPressureR parameters

This function converts a list of parameters into a single `observations`
tibble according to the [GeoLocator Data Package
specification](https://raphaelnussbaumer.com/GeoLocator-DP/core/observations/)
.

## Usage

``` r
params_to_observations(params)
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
data frame with columns `ring_number`, `tag_id`, `observation_type`,
`datetime`, `longitude`, `latitude`, and `comments`.
