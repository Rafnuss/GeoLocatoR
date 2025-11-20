# Plot GeoLocator Data Package coverage

Creates visualizations for a GeoLocator Data Package, including coverage
plots showing data availability across time for different sensors and
tags.

## Usage

``` r
# S3 method for class 'geolocatordp'
plot(x, type = NULL, ...)
```

## Arguments

- x:

  A GeoLocator Data Package object.

- type:

  Type of the plot to display. Currently only `"coverage"` is supported.

- ...:

  Additional parameters passed to plotting functions.

## Value

a plot, ggplotly or leaflet object.
