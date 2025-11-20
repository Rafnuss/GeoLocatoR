# Update computed metadata of a GeoLocator Data Package

This function update/create the following metadata of an existing
GeoLocator Data Package based on the data contained within the package.

- `update_gldp_temporal()`: Sets `pkg$temporal` with the date range of
  the `datetime` values from the `measurements` resource, if available.

- `update_gldp_spatial()`: Sets `pkg$spatial` to the GeoJSON bounding
  box of all locations found in observation, paths and pressurepaths.

- `update_gldp_taxonomic()`: Sets `pkg$taxonomic` to the unique
  `tags$scientific_name` found in the observations.

- `update_gldp_number_tags()`:

- `update_gldp_bibliographic_citation()`: Sets
  `pkg$bibliographicCitation` to the formatted
  [`utils::bibentry()`](https://rdrr.io/r/utils/bibentry.html) of the
  current package.

- `update_gldp_reference_location()`: Sets the `pkg$reference_location`
  field to the median latitude and longitude values from the
  `observations` resource, if available.

- `update_gldp_metadata`: Re-arrange the order of the properties
  according to the order of the schema

`update_gldp` performs all of the functions mentioned above.

## Usage

``` r
update_gldp(pkg)

update_gldp_temporal(pkg)

update_gldp_spatial(pkg)

update_gldp_taxonomic(pkg)

update_gldp_number_tags(pkg)

update_gldp_bibliographic_citation(pkg, ...)

update_gldp_reference_location(pkg)

update_gldp_metadata(pkg)
```

## Arguments

- pkg:

  A GeoLocator Data Package object

- ...:

  overwrite parameters for
  [`utils::bibentry()`](https://rdrr.io/r/utils/bibentry.html)

## Value

An updated GeoLocator Data Package object with modified metadata.
