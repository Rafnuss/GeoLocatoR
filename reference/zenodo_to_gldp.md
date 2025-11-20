# Create or update the metadata of a GeoLocator Data Package based on a Zenodo Record

This function update the metadata of a [GeoLocator Data
Package](https://datapackage.org/standard/data-package/) based on an
`zen4R` ZenodoRecord provided. If no package is provided (`pkg = NULL`),
a new data package is created.

All metadata are retrieved with the Zenodo API through the R package
[zen4R](https://github.com/eblondel/zen4R).

Note that no data is actually retrieved/copied.

## Usage

``` r
zenodo_to_gldp(zenodo_record, pkg = NULL)
```

## Arguments

- zenodo_record:

  A `zen4R` ZenodoRecord object from which to extract metadata.

- pkg:

  (optionall) A Geolocator Data Package object on which the metadata
  should be added. If not provided, create a new one.

## Value

An updated or new Geolocator Data Package object with metadata from the
zenodo record.
