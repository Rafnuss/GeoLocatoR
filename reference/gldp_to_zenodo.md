# Create or update a Zenodo Record based on the metadata from a GeoLocatoR Data Package

This function update a `zen4R` ZenodoRecord based on the GeoLocator Data
Package provided. If no `zenodo_record` is provided, the function will
create a new one.

Note that (1) the record is not deposited online (`zenodo_record` is a
variable in your computer) and (2) no data is actually retrieved/copied.

## Usage

``` r
gldp_to_zenodo(
  pkg,
  zenodo_record = zen4R::ZenodoRecord$new(),
  token = keyring::key_get(service = "ZENODO_PAT")
)
```

## Arguments

- pkg:

  A Geolocator Data Package object

- zenodo_record:

  A `zen4R` ZenodoRecord object from which to extract metadata

- token:

  A Zenodo API token. Best practice is to store it in your keyring (see
  description above), in which case the default value will retrieve it
  using
  [`keyring::key_get()`](https://keyring.r-lib.org/reference/key_get.html).

## Value

A ZenodoRecord object updated with the metadata from `pkg`.
