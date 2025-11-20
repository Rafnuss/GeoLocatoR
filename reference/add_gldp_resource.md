# Add a Geolocator Data Resource

This function adds a resource to a geolocator data package and ensures
that the data conforms to the schema defined for that resource. It is a
wrapper of
[[`frictionless::add_resource`](https://docs.ropensci.org/frictionless/reference/add_resource.html)](https://docs.ropensci.org/frictionless/reference/add_resource.html)
where it first validate against the schema and potentially modify the
data frame `data` before adding it to the package.

More specifically, the function adjusts the data frame according to the
schema's
[`fieldsMatch`](https://datapackage.org/standard/table-schema/#fieldsMatch)
property and also cast the type/class of the columns provided according
to. `cast_type`.

Note that this function is generally not recommended to be used as all
resources can be added or modified with their respective [accessors
functions](https://bit.ly/41HruRs).

## Usage

``` r
add_gldp_resource(
  package,
  resource_name,
  data,
  cast_type = FALSE,
  replace = FALSE,
  delim = ","
)
```

## Arguments

- package:

  A GeoLocator Data Package object to which the resource will be added.

- resource_name:

  A character string specifying the name of the resource. This name is
  used to locate the schema file.

- data:

  A data frame containing the data to be added as a resource. The data
  frame will be adjusted according to the schema.

- cast_type:

  A logical value indicating whether the data frame should be cast to
  the types specified in the schema. Defaults to `FALSE`.

- replace:

  If `TRUE`, the added resource will replace an existing resource with
  the same name.

- delim:

  Single character used to separate the fields in the CSV file(s), e.g.
  `\t` for tab delimited file. Will be set as `delimiter` in the
  resource Table Dialect, so read functions . know how to read the
  file(s).

## Value

The updated GeoLocator Data Package object with the new resource added.
