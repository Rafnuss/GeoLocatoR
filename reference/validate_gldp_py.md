# Validate a GeoLocator Data Package

This function validates a GeoLocator Data Package using the
`frictionless` command-line tool. It writes the package metadata to a
JSON file and performs validation to ensure that the package conforms to
the required standards. The function supports two modes: validating only
the package metadata or validating the entire package including its
resources.

## Usage

``` r
validate_gldp_py(
  pkg,
  path = "/Users/rafnuss/anaconda3/bin/",
  only_package = NULL,
  pkg_dir = tempdir()
)
```

## Arguments

- pkg:

  An object of class `"geolocatordp"` representing the GeoLocator Data
  Package to be validated.

- path:

  A string specifying the path to the directory containing the
  `frictionless` executable. Defaults to
  `"/Users/rafnuss/anaconda3/bin/"`.

- only_package:

  A logical indicating whether to validate only the package metadata
  (TRUE) or the entire package including resources (FALSE). Defaults to
  `NULL`, in which case it is determined based on the presence of
  resources in the package.

- pkg_dir:

  A string specifying the directory where the package files will be
  written for validation. Defaults to a temporary directory created with
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

NULL. The function performs validation as a side effect and does not
return a value.

## Details

The function performs the following steps:

1.  If `only_package` is `TRUE` or the package contains no resources, it
    writes only the metadata to a JSON file and validates it.

2.  If `only_package` is `FALSE` and resources are present, it writes
    the entire package, including resources, to a directory and
    validates it.

3.  Executes the `frictionless` validation command using the specified
    path to the `frictionless` executable.
