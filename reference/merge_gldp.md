# Merge Two GeoLocator Data Packages

Merges two GeoLocator Data Package objects (`x` and `y`) into a single
combined package. The metadata properties from both packages are merged
according to specific rules, and resource data is combined based on
their presence in either package.

## Usage

``` r
merge_gldp(x, y)
```

## Arguments

- x:

  A GeoLocator Data Package object.

- y:

  A GeoLocator Data Package object.

## Value

A GeoLocator Data Package object containing the merged data from both
`x` and `y`.

## Details

**Metadata merging rules:**

- **title**: Combined from both packages, separated by a "/".

- **contributors**: Combined from both packages, with duplicates
  removed.

- **embargo**: Set to the latest date from both packages.

- **licenses**: Combined from both packages, with duplicates removed.

- **id**: Replaced with a new UUID for the merged package.

- **source_ids**: Added (custom property) storing the original package
  IDs.

- **description**: Combined as two separate paragraphs, with a newline
  separator.

- **version**: Use the latest version (same as
  [`create_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/create_gldp.md))

- **relatedIdentifiers**: Combined, with duplicates removed.

- **grants**: Combined from both packages, with duplicates removed.

- **keywords**: Combined from both packages, with duplicates removed.

- **created**: Set to the current timestamp at the time of merging.

- **bibliographicCitation**: Removed from the merged package.

- Custom properties from `x` are retained in the merged package.

Merging requires the [`uuid`](https://cran.r-project.org/package=uuid)
package to generate a globally unique identifier for the merged package.

**Resource merging logic:**

- Each resource is checked for its presence in both `x` and `y`.

- Data from both sources is combined if the resource exists in either
  `x` or `y`.

- Resources are only included if they exist in at least one of the
  packages.
