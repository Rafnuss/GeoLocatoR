# Read a GeoLocator Data Package

This function reads a GeoLocator Data Package from a
`"datapackage.json"` file that describes the Data Package metadata and
its Data Resources. The function wraps the function
[[`frictionless::read_package`](https://docs.ropensci.org/frictionless/reference/read_package.html)](https://docs.ropensci.org/frictionless/reference/read_package.html)
and assigns the class `"geolocatordp"` to the datapackage read.

## Usage

``` r
read_gldp(x = "datapackage.json", force_read = TRUE)
```

## Arguments

- x:

  A string specifying the path to the JSON file containing the
  GeoLocator Data Package metadata. Defaults to `"datapackage.json"`.
  Also accepts direct urls, Zenodo DOI, Zenodo link, or Zenodo record
  number.

- force_read:

  Logical indicating whether to force the reading of the data from
  path/URL to memory. Defaults to `TRUE`.

## Value

A GeoLocator Data Package object created from a file/url

## Examples

``` r
if (FALSE) { # \dontrun{
# Local file
read_gldp("datapackage.json")
# Zenodo DOI
read_gldp("10.5281/zenodo.15259676")
# Zenodo DOI URL
read_gldp("https://doi.org/10.5281/zenodo.15259676")
# Zenodo record link
read_gldp("https://zenodo.org/records/15259676")
# Zenodo record number
read_gldp("15259676")
} # }
# Read a datapackage.json file
pkg <- read_gldp("https://zenodo.org/records/14099115/files/datapackage.json")

pkg
#> 
#> ── A GeoLocator Data Package (vmain) 
#> • title: "GeoLocator Data Package: South African Woodland Kingfisher"
#> • contributors:
#>   Raphaël Nussbaumer (rafnuss@gmail.com) (Researcher, ProjectLeader) -
#>   <https://orcid.org/0000-0002-8185-1020>
#>   Yann Rime (yann.rime@vogelwarte.ch) (Researcher, Contributor) -
#>   <https://orcid.org/0009-0005-7264-6753>
#> • embargo: 1970-01-01
#> • licenses:
#>   CC BY 4.0
#> • id: <https://doi.org/10.5281/zenodo.13829929>
#> • version: "1.1"
#> • relatedIdentifiers:
#>   IsPartOf <10.5281/zenodo.11207081>
#>   IsSupplementTo <>
#> • grants: "Swiss Ornithological Intitute"
#> • keywords: "Woodland Kingfisher", "intra-african", and "multi-sensor
#>   geolocator"
#> • created: 2024-11-12 12:58:55
#> • spatial:
#> • temporal: "2017-01-10" to "2019-09-28"
#> • taxonomic: "Halcyon senegalensis"
#> • numberTags:
#> 
#> ── 8 resources 
#> • tags (n=24)
#> • observations (n=63)
#> • measurements (n=1,924,975)
#> • twilights (n=3,262)
#> • staps (n=223)
#> • paths (n=22,523)
#> • edges (n=22,018)
#> • pressurepaths (n=224,828)
#> Use `unclass()` to print the Geolocator Data Package as a list.
```
