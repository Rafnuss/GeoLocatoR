# Add Geolocator DP resources from a GeoPressureTemplate

This function adds all possible resources to a GeolocatoR Data Package
by reading data from a
[GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate)
directories and files.

The function performs the following steps:

1.  If `"interim"` in `from`, reads all interim ".RData" files, extract
    all variables and add corresponding resources to the package
    (`measurements`, `twilights`, `staps`, `paths`, `edges`, and
    `pressurepaths`). Generated automatically temporary `tags` and
    `observations` tables from the `param` in the interim file.

2.  If `"raw-data"` in `from`, search for all tag_id in `data/raw-data/`
    which were not included from `interim`. It will use at least
    `tag_create` and if possible `tag_label` and `tag_set_map` using
    `config.yml` .It will also generate or update `tags` and
    `observations` table from the the tag data.

3.  Reads the `tags.csv` (or `tags.xlsx` if present) and
    `observations.csv` (or `observations.xlsx` if present) from the
    `./data` directory if they exist and overwrite the previously
    generated `tags` and `observations`.

You can exclude interim file or raw-tag folder to be included in the
package by starting the file name with an `"_"`.

It is possible to do a mix of some `tag` read from `"interim"` and other
from `"raw-data"` simultaneously.

You can find more information on the use of this function in the
[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geolocator-create.html)

## Usage

``` r
add_gldp_geopressuretemplate(
  pkg,
  directory = ".",
  from = c("raw-tag", "interim")
)
```

## Arguments

- pkg:

  A GeoLocator Data Package object.

- directory:

  A character string specifying the geopressuretemplate directory.

- from:

  A character vector specifying the source of the data files. Either or
  both of `"raw-tag"` (for creating `tag` based on the data in
  `data/raw-tag/`) and `"interim"` for data in `data/interim`.

## Value

The updated GLDP package object with new resources
