# Transform a GeoLocator Data Package to Darwin Core

Transforms a [GeoLocator Data
Package](https://raphaelnussbaumer.com/GeoLocator-DP/) to a [Darwin Core
Archive](https://dwc.tdwg.org/). The resulting CSV files can be uploaded
to an [IPT](https://www.gbif.org/ipt) for publication to GBIF. A
`meta.xml` file is included as well. See
[`gldp_to_eml()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/gldp_to_eml.md)
to create an `eml.xml` file.

## Usage

``` r
gldp_to_dwc(package, directory, path_type = "most_likely")
```

## Arguments

- package:

  A GeoLocator Data Package object.

- directory:

  Path to local directory to write file(s) to. If `NULL`, then a data
  frame is returned instead, which can be useful for extending/adapting
  the Darwin Core mapping before writing with
  [`readr::write_csv()`](https://readr.tidyverse.org/reference/write_delim.html).

- path_type:

  The type of path to use for the occurrence data. One of
  `"most_likely"`, `"mean_simulation"`, `"median_simulation"`, or
  `"geopressureviz"`. For simulation types, the mean or median position
  (lat/lon) is calculated for each `tag_id`-`stap_id` combination across
  all simulated paths. Defaults to `"most_likely"`. See
  [paths](https://raphaelnussbaumer.com/GeoLocatoR/reference/paths.md)
  for more details.

## Value

`occurrence.csv` file written to disk. Invisibly, an occurrence data
frame.

## Transformation details

Data are transformed into an [Occurrence
core](https://rs.gbif.org/core/dwc_occurrence). This transformation
combines data from three resources:

- [`tags`](https://raphaelnussbaumer.com/GeoLocator-DP/core/tags/):
  metadata about the device and deployment

- [`staps`](https://raphaelnussbaumer.com/GeoLocator-DP/geopressurer/staps/):
  stationary periods with temporal information

- [`paths`](https://raphaelnussbaumer.com/GeoLocator-DP/geopressurer/paths/):
  spatial positions estimated for each stationary period

The following terms are set from the package metadata:

- `datasetName`: Title as provided in `package$title`.

- `datasetID`: Identifier as provided in `package$id`.

- `rightsHolder`: Rights holder as provided in `package$contributors`
  (contributor with `"rightsHolder"` role). If no rightsHolder role is
  found, this field will be `NA`.

- `license`: License name as provided in `package$licenses`.

Key features of the Darwin Core transformation:

- Stationary periods (`staps`) are treated as events, with each position
  as an occurrence representing the bird's location during that period.

- Each occurrence represents one stationary period from the path data,
  filtered by `path_type`.

- The `eventDate` is expressed as an ISO 8601 interval (`start/end`)
  representing the duration of the stationary period.

- `basisOfRecord` is set to `"MachineObservation"` as data are derived
  from automated geolocator sensors.

- `samplingProtocol` is set to `"geolocator"`.

- `geodeticDatum` is `"EPSG:4326"` (WGS84).

- `scientificName` is taken from `tags$scientific_name`.

- `organismID` is set to `ring_number` to track individual birds across
  multiple observations and deployments.

- `occurrenceID` is a unique identifier combining `tag_id` and
  `stap_id`.

- `individualCount` is set to `1`.

- `occurrenceStatus` is set to `"present"`.

- `sex` and `lifeStage` are included if available in the `tags`
  resource.

- `coordinateUncertaintyInMeters` is calculated for simulation path
  types (`"mean_simulation"` or `"median_simulation"`) as the 95th
  percentile of the distance between each simulation and the aggregated
  center.
