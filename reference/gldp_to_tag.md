# Convert GeoLocator Data Package to GeoPressureR tag object(s)

This function converts a GeoLocator Data Package (gldp) object to one or
more GeoPressureR `tag` objects. The gldp structure contains data for
potentially multiple tags, while each GeoPressureR `tag` object
represents a single tag's sensor data.

## Usage

``` r
gldp_to_tag(pkg, tag_id = NULL)
```

## Arguments

- pkg:

  A GeoLocator Data Package object (class `"geolocatordp"`), typically
  created with
  [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md)
  or
  [`create_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/create_gldp.md).

- tag_id:

  Character vector specifying which tag(s) to extract. If `NULL`
  (default), all tags in the package are converted.

## Value

If `tag_id` is a single value or if the package contains only one tag,
returns a single GeoPressureR `tag` object. Otherwise, returns a named
list of `tag` objects, where names correspond to the `tag_id` values.

A GeoPressureR `tag` object contains:

- `param`: parameter object (see
  [`GeoPressureR::param_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/param_create.html))

- `pressure`: data.frame with columns `date` and `value` (if pressure
  data available)

- `light`: data.frame with columns `date` and `value` (if light data
  available)

- `acceleration`: data.frame with columns `date`, `value` (activity
  measure), and optionally `pitch` (if activity/pitch data available)

- `temperature_external`: data.frame with columns `date` and `value` (if
  external temperature data available)

- `temperature_internal`: data.frame with columns `date` and `value` (if
  internal temperature data available)

- `magnetic`: data.frame with columns `date`, `magnetic_x`,
  `magnetic_y`, `magnetic_z` (if magnetic data available)

- `stap`: data.frame with columns `stap_id`, `start`, `end`, and
  optionally `known_lat`, `known_lon`, `include` (if stationary periods
  defined)

- `twilight`: data.frame with columns `twilight`, `rise`, and optionally
  `label` (if twilight data available)

## Details

The function extracts sensor data from the gldp's `measurements`
resource and converts it to the format expected by GeoPressureR. The
conversion includes:

- Filtering measurements by `tag_id`

- Reshaping data from long format (measurements) to wide format
  (sensor-specific data.frames)

- Combining related sensors (e.g., `activity` and `pitch` into
  `acceleration`; `magnetic_x/y/z` into `magnetic`)

- Adding stationary period (`stap`) data if available in the package

- Adding twilight data if available in the package

- Setting appropriate parameter values from the gldp metadata

**Sensor mapping from GLDP to GeoPressureR:**

- `pressure` → `pressure` (date, value)

- `light` → `light` (date, value)

- `activity` + `pitch` → `acceleration` (date, value, pitch) where value
  = activity

- `temperature-external` → `temperature_external` (date, value)

- `temperature-internal` → `temperature_internal` (date, value)

- `magnetic_x/y/z` → `magnetic` (date, magnetic_x, magnetic_y,
  magnetic_z)

**Additional data extracted:**

- `staps` table → `tag$stap` (stap_id, start, end, known_lat, known_lon,
  include)

- `twilights` table → `tag$twilight` (twilight, rise, label)

Note that the resulting `tag` object will have
`manufacturer = "datapackage"` in its parameters, consistent with how
GeoPressureR handles data from GeoLocator Data Packages.

## See also

- [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md)
  for reading a GeoLocator Data Package

- [`GeoPressureR::tag_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_create.html)
  for creating tag objects from raw files

- [GeoPressureR
  documentation](https://raphaelnussbaumer.com/GeoPressureR/)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read a gldp from a file
pkg <- read_gldp("path/to/datapackage.json")

# Convert all tags
tags <- gldp_to_tag(pkg)

# Convert a specific tag
tag <- gldp_to_tag(pkg, tag_id = "18LX")

# Convert multiple specific tags
tags <- gldp_to_tag(pkg, tag_id = c("18LX", "18IC"))
} # }
```
