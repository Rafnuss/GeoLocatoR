# Package index

## Main operation

- [`create_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/create_gldp.md)
  : Create a GeoLocator Data Package
- [`add_gldp_resource()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/add_gldp_resource.md)
  : Add a Geolocator Data Resource
- [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.md)
  : Read a GeoLocator Data Package
- [`merge_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/merge_gldp.md)
  : Merge Two GeoLocator Data Packages
- [`update_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/update_gldp.md)
  [`update_gldp_temporal()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/update_gldp.md)
  [`update_gldp_spatial()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/update_gldp.md)
  [`update_gldp_taxonomic()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/update_gldp.md)
  [`update_gldp_number_tags()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/update_gldp.md)
  [`update_gldp_bibliographic_citation()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/update_gldp.md)
  [`update_gldp_reference_location()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/update_gldp.md)
  [`update_gldp_metadata()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/update_gldp.md)
  : Update computed metadata of a GeoLocator Data Package
- [`gldp_to_tag()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/gldp_to_tag.md)
  : Convert GeoLocator Data Package to GeoPressureR tag object(s)

## Accessor (and assignment) functions

- [`tags()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/tags.md)
  [`` `tags<-`() ``](https://raphaelnussbaumer.com/GeoLocatoR/reference/tags.md)
  : Get or set tags
- [`observations()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/observations.md)
  [`` `observations<-`() ``](https://raphaelnussbaumer.com/GeoLocatoR/reference/observations.md)
  : Get or set observations
- [`measurements()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/measurements.md)
  [`` `measurements<-`() ``](https://raphaelnussbaumer.com/GeoLocatoR/reference/measurements.md)
  : Get or set measurements
- [`staps()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/staps.md)
  [`` `staps<-`() ``](https://raphaelnussbaumer.com/GeoLocatoR/reference/staps.md)
  : Get or set staps
- [`twilights()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/twilights.md)
  [`` `twilights<-`() ``](https://raphaelnussbaumer.com/GeoLocatoR/reference/twilights.md)
  : Get or set twilights
- [`paths()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/paths.md)
  [`` `paths<-`() ``](https://raphaelnussbaumer.com/GeoLocatoR/reference/paths.md)
  : Get or set paths
- [`edges()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/edges.md)
  [`` `edges<-`() ``](https://raphaelnussbaumer.com/GeoLocatoR/reference/edges.md)
  : Get or set edges
- [`pressurepaths()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/pressurepaths.md)
  [`` `pressurepaths<-`() ``](https://raphaelnussbaumer.com/GeoLocatoR/reference/pressurepaths.md)
  : Get or set pressurepaths

## GeoPressureTemplate

- [`create_gldp_geopressuretemplate()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/create_gldp_geopressuretemplate.md)
  : Create a GeoLocator Data Package from a GeoPressure Template
- [`add_gldp_geopressuretemplate()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/add_gldp_geopressuretemplate.md)
  : Add Geolocator DP resources from a GeoPressureTemplate
- [`create_geopressuretemplate()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/create_geopressuretemplate.md)
  : Create a GeoPressureTemplate Project
- [`config2tibble()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/config2tibble.md)
  : Convert GeoPressureTemplate config.yml to tibble

## SOI

- [`add_gldp_soi()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/add_gldp_soi.md)
  : Add GLDP SOI Data to a Package
- [`read_gdl()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gdl.md)
  [`read_gdl_orders()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gdl.md)
  [`read_gdl_data()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gdl.md)
  [`read_gdl_access()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gdl.md)
  : Read Geolocator Data and Orders

## Zenodo

- [`gldp_to_zenodo()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/gldp_to_zenodo.md)
  : Create or update a Zenodo Record based on the metadata from a
  GeoLocatoR Data Package
- [`zenodo_to_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/zenodo_to_gldp.md)
  : Create or update the metadata of a GeoLocator Data Package based on
  a Zenodo Record

## Export

- [`gldp_to_eml()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/gldp_to_eml.md)
  : Transform a GeoLocator Data Package to EML
- [`gldp_to_dwc()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/gldp_to_dwc.md)
  : Transform a GeoLocator Data Package to Darwin Core

## Miscellaneous functions

- [`print(`*`<geolocatordp>`*`)`](https://raphaelnussbaumer.com/GeoLocatoR/reference/print.geolocatordp.md)
  : Print a GeoLocator Data Package

- [`plot(`*`<geolocatordp>`*`)`](https://raphaelnussbaumer.com/GeoLocatoR/reference/plot.geolocatordp.md)
  : Plot GeoLocator Data Package coverage

- [`version()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/version.md)
  : Get GeoLocator DP version

- [`validate_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/validate_gldp.md)
  : Validate a GeoLocator Data Package

- [`validate_gldp_py()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/validate_gldp_py.md)
  : Validate a GeoLocator Data Package

- [`tags_to_measurements()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/tags_to_measurements.md)
  : Transform Tags to a Tidy Data Frame

- [`params_to_tags()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/params_to_tags.md)
  :

  Generate `tags` from GeoPressureR parameters

- [`params_to_observations()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/params_to_observations.md)
  :

  Generate `observations` from GeoPressureR parameters
