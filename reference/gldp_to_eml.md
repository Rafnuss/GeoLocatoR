# Transform a GeoLocator Data Package to EML

Transforms a [GeoLocator Data
Package](https://raphaelnussbaumer.com/GeoLocator-DP/) to [Ecological
Metadata Language (EML)](https://eml.ecoinformatics.org/). The resulting
EML file can be uploaded to an [IPT](https://www.gbif.org/ipt) for
publication to GBIF and/or OBIS. A corresponding Darwin Core Archive can
be created with
[`gldp_to_dwc()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/gldp_to_dwc.md).

## Usage

``` r
gldp_to_eml(package, directory)
```

## Arguments

- package:

  A GeoLocator Data Package object.

- directory:

  Path to local directory to write files to.

## Value

`eml.xml` file written to disk. Invisibly, an
[EML::eml](https://docs.ropensci.org/EML/reference/eml.html) object.

## Transformation details

Metadata are derived from the GeoLocator Data Package and transformed to
EML. The following properties are set:

- `title`: Package title as provided in `package$title`.

- `abstract`: Package description as provided in `package$description`.

- `pubDate`: Publication year extracted from `package$created`.

- `creator`: Contributors with roles `"ProjectLeader"`, `"Researcher"`,
  or `"DataCurator"` as provided in `package$contributors`.

- `contact`: Contributors with role `"ContactPerson"` as provided in
  `package$contributors`.

- `associatedParty`: Other contributors as provided in
  `package$contributors`, including those with role `"RightsHolder"`.

- `intellectualRights`: License information from `package$licenses`.

- `keywords`: Keywords as provided in `package$keywords`.

- `packageId`: Package identifier as provided in `package$id`. If no ID
  is provided, a UUID is generated. As a result, no new DOI will be
  created when publishing to GBIF if `package$id` contains a DOI.

Coverage information:

- `temporalCoverage`: Date range from `package$temporal` (if available),
  derived from measurement timestamps.

- `geographicCoverage`: Bounding box from `package$spatial` (if
  available), calculated from all locations in observations, paths, and
  pressurepaths.

- `taxonomicCoverage`: Species list from `package$taxonomic` (if
  available), derived from unique scientific names in tags.

Methods:

- `methodStep`: Generic description indicating data were processed using
  the GeoLocator Data Package standard.

The following EML properties are not set:

- `type`

- `subtype`

- `update frequency`

- `publishing organization`

- `project data`

- `citations`

- `collection data`
