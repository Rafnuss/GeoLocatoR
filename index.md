# GeoLocatoR 🕊️📦

**GeoLocatoR** is an R package that helps you create, use, and share
your geolocator data in an easy way.

## 🚀 Overview

GeoLocatoR is designed to help you create and work with the [GeoLocator
Data Package (GeoLocator
DP)](https://raphaelnussbaumer.com/GeoLocator-DP/), a standardized
format for geolocator data that follows the [Data Package
standard](https://datapackage.org/standard/data-package/), ensuring your
data is organized, accessible, and ready to share.

## 🦅 Main Features

- **Creates a GeoLocator Data Package**: Create a geolocator datapackage
  object, add resources (data), and write the data package to your
  preferred directory. Or, read an existing data package from a
  directory or from Zenodo.
- **Works Seamlessly with GeoPressureTemplate**: GeoLocatoR is
  compatible with
  [GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate)
  — a template repository for analyzing geolocator data using
  GeoPressureR. Use both together to streamline your analysis and
  maintain a standardized project structure.
- **Supports Swiss Ornithological Institute Data**: Special functions
  have been designed to deal with internal SOI data. [See this
  vignette](https://rpubs.com/rafnuss/geolocator_create_from_soi).
- **Built on frictionless-r**: This package is built using the
  [`frictionless-r`](https://docs.ropensci.org/frictionless/) package,
  adhering to the Frictionless Data standards for open data.

## 📦 What is a Data Package?

A **Data Package** is a lightweight format for packaging data using the
[Data Package standard](https://datapackage.org/standard/data-package/).
It provides a framework for organizing your data files, metadata, and
descriptive information in a consistent and reusable manner. For
geolocator data, this ensures that information about tagged birds, their
movements, and associated metadata are all bundled into a single
exchangeable package. Learn more about the GeoLocator DP standard
[here](https://raphaelnussbaumer.com/GeoLocator-DP/).

## 🛠️ Installation

You can install GeoLocatoR from GitHub:

``` r
# install.packages("pak")
pak::pkg_install("Rafnuss/GeoLocatoR")
```

## 📖 Usage

Learn how to use GeoLocatoR with the dedicated part of the
[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geolocator-intro.html).

## Functions mapping

| [frictionless-r](https://docs.ropensci.org/frictionless/)                                  | Basic GeolocatoR                                                                                   | GeoPressureTemplate                                                                                                            |
|--------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------|
| [`create_package()`](https://docs.ropensci.org/frictionless/reference/create_package.html) | [`create_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/create_gldp.html)             | [`create_gldp_geopressuretemplate()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/create_gldp_geopressuretemplate.html) |
| [`add_resource()`](https://docs.ropensci.org/frictionless/reference/add_resource.html)     | [`add_gldp_resource()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/add_gldp_resource.html) | [`add_gldp_geopressuretemplate()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/add_gldp_geopressuretemplate.html)       |
| [`write_package()`](https://docs.ropensci.org/frictionless/reference/write_package.html)   |                                                                                                    | [`write_geopressuretemplate()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/write_geopressuretemplate.html)             |
| [`read_package()`](https://docs.ropensci.org/frictionless/reference/read_package.html)     | [`read_gldp()`](https://raphaelnussbaumer.com/GeoLocatoR/reference/read_gldp.html)                 |                                                                                                                                |

## 📚 Citation

If you use GeoLocatoR in your research, please cite it as follows:

> Nussbaumer, R. (2024). GeoLocatoR: R package for GeoLocator Data
> Package. <https://github.com/rafnuss/geolocator>
