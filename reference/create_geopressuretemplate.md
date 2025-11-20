# Create a GeoPressureTemplate Project

Initializes a geopressure template project by creating a specified
directory structure and populating it with essential files, including a
DESCRIPTION file, README, license, and data.

This function sets up the project directory and ensures that all
necessary components are in place for a geopressure analysis project.

## Usage

``` r
create_geopressuretemplate(path, pkg = NULL, open = interactive())
```

## Arguments

- path:

  A character string specifying the destination directory where the
  project will be created. The last folder will give the name to the
  project.

- pkg:

  A GeoLocatoR Datapackage object (optional)

- open:

  If `TRUE`, the package is opened in a new RStudio session.

## Value

The path to the created project directory.

## Details

Upon execution, the function performs the following steps:

- Creates the project directory.

- Generates the DESCRIPTION file using metadata from `pkg`.

- Creates a README file that outlines project details.

- Generates a LICENSE file based on the specified licenses.

- Writes relevant data files into the project structure.
