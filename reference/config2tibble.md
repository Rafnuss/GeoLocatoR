# Convert GeoPressureTemplate config.yml to tibble

Reads a GeoPressureTemplate `config.yml` file and converts it into a
tibble format with one row per tag and columns for each configuration
parameter. This is useful for comparing configurations across multiple
tags or for programmatic analysis of tag settings.

## Usage

``` r
config2tibble(
  file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
  filter_return = TRUE
)
```

## Arguments

- file:

  Character string specifying the path to the `config.yml` file.
  Defaults to `"config.yml"` in the current directory, or the value of
  the `R_CONFIG_FILE` environment variable if set.

- filter_return:

  Logical. If `TRUE` (default), only columns that vary across tags are
  returned. If `FALSE`, all configuration columns are included even if
  they have the same value for all tags.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with one row per tag ID and columns for each configuration parameter.
The exact columns returned depend on the configuration file structure
and the `filter_return` parameter.

## Details

This function uses
[`GeoPressureR::geopressuretemplate_config()`](https://raphaelnussbaumer.com/GeoPressureR/reference/geopressuretemplate.html)
internally to parse each tag's configuration. It then flattens the
nested list structure and applies appropriate type conversions for each
parameter (e.g., converting dates to strings, functions to their
deparsed representation).

The function handles various GeoPressureR configuration parameters
including:

- Tag creation settings (manufacturer, file paths, crop dates)

- Labeling parameters (flight/stap duration warnings)

- Map settings (extent, known locations, pressure/light thresholds)

- Graph creation and movement model parameters

- Bird characteristics (mass, wing measurements)

- Wind and environmental data settings

## See also

- [`GeoPressureR::geopressuretemplate_config()`](https://raphaelnussbaumer.com/GeoPressureR/reference/geopressuretemplate.html)
  for the underlying config parser

- [GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate)
  for the template structure

## Examples

``` r
if (FALSE) { # \dontrun{
# Read config from default location
cfg <- config2tibble()

# Read from specific file
cfg <- config2tibble("path/to/config.yml")

# Include all columns even if they don't vary
cfg_full <- config2tibble(filter_return = FALSE)

# View which parameters differ across tags
View(cfg)
} # }
```
