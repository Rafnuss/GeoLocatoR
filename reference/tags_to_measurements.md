# Transform Tags to a Tidy Data Frame

This function processes a list of tags, each containing sensor data, and
transforms it into a tidy data frame. The tags are expected to have
various sensor types and their corresponding measurements. The function
reshapes and standardizes the data for further analysis.

## Usage

``` r
tags_to_measurements(tags)
```

## Arguments

- tags:

  A list of tags, where each tag is a data frame containing sensor data.
  Each tag should include columns for sensor measurements and an
  identifier.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with columns:

- tag_id:

  A character vector representing the unique identifier for each tag.

- sensor:

  A character vector representing the type of sensor measurement,
  including types like "activity", "pitch", "light",
  "temperature_external", etc.

- datetime:

  A POSIXct datetime object representing the timestamp of the
  measurements.

- value:

  A numeric vector containing the sensor measurement values.

- label:

  A character vector for additional labeling, which is NA if not present
  in the original data.

## Details

The `tags_to_measurements()` function extracts and processes sensor data
from a list of tags. It renames the `value` column to the corresponding
sensor type if it exists, ensures the presence of a `label` column, and
reshapes the data into a long format. The function handles various
sensor types such as pressure, acceleration, light, temperature, and
magnetic fields.

If no tags are provided, the function returns an empty tibble with the
appropriate column names and types.
