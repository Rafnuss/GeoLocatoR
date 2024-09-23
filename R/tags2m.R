#' Transform Tags to a Tidy Data Frame
#'
#' This function processes a list of tags, each containing sensor data, and transforms it into a
#' tidy data frame. The tags are expected to have various sensor types and their corresponding
#' measurements. The function reshapes and standardizes the data for further analysis.
#'
#' @param tags A list of tags, where each tag is a data frame containing sensor data. Each tag
#' should include columns for sensor measurements and an identifier.
#'
#' @return A tibble (data frame) with columns:
#' \describe{
#'   \item{tag_id}{A character vector representing the unique identifier for each tag.}
#'   \item{sensor}{A character vector representing the type of sensor measurement, including types
#'   like "activity", "pitch", "light", "temperature", etc.}
#'   \item{datetime}{A POSIXct datetime object representing the timestamp of the measurements.}
#'   \item{value}{A numeric vector containing the sensor measurement values.}
#'   \item{label}{A character vector for additional labeling, which is NA if not present in the
#'   original data.}
#' }
#'
#' @details
#' The `tags2m()` function extracts and processes sensor data from a list of tags. It renames the
#' `value` column to the corresponding sensor type if it exists, ensures the presence of a `label`
#' column, and reshapes the data into a long format. The function handles various sensor types such
#'  as pressure, acceleration, light, temperature, and magnetic fields.
#'
#' If no tags are provided, the function returns an empty tibble with the appropriate column names
#'  and types.
#'
#' @export
tags2m <- function(tags) {

  if (length(tags) > 0) {
    m <- tags %>%
      purrr::map(function(tag) {
        tag %>%
          purrr::keep(names(tag) %in% c("pressure", "acceleration", "light", "temperature",
                                        "airtemperature", "magnetic")) %>%
          purrr::imap(\(df, sensor) {
            if (sensor == "acceleration") {
              sensor <- "activity"
            }
            if ("value" %in% colnames(df)) {
              df <- df %>% rename(!!sym(sensor) := .data$value)
            }
            # Add 'label' column if it doesn't exist
            if (!"label" %in% colnames(df)) {
              df <- df %>% mutate(label = NA_character_)
            }
            return(df)
          }) %>%
          purrr::map(~ select(.x, any_of(
            c("pressure", "activity", "pitch", "light", "temperature", "airtemperature",  "gX",
              "gY", "gZ", "mX", "mY", "mZ", "date", "label")))) %>%
          purrr::map(~ pivot_longer(.x,
            cols = -c(date, label), names_to = "sensor",
            values_to = "value"
          )) %>%
          bind_rows() %>%
          mutate(tag_id = tag$param$id)
      }) %>%
      purrr::list_rbind() %>%
      mutate() %>%
      rename(
        datetime = date
      )
  } else {
    m <- tibble::tibble(
      tag_id = character(),
      sensor = character(),
      datetime = as.POSIXct(character()),
      value = numeric(),
      label = character()
    )
  }
  return(m)
}
