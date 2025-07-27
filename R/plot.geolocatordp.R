#' Plot GeoLocator Data Package coverage
#'
#' @description
#' Creates visualizations for a GeoLocator Data Package, including coverage plots
#' showing data availability across time for different sensors and tags.
#'
#' @param x A GeoLocator Data Package object.
#' @param type Type of the plot to display. Currently only `"coverage"` is supported.
#' @param ... Additional parameters passed to plotting functions.
#'
#' @return a plot, ggplotly or leaflet object.
#'
#' @export
plot.geolocatordp <- function(x, type = NULL, ...) {
  if (is.null(type)) {
    type <- "coverage"
  }

  if (type == "coverage") {
    plot_pkg_coverage(x)
  }
}

#' Plot coverage data for a GeoLocator Data Package
#'
#' Internal helper function to create a coverage plot showing data availability
#' across time for different sensors and tags.
#'
#' @param x A GeoLocator Data Package object
#' @return A ggplot2 object showing the coverage plot
#' @noRd
plot_pkg_coverage <- function(x) {
  m <- measurements(x) %>%
    mutate(sensor = case_when(
      sensor %in% c(
        "magnetic_x", "magnetic_y", "magnetic_z", "acceleration_x", "acceleration_y",
        "acceleration_z", "pitch"
      ) ~ "magnetic",
      TRUE ~ sensor # Keep other sensor names unchanged
    )) %>%
    mutate(date = as.Date(.data$datetime)) %>%
    group_by(.data$tag_id, .data$sensor, .data$date) %>% # Group by tag_id, sensor, and date
    summarize(has_data = sum(!is.na(.data$value)) > 0) %>% # Check if there is data (non-NA)
    ungroup()

  o <- observations(x) %>%
    filter(!is.na(.data$datetime)) %>%
    filter(.data$tag_id %in% unique(m$tag_id))

  ggplot2::ggplot(data = m, ggplot2::aes(x = .data$date, y = .data$sensor, fill = .data$has_data)) +
    ggplot2::geom_tile() +
    ggplot2::facet_grid(.data$tag_id ~ ., scales = "free_y") +
    ggplot2::scale_fill_manual(values = c("black", "grey")) +
    ggplot2::geom_vline(
      data = o %>% filter(.data$observation_type %in% c("equipment", "retrieval")),
      ggplot2::aes(
        xintercept = as.numeric(as.Date(.data$datetime)),
        color = .data$observation_type
      ),
      linetype = "solid", linewidth = 1.2
    ) +
    ggplot2::geom_vline(
      data = o %>% filter(.data$observation_type %in% c("capture", "sighting", "other")),
      ggplot2::aes(
        xintercept = as.numeric(as.Date(.data$datetime)),
        color = .data$observation_type
      ),
      linetype = "dashed", linewidth = 0.8
    ) + # Thinner dashed lines

    # Color scheme highlighting 'equipment' and 'retrieval'
    ggplot2::scale_color_manual(
      values = c(
        "equipment" = "red",
        "retrieval" = "green",
        "capture" = "blue",
        "sighting" = "orange",
        "other" = "purple"
      ),
      name = "Observation Type"
    ) + # Customize colors and legend
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), legend.position = "bottom") +
    ggplot2::labs(x = "Date", y = "Sensor", title = "Package's Measurement Coverage")
}
