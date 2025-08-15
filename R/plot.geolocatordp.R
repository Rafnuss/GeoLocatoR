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
    if ("measurements" %in% frictionless::resources(x)) {
      type <- "coverage_measurements"
    } else if ("observations" %in% frictionless::resources(x)) {
      type <- "coverage_observations"
    } else {
      cli_abort("No coverage data available in the GeoLocator Data Package.")
    }
  }

  if (type == "coverage_measurements") {
    plot_pkg_coverage_measurements(x)
  } else if (type == "coverage_observations") {
    plot_pkg_coverage_observations(x)
  } else if (type == "path_most_likely") {
    plot_pkg_path_most_likely(x)
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
plot_pkg_coverage_measurements <- function(x) {
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

  ggplot2::ggplot(
    data = m,
    ggplot2::aes(x = .data$date, y = .data$sensor, fill = .data$has_data)
  ) +
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


#' Plot coverage data for a GeoLocator Data Package
#'
#' Internal helper function to create a coverage plot showing observations across time for all tags.
#'
#' @param x A GeoLocator Data Package object
#' @return A ggplot2 object showing the coverage plot
#' @noRd
plot_pkg_coverage_observations <- function(x) {
  o <- observations(x) %>%
    filter(!is.na(.data$datetime))

  m <- measurements(x) %>%
    group_by(.data$tag_id) %>%
    summarize(
      start = min(.data$datetime, na.rm = TRUE),
      end = max(.data$datetime, na.rm = TRUE)
    ) %>%
    ungroup()


  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = m, ggplot2::aes(x = .data$start, y = .data$tag_id, xend = .data$end, color = "black")
    ) +
    ggplot2::geom_point(
      data = o, ggplot2::aes(x = .data$datetime, y = .data$tag_id, color = .data$observation_type)
    ) +
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
    ggplot2::labs(x = "Date", y = "Tag ID", title = "Package's Observations Coverage")
}

#' Plot coverage data for a GeoLocator Data Package
#'
#' Internal helper function to create a coverage plot showing observations across time for all tags.
#'
#' @param x A GeoLocator Data Package object
#' @return A ggplot2 object showing the coverage plot
#' @noRd
plot_pkg_path_most_likely <- function(x) {


  p <- paths(x) %>%
    filter(type == "most_likely") %>%
    left_join(staps(x), by=c("stap_id", "tag_id")) %>%
    mutate(duration = stap2duration(.))

    # Generate a color palette for unique tag IDs
    tag_ids <- unique(p$tag_id)
    n_tags <- length(tag_ids)
    palette_fun <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
    color_palette <- leaflet::colorFactor(palette = palette_fun(n_tags), domain = tag_ids)

    leaflet_map <- leaflet::leaflet(height = 600) %>% leaflet::addTiles()

    # Add polylines and markers for each tag_id
    for (tag in tag_ids) {
      pid <- p %>% filter(tag_id == tag)
      leaflet_map <- leaflet_map %>%
        leaflet::addPolylines(
          lng = ~lon,
          lat = ~lat,
          data = pid,
          color = color_palette(tag),
          weight = 2,
          popup = ~paste0("Tag ID: ", tag_id, "<br>stap #", stap_id)
        ) %>%
        leaflet::addCircleMarkers(
          lng = ~lon,
          lat = ~lat,
          data = pid,
          color = color_palette(tag),
          radius = ~scales::rescale(log1p(duration), to = c(3, 12)),
          fillOpacity = 0.8,
          popup = ~paste0("Tag ID: ", tag_id, "<br>stap #", stap_id)
        )
    }

    # Add a legend
    leaflet_map <- leaflet_map %>%
      leaflet::addLegend(
        position = "topright",
        pal = color_palette,
        values = tag_ids,
        title = "Bird Trajectories",
        opacity = 1
      )

    # Display the map
    leaflet_map

}

