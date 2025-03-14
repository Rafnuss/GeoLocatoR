#' Create a GeoPressureTemplate Project
#' @description
#' Initializes a geopressure template project by creating a specified directory structure and
#' populating it with essential files, including a DESCRIPTION file, README, license, and data.
#'
#' This function sets up the project directory and ensures that all necessary components are in
#' place for a geopressure analysis project.
#'
#' @param path A character string specifying the destination directory where the project will
#' be created. The last folder will give the name to the project.
#' @param pkg A GeoLocatoR Datapackage object (optional)
#' @param open If `TRUE`, the package is opened in a new RStudio session.
#'
#' @return The path to the created project directory.
#'
#' @details Upon execution, the function performs the following steps:
#' \itemize{
#'   \item Creates the project directory.
#'   \item Generates the DESCRIPTION file using metadata from `pkg`.
#'   \item Creates a README file that outlines project details.
#'   \item Generates a LICENSE file based on the specified licenses.
#'   \item Writes relevant data files into the project structure.
#' }
#'
#' @export
create_geopressuretemplate <- function(path,
                                       pkg = NULL,
                                       open = rlang::is_interactive()) {
  path <- gert::git_clone("https://github.com/Rafnuss/GeoPressureTemplate/",
    path = path, verbose = FALSE
  )
  cli::cli_bullets(c(
    "v" = "Cloning repo from {.url https://github.com/Rafnuss/GeoPressureTemplate/} \\
    into {.path {path}}."
  ))
  project_name <- basename(path)

  file.rename(
    from = file.path(path, "GeoPressureTemplate.Rproj"),
    to = file.path(path, glue::glue("{project_name}.Rproj"))
  )

  # setwd(path)
  withr::with_dir(path, {
    # setwd(path)
    d <- desc::desc()
    d$set("Package", gsub("[^a-zA-Z0-9\\.]", "", project_name))
    d$write()

    # delete existing data
    unlink(list.files("./data/raw-tag/", full.names = TRUE), recursive = TRUE)
    unlink(list.files("./data/tag-label/", full.names = TRUE), recursive = TRUE)
    unlink(list.files("./data/twilight-label/", full.names = TRUE), recursive = TRUE)

    # Remove example in config
    default_yaml <- readLines("config.yml")
    writeLines(default_yaml[1:(grep("18LX:", default_yaml)[1] - 1)], "config.yml")

    if (!is.null(pkg)) {
      check_gldp(pkg)

      # Update the description file
      try({
        create_geopressuretemplate_desc(pkg)
      })


      # Update the README file
      try({
        create_geopressuretemplate_readme(pkg)
      })

      # Update the LICENSE file
      try({
        create_geopressuretemplate_licences(pkg$licenses)
      })

      # Add data
      try({
        create_geopressuretemplate_data(pkg)
      })

      # Set config file
      try({
        create_geopressuretemplate_config(pkg)
      })
    }
  })

  if (open) {
    rstudioapi::openProject(path, newSession = TRUE)
  }

  return(path)
}

#' @noRd
create_geopressuretemplate_desc <- function(pkg) {
  d <- desc::description$new()

  d$set("Title", pkg$title, check = FALSE)
  d$set("License", paste(purrr::map_chr(pkg$license, ~ .x$name), collapse = ", "), check = FALSE)
  d$set_authors(contributors2persons(pkg$contributors))

  # Optional fields
  if ("description" %in% names(pkg)) {
    d$set("Description", rvest::html_text(rvest::read_html(pkg$description)), check = FALSE)
  }

  if ("version" %in% names(pkg)) {
    d$set_version(gsub("^v", "", pkg$version))
  }

  d$normalize()
  d$write()
}

#' @noRd
create_geopressuretemplate_readme <- function(pkg) {
  check_gldp(pkg)

  content <- paste(
    "# ", pkg$title, "\n\n",
    if (!is.null(pkg$description)) paste(pkg$description, "\n") else "",
    if (!is.null(pkg$keywords)) {
      paste("**Keywords:** ", paste(pkg$keywords, collapse = ", "), "\n")
    }, "\n",
    "## Contributors\n\n",
    paste(sapply(pkg$contributors, function(c) {
      paste0(
        "- **", c$title, "** - ", paste(c$roles, collapse = ", "),
        " ([Email](mailto:", c$email, "))\n"
      )
    }), collapse = ""),
    "\n",
    "## Overview\n\n",
    "**Version:** ", pkg$version, "\n",
    "**Created:** ", pkg$created, "\n",
    "**Temporal coverage:** ", pkg$temporal$start, " - ", pkg$temporal$end, "\n",
    "**Taxonomic coverage:** ", paste(pkg$taxonomic, collapse = ", "), "\n",
    "**Number of tags:** \n",
    paste0("- ", names(pkg$numberTags), ": ", unlist(pkg$numberTags), collapse = "\n"), "\n",
    sep = ""
  )

  writeLines(content, con = "README.md")
}

#' @noRd
create_geopressuretemplate_licences <- function(licenses) {
  # 1. If more than one license, display a warning using cli
  if (length(licenses) > 1) {
    cli_warn("Multiple licenses detected. Only the first license will be used.")
  }
  licenses <- licenses[[1]]

  # 2. Delete existing LICENSE.md file if it exists
  if (file.exists("LICENSE.md")) {
    file.remove("LICENSE.md")
  }

  # 3. Match and apply the license using usethis functions
  licenses$name <- tolower(licenses$name)

  if (grepl("mit", licenses$name)) {
    usethis::use_mit_license()
  } else if (grepl("gpl", licenses$name)) {
    usethis::use_gpl_license()
  } else if (grepl("agpl", licenses$name)) {
    usethis::use_agpl_license()
  } else if (grepl("lgpl", licenses$name)) {
    usethis::use_lgpl_license()
  } else if (grepl("apache", licenses$name)) {
    usethis::use_apache_license()
  } else if (grepl("cc0", licenses$name) ||
    grepl("cc-0", licenses$name) ||
    grepl("cc 0", licenses$name)) {
    usethis::use_cc0_license()
  } else if (grepl("ccby", licenses$name) ||
    grepl("cc-by", licenses$name) ||
    grepl("cc by", licenses$name)) {
    usethis::use_ccby_license()
  } else if (grepl("proprietary", licenses$name)) {
    usethis::use_proprietary_license()
  } else {
    cli_warn("No matching license found in {.pkg usethis}. License file not created.")
  }
}


#' @noRd
create_geopressuretemplate_data <- function(pkg) {
  check_gldp(pkg)

  # Observations
  readr::write_csv(tags(pkg), "./data/tags.csv")
  readr::write_csv(observations(pkg), "./data/observations.csv")

  # Create variable type from sensor and re-group sensors by type
  m <- measurements(pkg) %>%
    mutate(variable_type = .data$sensor) %>%
    mutate(sensor = case_when(
      stringr::str_detect(.data$variable_type, "pressure") ~ "pressure",
      stringr::str_detect(.data$variable_type, "activity|pitch") ~ "acceleration",
      stringr::str_detect(.data$variable_type, "light") ~ "light",
      stringr::str_detect(.data$variable_type, "temperature_internal") ~ "temperature_internal",
      stringr::str_detect(.data$variable_type, "temperature_external") ~ "temperature_external",
      stringr::str_detect(
        .data$variable_type,
        "acceleration_x|acceleration_y|acceleration_z|magnetic_x|magnetic_y|magnetic_z"
      ) ~
        "magnetic",
      TRUE ~ "other"
    ))

  # Create the tag-label directory if it doesn't exist
  path_label <- glue::glue("./data/tag-label/")
  if (!dir.exists(path_label)) {
    dir.create(path_label, recursive = TRUE)
  }
  path_twl <- glue::glue("./data/twilight-label/")
  if (!dir.exists(path_twl)) {
    dir.create(path_twl, recursive = TRUE)
  }

  split(m, m$tag_id) %>%
    purrr::iwalk(\(dft, tag_id) {
      # Create the raw-tag directory if it doesn't exist
      dir_path <- glue::glue("./data/raw-tag/{tag_id}")
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }

      # Split the data by sensor
      # Write the data to CSV
      split(dft, dft$sensor) %>%
        purrr::iwalk(\(dfts, sensor) {
          # remove unsed columns
          tmp <- dfts %>% select(-c("tag_id", "sensor", "label"))

          # Check for duplicates and merge with median if necessary
          duplicates_exist <- tmp %>%
            group_by(.data$datetime, .data$variable_type) %>%
            filter(n() > 1) %>%
            ungroup()

          if (nrow(duplicates_exist) > 0) {
            cli::cli_warn(c(
              "!" = "Duplicate measurements of {.var {sensor}} on the same datetime for \\
            {.field {tag_id}}.",
              "i" = "The median value will be taken for these duplicates."
            ))

            # grouping and median
            tmp <- tmp %>%
              group_by(.data$datetime, .data$variable_type) %>%
              summarize(value = stats::median(.data$value), .groups = "drop") %>%
              ungroup()
          }

          # Write raw-tag
          tmp %>%
            tidyr::pivot_wider(names_from = "variable_type", values_from = "value") %>%
            rename(value = any_of(c(
              "pressure", "acceleration", "light", "temperature_external",
              "temperature_internal", "magnetic", "activity"
            ))) %>%
            readr::write_csv(
              file = glue::glue("{dir_path}/{sensor}.csv")
            )
        })


      # Write tag label
      dft %>%
        filter(.data$sensor %in% c("pressure", "acceleration")) %>%
        transmute(
          date = .data$datetime,
          .data$value,
          label = ifelse(is.na(.data$label), "", .data$label),
          series = .data$sensor
        ) %>%
        GeoPressureR:::trainset_write(
          file = glue::glue("{path_label}/{tag_id}-labeled.csv"),
          quiet = TRUE
        )
    }, .progress = list(type = "tasks"))

  # Write twilight label
  twl <- twilights(pkg)

  split(twl, twl$tag_id) %>%
    purrr::iwalk(\(dft, tag_id) {
      dft %>%
        transmute(
          date = .data$twilight,
          value = as.numeric(format(.data$twilight, "%H")) * 60 +
            as.numeric(format(.data$twilight, "%M")),
          label = ifelse(is.na(.data$label), "", .data$label),
          series = ifelse(.data$rise, "Rise", "Set")
        ) %>%
        GeoPressureR:::trainset_write(
          file = glue::glue("{path_twl}/{tag_id}-labeled.csv"),
          quiet = TRUE
        )
    }, .progress = list(type = "tasks"))
}

#' @noRd
create_geopressuretemplate_config <- function(pkg) {
  check_gldp(pkg)

  t <- tags(pkg)
  o <- observations(pkg)

  t_config <- t %>%
    purrr::pmap(\(tag_id, ring_number, scientific_name, ...) {
      # Create the basic config from tag tibble
      co <- list(
        bird_create = list(
          scientific_name = scientific_name
        ),
        ring_number = ring_number
      )

      # Construct the known data.frame from observation
      k <- o %>%
        filter(.data$tag_id == !!tag_id) %>%
        arrange(.data$datetime) %>%
        transmute(
          stap_id = ifelse(.data$observation_type == "equipment", 1,
            ifelse(.data$observation_type == "retrieval", -1, 0)
          ),
          datetime = as.POSIXct(.data$datetime, tz = "UTC"),
          known_lon = .data$longitude,
          known_lat = .data$latitude,
          location_name = .data$location_name,
          device_status = .data$device_status,
          condition = .data$condition,
          age_class = .data$age_class,
          sex = .data$sex,
          observation_comments = .data$observation_comments
        )

      # Add sex if unique and defined (i.e., not U)
      usex <- unique(k$sex)
      if (length(usex) == 1 && usex != "U") {
        co$sex <- usex
      }

      # Conditionally remove the column
      rm_col <- c("sex")
      if (all(is.na(k$location_name) | k$location_name == "")) {
        rm_col <- c(rm_col, "location_name")
      }
      if (all(is.na(k$device_status) | k$device_status == "" | k$device_status == "unknown")) {
        rm_col <- c(rm_col, "device_status")
      }
      if (all(is.na(k$condition) | k$condition == "" | k$condition == "unknown")) {
        rm_col <- c(rm_col, "condition")
      }
      if (all(is.na(k$age_class) | k$age_class == "" | k$age_class == 0 | k$age_class == "0")) {
        rm_col <- c(rm_col, "age_class")
      }
      if (all(is.na(k$observation_comments) | k$observation_comments == "")) {
        rm_col <- c(rm_col, "observation_comments")
      }
      k <- k %>% select(any_of(names(k)[!names(k) %in% rm_col]))

      # Add crop date from equipement and retrieval
      co$tag_create <- list(
        crop_start = k %>%
          filter(.data$stap_id == 1) %>%
          mutate(dt = format(.data$datetime + as.difftime(1, units = "days"), "%Y-%m-%d")) %>%
          pull(.data$dt),
        # we start one day after equipment (at 00:00)
        crop_end = k %>%
          filter(.data$stap_id == -1) %>%
          mutate(dt = format(.data$datetime, "%Y-%m-%d")) %>%
          pull(.data$dt)
      )

      # Add known
      co$tag_set_map <- list(
        known = k %>% mutate(
          datetime = format(.data$datetime, "%Y-%m-%d")
        )
      )

      co
    })

  # Add tag_id as name
  names(t_config) <- t$tag_id

  # convert to yaml
  t_yaml <- yaml::as.yaml(
    t_config,
    handlers = list(
      data.frame = \(k) {
        tmp <- lapply(names(k), function(name) {
          x <- k[[name]]
          if (name == "stap_id" && any(x == 0)) {
            add_text <- " # Modify the `stap_id` of `0` to the correct stap_id"
          } else {
            add_text <- ""
          }
          if (!is.numeric(x)) {
            x <- glue::glue('"{x}"')
          }
          tmp <- paste0("[", paste(x, collapse = ", "), "]")

          # class(tmp) <- "verbatim" # does not work
          glue::glue("{tmp}{add_text}")
        })
        names(tmp) <- names(k)
        tmp
      }
    )
  )

  # Manually fix issue with tibble export
  t_yaml <- gsub("\\]\\'", "]", gsub("\\'\\[", "[", t_yaml))

  # Combine default config.yml with trim_yaml
  combined_yaml <- c(readLines("config.yml"), t_yaml)

  # Write the combined output to config.yml
  writeLines(combined_yaml, "config.yml")
}
