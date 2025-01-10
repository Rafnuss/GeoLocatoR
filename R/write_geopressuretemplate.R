#' Create a Geopressure Template Project
#' @description
#' Initializes a geopressure template project by creating a specified directory structure and
#' populating it with essential files, including a DESCRIPTION file, README, license, and data.
#'
#' This function sets up the project directory and ensures that all necessary components are in
#' place for a geopressure analysis project.
#'
#' @param pkg A list containing package metadata, including `name`, `title`, `description`,
#' `version`, `licenses`, and `contributors`.
#' @param destdir A character string specifying the destination directory where the project will
#' be created. Default is the current directory (`"."`).
#' @param project_name A character string that sets the name of the project. Default is derived
#' from `pkg$name`.
#' @param overwrite A logical value indicating whether to overwrite an existing directory with the
#' same name. Default is `FALSE`.
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
write_geopressuretemplate <- function(pkg,
                                      destdir = ".",
                                      project_name = pkg$name,
                                      overwrite = FALSE,
                                      open = rlang::is_interactive()) {
  # Create a geopressuretemplate directory
  project_dir <- write_geopressuretemplate_create(
    destdir = destdir,
    project_name = project_name,
    overwrite = overwrite
  )

  # setwd(project_dir)
  withr::with_dir(project_dir, {
    # Update the description file
    write_geopressuretemplate_desc(pkg)

    # Update the README file
    write_geopressuretemplate_readme(pkg)

    # Update the LICENSE file
    write_geopressuretemplate_licences(pkg$licenses)

    # Add data
    write_geopressuretemplate_data(pkg)

    write_geopressuretemplate_config(pkg)
  })

  if (open) {
    rstudioapi::openProject(project_dir, newSession = TRUE)
  }

  return(project_dir)
}

#' @noRd
write_geopressuretemplate_create <- function(destdir, project_name, overwrite) {
  if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
    cli::cli_alert_warning("Created directory: {destdir}")
  }

  if (dir.exists(file.path(destdir, project_name))) {
    if (overwrite) {
      unlink(file.path(destdir, project_name), recursive = TRUE)
    } else {
      cli::cli_abort(c(
        x = "Directory already exists: {project_name}",
        ">" = "Use {.code overwrite = TRUE} to overwrite the existing directory."
      ))
    }
  }

  # usethis::create_from_github
  repo_url <- "https://github.com/Rafnuss/GeoPressureTemplate/archive/refs/heads/main.zip"
  temp_zip <- tempfile(fileext = ".zip")

  # Download the ZIP file
  curl::curl_download(repo_url, temp_zip)

  # Unzip the file
  zip::unzip(temp_zip, exdir = destdir)

  # Clean up temporary file
  unlink(temp_zip)

  project_dir <- file.path(destdir, project_name)
  file.rename(
    from = file.path(destdir, "GeoPressureTemplate-main"),
    to = project_dir
  )

  file.rename(
    from = file.path(project_dir, "GeoPressureTemplate.Rproj"),
    to = file.path(project_dir, glue::glue("{project_name}.Rproj"))
  )

  return(project_dir)
}

#' @noRd
write_geopressuretemplate_desc <- function(pkg) {
  d <- desc::description$new()

  d$set("Title", pkg$title)
  d$set("License", paste(purrr::map_chr(pkg$license, ~ .x$name), collapse = ", "))

  d$set_authors(contributors2persons(pkg$contributors))

  if ("name" %in% names(pkg)) {
    name <- gsub("([._-])([a-z])", "\\U\\2", pkg$name, perl = TRUE)
    name <- gsub("[^A-Za-z0-9.]", "", name)
    d$set("Package", name)
  }

  # Optional fields
  if ("description" %in% names(pkg)) {
    d$set("Description", pkg$description)
  }
  if ("version" %in% names(pkg)) {
    d$set_version(pkg$version)
  }
  if ("homepage" %in% names(pkg)) {
    d$set_urls(pkg$homepage)
  } else {
    d$clear_urls()
  }

  d$normalize()
  d$write()
}

#' @noRd
write_geopressuretemplate_readme <- function(pkg) {
  content <- paste(
    "# ", pkg$title, "\n\n",
    if (!is.null(pkg$description)) paste(pkg$description, "\n\n") else "",
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
    if (!is.null(pkg$embargo)) paste("**Embargo until:** ", pkg$embargo, "\n") else "",
    if (!is.null(pkg$keywords)) {
      paste("**Keywords:** ", paste(pkg$keywords, collapse = ", "), "\n")
    } else {
      ""
    },
    sep = ""
  )

  writeLines(content, con = "README.md")
}

#' @noRd
write_geopressuretemplate_licences <- function(licenses) {
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

  if (grepl("MIT", licenses$name)) {
    usethis::use_mit_license()
  } else if (grepl("GPL", licenses$name)) {
    usethis::use_gpl_license()
  } else if (grepl("AGPL", licenses$name)) {
    usethis::use_agpl_license()
  } else if (grepl("LGPL", licenses$name)) {
    usethis::use_lgpl_license()
  } else if (grepl("Apache", licenses$name)) {
    usethis::use_apache_license()
  } else if (grepl("CC0", licenses$name) || grepl("CC-0", licenses$name) ||
    grepl("CC 0", licenses$name)) {
    usethis::use_cc0_license()
  } else if (grepl("CCBY", licenses$name) || grepl("CC-BY", licenses$name) ||
    grepl("CC BY", licenses$name)) {
    usethis::use_ccby_license()
  } else if (grepl("Proprietary", licenses$name)) {
    usethis::use_proprietary_license()
  } else {
    cli_warn("No matching license found in {.pkg usethis}. License file not created.")
  }
}


#' @noRd
write_geopressuretemplate_data <- function(pkg) {
  # Observations
  writexl::write_xlsx(tags(pkg), "./data/tags.xlsx")
  writexl::write_xlsx(observations(pkg), "./data/observations.xlsx")

  # delete existing data
  unlink("./data/raw-tag/", recursive = TRUE)
  unlink("./data/tag-label/", recursive = TRUE)
  unlink("./data/twilight-label/", recursive = TRUE)

  m <- measurements(pkg)

  split(m, m$tag_id) %>%
    purrr::iwalk(\(dft, tag_id) {
      # Split the data by sensor
      dft_split <- dft %>%
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

      # Create the raw-tag directory if it doesn't exist
      dir_path <- glue::glue("./data/raw-tag/{tag_id}")
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }

      # Write the data to CSV
      split(dft_split, dft_split$sensor) %>%
        purrr::iwalk(\(dfts, sensor) {
          dfts %>%
            select(-c("tag_id", "sensor", "label")) %>%
            tidyr::pivot_wider(names_from = "variable_type", values_from = "value") %>%
            rename(value = any_of(c(
              "pressure", "acceleration", "light", "temperature_external",
              "temperature_internal", "magnetic", "activity"
            ))) %>%
            readr::write_csv(
              file = glue::glue("{dir_path}/{sensor}.csv")
            )
        })

      # Create the tag-label directory if it doesn't exist
      dir_path <- glue::glue("./data/tag-label/{tag_id}")
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
    }, .progress = list(type = "tasks"))
}

#' @noRd
write_geopressuretemplate_config <- function(pkg) {
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
          life_stage = .data$life_stage,
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
      if (all(is.na(k$life_stage) | k$life_stage == "" | k$life_stage == 0 | k$life_stage == "0")) {
        rm_col <- c(rm_col, "life_stage")
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

      return(co)
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
            add_text <- " !!!Modify the `stap_id` of `0` to the correct stap_id"
          } else {
            add_text <- ""
          }
          if (!is.numeric(x)) {
            x <- glue::glue('"{x}"')
          }
          tmp <- paste0("[", paste(x, collapse = ", "), "]")

          # class(tmp) <- "verbatim" # does not work
          return(glue::glue("{tmp}{add_text}"))
        })
        names(tmp) <- names(k)
        return(tmp)
      }
    )
  )

  # Manually fix issue with tibble export
  t_yaml <- gsub("\\]\\'", "]", gsub("\\'\\[", "[", t_yaml))


  # Read the default config.yml
  default_yaml <- readLines("config.yml")

  # Remove any example
  trim_yaml <- default_yaml[1:(grep("18LX:", default_yaml)[1] - 1)]

  # Combine trim_yaml and t_yaml
  combined_yaml <- c(trim_yaml, t_yaml)

  # Write the combined output to config.yml
  writeLines(combined_yaml, "config.yml")
}
