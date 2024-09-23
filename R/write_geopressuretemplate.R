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
                                      overwrite = FALSE) {
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
  })

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

  # nolint start
  role_mapping <- c(
    "ContactPerson" = "ctr", # Contractor (assumed due to lack of clear match)
    "Contributor" = "ctb", # Contributor
    "DataCollector" = "dtc", # Data contributor
    "DataCurator" = "dtc", # Data contributor (no closer match)
    "DataManager" = "dtc", # Data contributor (no closer match)
    "Distributor" = "ctr", # Contractor (assumed)
    "Editor" = "rev", # Reviewer
    "HostingInstitution" = "cph", # Copyright holder (legal entity)
    "Producer" = "aut", # Author (substantial contribution)
    "ProjectLeader" = "cre", # Creator (project leader matches)
    "ProjectManager" = "cre", # Creator (manager of the project)
    "ProjectMember" = "ctb", # Contributor (smaller contributions)
    "RegistrationAgency" = "ctr", # Contractor (assumed)
    "RegistrationAuthority" = "ctr", # Contractor (assumed)
    "RelatedPerson" = "trl", # Translator (assumed)
    "Researcher" = "aut", # Author
    "ResearcherGroup" = "aut", # Author (group treated as authors)
    "RightsHolder" = "cph", # Copyright holder
    "Sponsor" = "fnd", # Funder
    "Supervisor" = "ths", # Thesis advisor
    "WorkPackageLeader" = "cre" # Creator (assumed leader role)
  )
  # nolint end

  contributors <- pkg$contributors %>%
    purrr::map(~ {
      utils::person(
        given = ifelse(is.null(.x$givenName) & !is.null(.x$title), .x$title, .x$givenName),
        family = .x$familyName,
        email = .x$email,
        role = map_vec(.x$roles, ~ role_mapping[.x]),
        comment = c(.x$path, .x$organization)
      )
    })

  contributors <- do.call(c, Filter(Negate(is.null), contributors))

  d$set_authors(contributors)

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
          str_detect(.data$variable_type, "pressure") ~ "pressure",
          str_detect(.data$variable_type, "activity|pitch") ~ "acceleration",
          str_detect(.data$variable_type, "light") ~ "light",
          str_detect(.data$variable_type, "temperature|airtemperature") ~ "temperature",
          str_detect(.data$variable_type, "gX|gY|gZ|mX|mY|mZ") ~ "magnetic",
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
              "pressure", "acceleration", "light", "temperature",
              "airtemperature", "magnetic", "activity"
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
