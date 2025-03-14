#' Add Geolocator DP resources from a GeoPressureTemplate
#'
#' @description
#' This function adds all possible resources to a Geolocator Data Package by reading data from
#' a [GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate) directories and files.
#'
#' You can find more information on the use of this function in the [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/geolocator-create.html)
#'
#' @param pkg A GeoLocator Data Package object.
#' @param directory A character string specifying the geopressuretemplate directory.
#' @param from A character vector specifying the source of the data files. Either or both of
#' `"raw-tag"` (for creating `tag` based on the data in `data/raw-tag/`) and `"interim"` for data
#' in `data/interim`.
#' @inheritParams add_gldp_resource
#'
#' @return The updated GLDP package object with added geopressure templates.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads the "tags.csv" and "observations.csv" files from the "./data" directory if they exist
#' and adds them to the GLDP package.
#' 3. Determines the source of the data files ("data" or "interim") if the `from` parameter is NULL.
#' 4. If `interim`, reads all interim ".RData" files, extract all resources possibles and add them
#' to the package.
#' 5. If `raw-data`, create GeoPressureR `tag` object from the `data/raw-data/`, compute the `tags`
#' and `observations` resources and add them to the package.
#'
#' You can exclude interim file to be included in the package by starting the file name with an `_`.
#'
#' It is not possible to do a mix of some `tag` read from `from="data"` and some `tag` read from
#' `from="interim"`.
#'
#' @export
add_gldp_geopressuretemplate <- function(
    pkg,
    directory = ".",
    from = c("raw-tag", "interim"),
    replace = FALSE) {
  # Check input
  check_gldp(pkg)
  # Check if the directory exists
  if (!dir.exists(directory)) {
    cli_abort(
      message = "The specified directory does not exist: {.file {directory}}."
    )
  }
  assertthat::assert_that(all(from %in% c("interim", "raw-tag")))
  assertthat::assert_that(is.logical(replace))

  # Initiate empty resources to be able to merge interim and raw-tag as necessary
  t <- NULL
  o <- NULL
  m <- NULL

  # Change working directory to the specified directory so that GeoPressureR can work with default
  # value: setwd(directory)
  pkg <- withr::with_dir(directory, {
    # STEP 1: Read all interim file available
    if ("interim" %in% from) {
      all_files <- list.files(path = "./data/interim/", pattern = "\\.RData$", full.names = TRUE)
      # Exclude folder starting with _
      all_files <- all_files[!grepl("^_", basename(all_files))]

      # List of variable names to be processed
      var_names_required <- c("tag", "param")
      var_names_path <- c("path_simulation", "path_geopressureviz", "path_tag", "path_most_likely")
      var_names_edges <- c("edge_most_likely", "edge_simulation", "edge_geopressureviz", "edge_tag")
      var_names_pressurepath <- c(
        "pressurepath_most_likely", "pressurepath_geopressureviz",
        "path_geopressureviz"
      )
      var_names <- c(var_names_required, var_names_path, var_names_edges, var_names_pressurepath)

      # Initialize lists dynamically
      interim <- stats::setNames(vector("list", length(var_names)), var_names)
      interim <- lapply(interim, function(x) vector("list", length(all_files)))

      # Loop through files and populate the result lists
      for (i in seq_along(all_files)) {
        save_list <- load(all_files[i])
        for (var in var_names) {
          if (var %in% save_list) {
            interim[[var]][[i]] <- get(var)
          }
        }
      }

      # Check for required variables
      for (var in var_names_required) {
        is_null_var <- sapply(interim[[var]], is.null)
        if (any(is_null_var)) {
          cli::cli_abort(
            "Interim file {.file {basename(all_files)[is_null_var]}} have no variable \\
                {.var {var}}"
          )
        }
      }

      # Crete tag resource
      t <- params2t(interim$param)

      # Create observations resource
      o <- params2o(interim$param)

      # Create measurements resource
      m <- tags2m(interim$tag)

      # Add twilights
      twl <- interim$tag %>%
        purrr::map(function(tag) {
          if ("twilight" %in% names(tag)) {
            tag$twilight %>%
              mutate(tag_id = tag$param$id)
          } else {
            NULL
          }
        }) %>%
        purrr::compact() %>% # Remove NULLs
        purrr::list_rbind()

      if (nrow(twl) > 0) {
        pkg <- add_gldp_resource(pkg, "twilights", twl, replace = replace)
      }

      # Add stap
      staps <- interim$tag %>%
        purrr::map(function(tag) {
          if ("stap" %in% names(tag)) {
            tag$stap %>%
              mutate(tag_id = tag$param$id)
          } else {
            NULL
          }
        }) %>%
        purrr::compact() %>% # Remove NULLs
        purrr::list_rbind() %>%
        select(-any_of(c("duration", "nb_sample")))


      if (nrow(staps) > 0) {
        pkg <- add_gldp_resource(pkg, "staps", staps, replace = replace)
      }

      # Add Path
      paths <- var_names_path %>%
        purrr::map(\(x) {
          interim[[x]] %>%
            purrr::imap(function(p, i) {
              if (!is.null(p)) {
                p %>%
                  mutate(
                    type = sub("path_", "", x),
                    tag_id = interim$tag[[i]]$param$id
                  )
              }
            }) %>%
            purrr::list_rbind()
        }) %>%
        purrr::list_rbind() %>%
        tibble::tibble() %>%
        select(-any_of(c("start", "end", "include")))

      if (nrow(paths) > 0) {
        pkg <- add_gldp_resource(pkg, "paths", paths, replace = replace)
      }

      # Add Edge
      edges <- var_names_edges %>%
        purrr::map(\(x) {
          interim[[x]] %>%
            purrr::imap(function(e, i) {
              if (!is.null(e)) {
                e %>%
                  mutate(
                    type = sub("edge_", "", x),
                    tag_id = interim$tag[[i]]$param$id
                  )
              }
            }) %>%
            purrr::list_rbind()
        }) %>%
        purrr::list_rbind() %>%
        tibble::tibble()

      if ("gs" %in% names(edges)) {
        edges <- edges %>%
          mutate(
            gs_u = Re(.data$gs),
            gs_v = Im(.data$gs),
          ) %>%
          select(-c("gs"))
      }

      if ("ws" %in% names(edges)) {
        edges <- edges %>%
          mutate(
            ws_u = Re(.data$ws),
            ws_v = Im(.data$ws),
          ) %>%
          select(-c("ws"))
      }

      if (nrow(edges) > 0) {
        pkg <- add_gldp_resource(pkg, "edges", edges, replace = replace)
      }

      # Add Pressurepath
      pressurepaths <- var_names_pressurepath %>%
        purrr::map(\(x) {
          interim[[x]] %>%
            purrr::imap(function(p, i) {
              if (!is.null(p)) {
                p %>%
                  mutate(
                    type = sub("pressurepath_", "", x),
                    tag_id = interim$tag[[i]]$param$id
                  ) %>%
                  select(-any_of("datetime")) %>%
                  rename(
                    datetime = date
                  )
              }
            }) %>%
            purrr::list_rbind()
        }) %>%
        purrr::list_rbind() %>%
        tibble::tibble() %>%
        select(-any_of(c("known", "include")))

      if (nrow(pressurepaths) > 0) {
        pkg <- add_gldp_resource(pkg, "pressurepaths", pressurepaths, replace = replace)
      }
    }

    # STEP 2: Read raw tag data for the file not in interim
    if ("raw-tag" %in% from) {
      # Read tag data
      all_dirs <- list.dirs(path = "data/raw-tag", recursive = FALSE)
      # Exclude folder starting with _
      all_dirs <- all_dirs[!grepl("^_", basename(all_dirs))]
      # Get the list of tag_id
      list_id <- basename(all_dirs)

      # Remove tag_id already present in t
      list_id <- list_id[!(list_id %in% t$tag_id)]

      # Not sure about this warning
      if (length(list_id) == 0 && FALSE) {
        cli::cli_warn(
          "We did not find any tag data in {.file {file.path(directory, 'data/raw-tag')}}."
        )
      }

      dtags <- list_id %>%
        purrr::map(rawtagid2tag, .progress = list(type = "tasks"))

      # Adding measurements resource
      m <- bind_rows(m, tags2m(dtags))

      # Adding tag resource
      t <- bind_rows(
        t,
        dtags %>%
          purrr::map(~ .x$param) %>%
          params2t()
      )

      # Adding observations resource
      o <- bind_rows(
        o,
        dtags %>%
          purrr::map(~ .x$param) %>%
          params2o()
      )
    }

    # STEP 3: Overwrite tags and observations if csv/xlsx files present
    if (file.exists("./data/tags.xlsx")) {
      tf <- readxl::read_excel(
        "./data/tags.xlsx",
        col_types = c(
          tag_id = "text",
          ring_number = "text",
          scientific_name = "text",
          manufacturer = "text",
          model = "text",
          firmware = "text",
          weight = "numeric",
          attachment_type = "text",
          readout_method = "text",
          tag_comments = "text"
        )
      )
    } else if (file.exists("./data/tags.csv")) {
      tf <- readr::read_csv(
        "./data/tags.csv",
        col_types = readr::cols(
          tag_id = "c",
          ring_number = "c",
          scientific_name = "c",
          manufacturer = "c",
          model = "c",
          firmware = "c",
          weight = "d",
          attachment_type = "c",
          readout_method = "c",
          tag_comments = "c"
        )
      )
    }

    # Check that all tag_id are in tf
    if (!all(t$tag_id %in% tf$tag_id)) {
      cli::cli_warn(c(
        "!" = "Not all {.var tag_id} in {.file {file}} are present in the interim/raw data.",
        "i" = "We will proceed with a merge of the two.",
        ">" = "Please, fix {.file {file}}"
      ))
      t <- bind_rows(
        filter(t, !(.data$tag_id %in% tf$tag_id)),
        filter(tf, .data$tag_id %in% t$tag_id)
      )
    } else {
      t <- tf
    }


    if (file.exists("./data/observations.xlsx")) {
      o <- readxl::read_excel(
        "./data/observations.xlsx",
        col_types = c(
          ring_number = "text", # ring_number: character
          tag_id = "text", # tag_id: character
          observation_type = "text", # observation_type: character
          datetime = "date", # datetime: date-time (ISO 8601 format)
          latitude = "numeric", # latitude: numeric
          longitude = "numeric", # longitude: numeric
          location_name = "text", # location_name: character
          device_status = "text", # device_status: character
          observer = "text", # observer: character
          catching_method = "text", # catching_method: character
          age_class = "text", # age_class: character
          sex = "text", # sex: character
          condition = "text", # condition: character
          mass = "numeric", # mass: numeric
          wing_length = "numeric", # wing_length: numeric
          additional_metric = "text", # additional_metric: character
          observation_comments = "text" # observation_comments: character
        )
      )
    } else if (file.exists("./data/observations.csv")) {
      o <- readr::read_csv(
        "./data/observations.csv",
        col_types = readr::cols(
          ring_number = "c",
          tag_id = "c",
          observation_type = "c",
          datetime = "T", # ISO 8601 format
          latitude = "d",
          longitude = "d",
          location_name = "c",
          device_status = "c",
          observer = "c",
          catching_method = "c",
          age_class = "c",
          sex = "c",
          condition = "c",
          mass = "d",
          wing_length = "d",
          additional_metric = "c",
          observation_comments = "c"
        )
      )
    }

    # Use add_gldp_resource instead of tags() <- to avoid update
    pkg <- add_gldp_resource(pkg, "tags", t, replace = replace)
    pkg <- add_gldp_resource(pkg, "observations", o, replace = replace)
    pkg <- add_gldp_resource(pkg, "measurements", m, replace = replace)

    pkg <- pkg %>%
      update_gldp_taxonomic() %>%
      update_gldp_number_tags() %>%
      update_gldp_spatial() %>%
      update_gldp_temporal()

    return(pkg)
  })

  return(pkg)
}

#' @noRd
rawtagid2tag <- function(id, display_config_error = TRUE) {
  config <- tryCatch(
    {
      GeoPressureR::geopressuretemplate_config(
        id,
        tag_create = list(assert_pressure = FALSE)
      )
    },
    error = function(e) {
      if (display_config_error) {
        # Warn that the configuration file could not be read and display the error
        cli::cli_warn(c(
          "i" = "Configuration file {.file config.yml} could not be read to build the
                datapackage.",
          ">" = "Create the tag with default value wit
                    {.fun GeoPressureR::param_create}.",
          "!" = "Error: {e$message}"
        ))
        display_config_error <- FALSE
      }
      GeoPressureR::param_create(id,
        default = TRUE,
        tag_create = list(assert_pressure = FALSE)
      )
    }
  )

  tag <- do.call(GeoPressureR::tag_create, c(
    list(id = id, quiet = TRUE),
    config$tag_create
  ))

  tag <- tryCatch(
    {
      GeoPressureR::tag_label_read(
        tag = tag,
        file = config$tag_label$file
      )

      GeoPressureR::tag_label_stap(
        tag = tag,
        quiet = TRUE,
        file = config$tag_label$file
      )

      tag <- do.call(GeoPressureR::tag_set_map, c(
        list(tag = tag),
        config$tag_set_map
      ))

      tag # return the value
    },
    error = function(e) {
      # Cheat to still keep tag_set_map information even without label and tag_set_map
      tag$param$tag_set_map <- config$tag_set_map
      tag # return the value
    }
  )

  tag$param$bird_create <- config$bird_create

  return(tag)
}
