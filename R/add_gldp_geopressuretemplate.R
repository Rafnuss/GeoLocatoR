#' Add GLDP Geopressure Template
#'
#' This function adds geopressuretemplates to a GLDP package by reading data from specified
#' directories and files.
#'
#' @param pkg A GLDP package object.
#' @param directory A character string specifying the directory where the data files are located.
#' @param from A character string specifying the source of the data files. Can be "raw-tag" (for
#' creating `tag` based on the data in `data/raw-tag/`) or
#' "interim" for data in `data/interim`. If `NULL` (default), the function will determine the
#' source based on the presence of at least one file in the "interim" directory.
#' @inheritParams add_gldp_resource
#'
#' @return The updated GLDP package object with added geopressure templates.
#'
#' @details
#' The function performs the following steps:
#' 1. Reads the "tags.csv" and "observations.csv" files from the "./data" directory if they exist
#' and adds them to the GLDP package.
#' 3. Determines the source of the data files ("data" or "interim") if the `from` parameter is NULL.
#' 4. Reads all ".RData" files from the specified source directory and processes them.
#'
#' You can exclude interim file to be included in the package by starting the file name with an `_`.
#'
#' It is not possible to do a mix of some `tag` read from `from="data"` and some `tag` read from
#' `from="interim"`.
#'
#' @examples
#' \dontrun{
#' pkg <- create_gldp_package()
#' pkg <- add_gldp_geopressuretemplate(pkg, directory = "path/to/data")
#' }
#'
#' @export
add_gldp_geopressuretemplate <- function(
    pkg,
    directory = ".",
    from = NULL, # "raw-tag", "interim", ????"config"
    replace = FALSE) {
  check_gldp_pkg(pkg)

  # Check if the directory exists
  if (!dir.exists(directory)) {
    cli_abort(
      message = "The specified directory does not exist: {.file {directory}}."
    )
  }

  # Change working directory to the specified directory so that GeoPressureR can work with default
  # value: setwd(directory)
  pkg <- withr::with_dir(directory, {
    # Read tags and observations files if present
    if (file.exists("./data/tags.xlsx")) {
      t <- readxl::read_excel("./data/tags.xlsx")
      pkg <- add_gldp_resource(pkg, "tags", t, replace = replace)
    } else if (file.exists("./data/tags.csv")) {
      t <- readr::read_csv("./data/tags.csv", show_col_types = FALSE)
      pkg <- add_gldp_resource(pkg, "tags", t, replace = replace)
    }

    if (file.exists("./data/observations.xlsx")) {
      o <- readxl::read_excel("./data/observations.xlsx")
      pkg <- add_gldp_resource(pkg, "observations", o, replace = replace, cast_type = TRUE)
    } else if (file.exists("./data/observations.csv")) {
      o <- readr::read_csv("./data/observations.csv", show_col_types = FALSE)
      pkg <- add_gldp_resource(pkg, "observations", o, replace = replace, cast_type = TRUE)
    }


    if (is.null(from)) {
      if (length(list.files(path = "./data/interim/", pattern = "\\.RData$", full.names = TRUE))
      > 0) {
        from <- "interim"
      } else {
        from <- "raw-tag"
      }
    }
    assertthat::assert_that(from %in% c("interim", "raw-tag"))



    if (from == "interim") {
      all_files <- list.files(path = "./data/interim/", pattern = "\\.RData$", full.names = TRUE)
      # Exclude folder starting with _
      all_files <- all_files[!grepl("^_", basename(all_files))]

      if (length(all_files) == 0) {
        cli::cli_warn(
          "We did not find any interim .Rdata file in {.file
          {file.path(directory, 'data/interim')}}."
        )
      }

      # List of variable names to be processed
      var_names <- c(
        "marginal", "tag", "param", "path_most_likely", "edge_most_likely",
        "path_simulation", "edge_simulation", "pressurepath_most_likely",
        "pressurepath_geopressureviz", "path_geopressureviz"
      )

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

      # Adding measurements resource
      m <- tags2m(interim$tag)
      pkg <- add_gldp_resource(pkg, "measurements", m, replace = replace)


      # Adding tag resource
      if (!("tags" %in% frictionless::resources(pkg))) {
        t <- params2t(interim$param)
        pkg <- add_gldp_resource(pkg, "tags", t, replace = replace)
      }

      # Add twilights
      twl <- interim$tag %>%
        purrr::map(function(tag) {
          tag$twilight %>%
            mutate(tag_id = tag$param$id)
        }) %>%
        purrr::list_rbind()

      if (nrow(twl) > 0) {
        pkg <- add_gldp_resource(pkg, "twilights", twl, replace = replace)
      }

      # Add stap
      staps <- interim$tag %>%
        purrr::map(function(tag) {
          tag$stap %>%
            mutate(tag_id = tag$param$id)
        }) %>%
        purrr::list_rbind() %>%
        select(-any_of(c("duration", "nb_sample")))


      if (nrow(staps) > 0) {
        pkg <- add_gldp_resource(pkg, "staps", staps, replace = replace)
      }

      # Add Path
      paths <- c("path_most_likely", "path_simulation", "path_geopressureviz", "path_tag") %>%
        purrr::map(\(x) {
          if (x %in% names(interim)) {
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
          }
        }) %>%
        purrr::list_rbind() %>%
        tibble::tibble() %>%
        select(-any_of(c("start", "end", "include")))

      if (nrow(paths) > 0) {
        pkg <- add_gldp_resource(pkg, "paths", paths, replace = replace)
      }

      # Add Edge
      edges <- c("edge_most_likely", "edge_simulation", "edge_geopressureviz", "edge_tag") %>%
        purrr::map(\(x) {
          if (x %in% names(interim)) {
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
          }
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


      # Adding tag resource if not present as csv or xlsx
      if (!("tags" %in% frictionless::resources(pkg))) {
        t <- interim$param %>% params2t()
        pkg <- add_gldp_resource(pkg, "tags", t, replace = replace)
      }

      # Adding observations resource if not present as csv or xlsx
      if (!("observations" %in% frictionless::resources(pkg))) {
        o <- interim$param %>% params2o()
        pkg <- add_gldp_resource(pkg, "observations", o, replace = replace)
      }
    } else if (from == "raw-tag") {
      # OPTION 1: Read from directory:
      # Read tag data
      all_dirs <- list.dirs(path = "data/raw-tag", recursive = FALSE)
      # Exclude folder starting with _
      all_dirs <- all_dirs[!grepl("^_", basename(all_dirs))]

      list_id <- basename(all_dirs)

      if (length(list_id) == 0) {
        cli::cli_warn(
          "We did not find any tag data in {.file {file.path(directory, 'data/raw-tag')}}."
        )
      }

      dtags <- list_id %>%
        purrr::map(
          \(id) {
            tag <- GeoPressureR::tag_create(
              id,
              crop_start = config::get("tag_create", id)$crop_start,
              crop_end = config::get("tag_create", id)$crop_end,
              assert_pressure = FALSE, # Allow tag to not have pressure data
              quiet = TRUE
            )
            if (file.exists(glue::glue("./data/tag-label/{id}-labeled.csv"))) {
              tag <- GeoPressureR::tag_label(tag, quiet = TRUE)
            }
            return(tag)
          },
          .progress = list(
            type = "tasks"
          )
        )

      # Adding measurements resource if not present as csv or xlsx
      m <- tags2m(dtags)
      pkg <- add_gldp_resource(pkg, "measurements", m, replace = replace)

      # Adding tag resource
      if (!("tags" %in% frictionless::resources(pkg))) {
        t <- dtags %>%
          purrr::map(~ .x$param) %>%
          params2t()
        pkg <- add_gldp_resource(pkg, "tags", t, replace = replace, cast_type = TRUE)
      }

      # Adding observations resource if not present as csv or xlsx
      if (!("observations" %in% frictionless::resources(pkg))) {
        o <- dtags %>%
          purrr::map(~ .x$param) %>%
          params2o()
        pkg <- add_gldp_resource(pkg, "observations", o, replace = replace, cast_type = TRUE)
      }
    }

    pkg <- update_gldp(pkg)
  })

  return(pkg)
}
