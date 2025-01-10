#' Add GLDP SOI Data to a Package
#'
#' This function adds data from the Swiss Ornithological Institute (SOI) to a package. It includes
#' tags, measurements, and observations based on the provided data frame and directory of data. The
#' function also handles missing directories and updates the package accordingly.
#'
#' @param pkg The package object to which the data will be added.
#' @param gdl A data frame containing the SOI data. Must include columns like `OrderName`,
#' `GDL_ID`, and other relevant fields for tags, measurements, and observations.
#' @param directory_data A character string specifying the path to the directory where data files
#' are located. This directory is used to locate and match GDL_IDs to their corresponding
#' directories.
#' @param allow_empty_o A logical value indicating whether to allow observations with missing
#' datetime values. Default is \code{FALSE}.
#' @param replace A logical value indicating whether to replace existing resources in the package
#' with the new data. Default is \code{FALSE}.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Validates the input package and the data frame.
#'   \item Checks and retrieves the directory information for each GDL_ID.
#'   \item Creates tag data, measurements, and observations based on the provided data.
#'   \item Adds these resources to the package, with options to replace existing resources.
#'   \item Updates the package metadata.
#' }
#'
#' @return The updated package object with the added resources.
#'
#' @examples
#' \dontrun{
#' my_package <- some_package_function()
#' my_gdl <- data.frame(
#'   OrderName = c("A", "B"),
#'   GDL_ID = c("001", "002"),
#'   # Additional required columns...
#' )
#' updated_package <- add_gldp_soi(
#'   pkg = my_package,
#'   gdl = my_gdl,
#'   directory_data = "/path/to/data",
#'   allow_empty_o = TRUE,
#'   contributors = list("John Doe" = c("aut", "DataCollector")),
#'   replace = TRUE
#' )
#' }
#'
#' @export
add_gldp_soi <- function(pkg,
                         gdl,
                         directory_data,
                         allow_empty_o = FALSE,
                         replace = FALSE) {
  check_gldp_pkg(pkg)
  assertthat::assert_that(is.data.frame(gdl))


  # Retrieve directory of all data and display warning message if absent
  if (!("directory" %in% names(gdl))) {
    gdl <- add_gldp_soi_directory(gdl, directory_data)
  }

  # Read tag data
  dtags <- gdl %>%
    filter(!is.na(.data$directory)) %>%
    select("GDL_ID", "directory") %>%
    purrr::pmap(
      \(GDL_ID, directory) { # nolint
        tryCatch(
          {
            if (grepl("\\.glf$", directory)) {
              GeoPressureR::tag_create(
                id = GDL_ID,
                directory = dirname(directory),
                light_file = basename(directory),
                assert_pressure = FALSE, # Allow tag to not have pressure data
                quiet = TRUE
              )
            } else {
              GeoPressureR::tag_create(
                id = GDL_ID,
                directory = directory,
                assert_pressure = FALSE, # Allow tag to not have pressure data
                quiet = TRUE
              )
            }
          },
          error = function(e) {
            list() # Return empty list on error
          }
        )
      },
      .progress = list(
        type = "tasks"
      )
    )

  # Create measurement tibble
  m <- tags2m(dtags)

  if (nrow(m) > 0) {
    pkg <- add_gldp_resource(pkg, "measurements", m, replace = replace)
  }

  # Adding tag resource
  t <- gdl %>%
    rowwise() %>%
    mutate(
      attachment_type = paste0(
        c(
          if (!is.na(.data[["Harness_data"]])) .data[["Harness_data"]],
          if (!is.na(.data[["HarnessMaterial_data"]])) {
            glue::glue("material:{.data[['HarnessMaterial_data']]}")
          },
          if (!is.na(.data[["HarnessAttachement_data"]])) {
            glue::glue("attachement:{.data[['HarnessAttachement_data']]}")
          },
          if (!is.na(.data[["HarnessThickness"]])) {
            glue::glue("thickness:{.data[['HarnessThickness']]}")
          },
          if (!is.na(.data[["LegHarnessDiameter"]])) {
            glue::glue("legDiameter:{.data[['LegHarnessDiameter']]}")
          },
          if (!is.na(.data[["BreastHarnessDiameterHead"]])) {
            glue::glue("BreastDiameterHead:{.data[['BreastHarnessDiameterHead']]}")
          },
          if (!is.na(.data[["BreastHarnessDiameterTail"]])) {
            glue::glue("BreastDiameterTail:{.data[['BreastHarnessDiameterTail']]}")
          }
        ),
        collapse = "|"
      )
    ) %>%
    ungroup() %>%
    transmute(
      tag_id = .data$GDL_ID,
      manufacturer = "Swiss Ornithological Institute",
      model = glue::glue("{.data$GDL_Type}-{.data$HardwareVersion}"),
      firmware = .data$FirmwareVersion,
      weight = .data$TotalWeight,
      attachment_type = .data$attachment_type,
      # readout_method = "tag-retrieval",
      scientific_name = .data$Species,
      ring_number = .data$RingNumber,
      tag_comments = .data$Remarks,
    )

  pkg <- add_gldp_resource(pkg, "tags", t, replace = replace)

  # Adding sensor resource
  o <- bind_rows(
    gdl %>% transmute(
      ring_number = .data$RingNumber,
      tag_id = .data$GDL_ID,
      datetime = .data$UTC_Attached,
      location_name = .data$SiteAttached,
      longitude = .data$LongitudeAttached,
      latitude = .data$LatitudeAttached,
      observation_type = "equipment",
      life_stage = "0",
      sex = "U",
    ),
    gdl %>% transmute(
      ring_number = .data$RingNumber,
      tag_id = .data$GDL_ID,
      datetime = .data$UTC_Removed,
      longitude = .data$LongitudeRemoved,
      latitude = .data$LatitudeRemoved,
      observation_type = "retrieval",
      life_stage = "0",
      sex = "U",
    )
  )

  if (!allow_empty_o) {
    o <- o %>% filter(!is.na(.data$datetime))
  }

  if (nrow(o) > 0) {
    pkg <- add_gldp_resource(pkg, "observations", o, replace = replace)
  }

  return(pkg)
}



#' @noRd
add_gldp_soi_directory <- function(gdl, directory_data) {
  # Check if the required columns are present
  assertthat::assert_that(
    all(c("OrderName", "GDL_ID") %in% colnames(gdl)),
    msg = "The input tibble must contain {.field OrderName} and {.field GDL_ID} columns."
  )
  assertthat::assert_that(!any(is.na(gdl$OrderName) | gdl$OrderName == ""),
    msg = "The 'OrderName' column contains empty values."
  )
  assertthat::assert_that(!any(is.na(gdl$GDL_ID) | gdl$GDL_ID == ""),
    msg = "The 'GDL_ID' column contains empty values."
  )
  assertthat::assert_that(file.exists(directory_data) && file.info(directory_data)$isdir)

  # Function to get the highest alphabetical directory path matching the GDL_ID pattern
  check_folder_exists <- function(order_name, gdl_id, base_dir) {
    order_dir <- file.path(base_dir, order_name)

    # 1 find folder with tag_id name
    folders <- list.dirs(order_dir, recursive = FALSE, full.names = FALSE)
    matching_folders <- sort(folders[grepl(glue::glue("^{gdl_id}"), folders)], decreasing = TRUE)
    if (length(matching_folders) == 1) {
      return(file.path(order_dir, matching_folders[1]))
    } else if (length(matching_folders) > 1) {
      print(matching_folders)
      return(file.path(order_dir, matching_folders[1]))
    }

    # 2 find file with tag_id name
    if (order_name == "Wallis") {
      files <- c(
        list.files(file.path(base_dir, "UpuEpoCH09/glf"),
          recursive = FALSE, full.names = TRUE
        ),
        list.files(file.path(base_dir, "UpuEpoCH10/"), recursive = FALSE, full.names = TRUE)
      )
    } else {
      files <- list.files(order_dir, recursive = FALSE, full.names = TRUE)
    }

    matching_files <- files[grepl(glue::glue("^{gdl_id}"), basename(files))]
    # Check if there are matching files and prioritize .glf files
    if (length(matching_files) > 0) {
      # Look for a .glf file first
      glf_files <- matching_files[grepl("\\.glf$", matching_files)]

      # If there are .glf files, return the first one, otherwise return the first matching file
      if (length(glf_files) > 0) {
        return(glf_files[1])
      } else {
        return(matching_files[1])
      }
    }


    return(NA)
  }

  # Apply the check_folder_exists function to each row and add directory and folder_exists columns
  gdl <- gdl %>%
    rowwise() %>%
    mutate(directory = check_folder_exists(.data$OrderName, .data$GDL_ID, directory_data)) %>%
    ungroup()

  # Display warning for any missing directories
  gdl_id_na_dir <- gdl %>%
    filter(is.na(.data$directory)) %>%
    pull(.data$GDL_ID)

  if (length(gdl_id_na_dir) > 0) {
    cli::cli_warn("We could not find the data directory for {length(gdl_id_na_dir)} tags (out of \
                      {nrow(gdl)}). GDL_IDs: {.field {gdl_id_na_dir}}. These will not be imported.")
  }

  return(gdl)
}
