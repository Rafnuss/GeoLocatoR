#' Create a GeoLocator Data Package from a GeoPressure Template
#'
#' This function creates a GeoLocator Data Package based on the information found in a
#' `DESCRIPTION` file located in the specified directory. It utilizes the `create_gldp` function
#' and ensures that required fields are present, converting them into a format compatible with
#' GeoLocator Data Package standards.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Checks if the provided directory exists.
#' 2. Reads the `DESCRIPTION` file using the `desc` package.
#' 3. Verifies the presence of mandatory fields (`Title`, `Authors@R`, and `License`) and warns
#'    about the absence of suggested fields (`Version` and `Description`).
#' 4. Maps author roles to their corresponding GeoLocator roles.
#' 5. Creates and returns a GeoLocator Data Package with the information extracted from the
#' `DESCRIPTION` file.
#'
#' @param directory A string specifying the path to the directory containing the
#' GeoPressureTemplate, including the `DESCRIPTION` file.
#'
#' @return A list containing the descriptor for the GeoLocator Data Package.
#'
#' @export
create_gldp_geopressuretemplate <- function(directory = ".") {
  # Check if the directory exists
  if (!dir.exists(directory)) {
    cli_abort(
      message = "The specified directory does not exist: {.file {directory}}."
    )
  }
  pkg <- withr::with_dir(directory, {
    # setwd(directory)

    if (!file.exists("DESCRIPTION")) {
      cli_abort(
        message = "The specified directory has no {.file DESCRIPTION} file."
      )
    }

    # Read description file
    d <- desc::desc()

    # Check for mandatory variables
    for (var in c("Title", "Authors@R", "License")) {
      if (!d$has_fields(var)) {
        cli_abort("{.field {var}} is not present in {.file DESCRIPTION} and is mandatory.")
      }
    }

    # Check for suggested variable
    for (var in c("Version", "Description")) {
      if (!d$has_fields(var)) {
        cli::cli_warn("{.field {var}} is not present in
                      {.file DESCRIPTION} and is strongly suggested")
      }
    }

    role_mapping <- c(
      "aut" = "Researcher",
      "com" = "DataCollector",
      "cph" = "RightsHolder",
      "cre" = "ProjectLeader",
      "ctb" = "Contributor",
      "ctr" = "Contributor",
      "dtc" = "DataCollector",
      "fnd" = "Sponsor",
      "rev" = "Editor",
      "ths" = "Supervisor",
      "trl" = "RelatedPerson"
    )

    contributors <- lapply(d$get_authors(), function(x) {
      list(
        title = glue::glue("{x$given} {x$family}"),
        givenName = x$given,
        familyName = x$family,
        roles = as.character(role_mapping[x$role]),
        email = x$email,
        path = glue::glue("https://orcid.org/{x$comment[['ORCID']]}")
        # organization = ""
      )
    })

    # Create package
    pkg <- create_gldp(
      title = d$get("Title"),
      contributors = contributors,
      description = d$get("Description"),
      # id = ,
      # Code license is not the same as the data license.
      # licenses = list(list(name = as.character(d$get("License")))),
      # version = as.character(d$get_version()), # default
      # keywords =
    )
    return(pkg)
  })
  return(pkg)
}
