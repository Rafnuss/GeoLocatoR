#' @noRd
contributors2persons <- function(contributors) {
  # nolint start
  role_mapping <- c(
    "contactperson" = "ctr", # Contractor (assumed due to lack of clear match)
    "contributor" = "ctb", # Contributor
    "datacollector" = "dtc", # Data contributor
    "datacurator" = "dtc", # Data contributor (no closer match)
    "datamanager" = "dtc", # Data contributor (no closer match)
    "distributor" = "ctr", # Contractor (assumed)
    "editor" = "rev", # Reviewer
    "hostinginstitution" = "cph", # Copyright holder (legal entity)
    "producer" = "aut", # Author (substantial contribution)
    "projectleader" = "cre", # Creator (project leader matches)
    "projectmanager" = "cre", # Creator (manager of the project)
    "projectmember" = "ctb", # Contributor (smaller contributions)
    "registrationagency" = "ctr", # Contractor (assumed)
    "registrationauthority" = "ctr", # Contractor (assumed)
    "relatedperson" = "trl", # Translator (assumed)
    "researcher" = "aut", # Author
    "researchergroup" = "aut", # Author (group treated as authors)
    "rightsholder" = "cph", # Copyright holder
    "sponsor" = "fnd", # Funder
    "supervisor" = "ths", # Thesis advisor
    "workpackageleader" = "cre" # Creator (assumed leader role)
  )
  # nolint end

  persons <- contributors %>%
    purrr::map(~ {
      utils::person(
        given = ifelse(is.null(.x$givenName) & !is.null(.x$title), .x$title, .x$givenName),
        family = .x$familyName,
        email = .x$email,
        role = purrr::map_vec(.x$roles, ~ dplyr::coalesce(role_mapping[tolower(.x)], "ctb")),
        comment = c(.x$path, .x$organization)
      )
    })

  persons <- do.call(c, Filter(Negate(is.null), persons))

  persons
}


#' @noRd
cast_table <- function(data, schema) {
  schema_fields <- sapply(schema$fields, \(x) x$name)
  schema_types <- sapply(schema$fields, \(x) x$type)

  for (i in seq_along(schema_fields)) {
    field <- schema_fields[i]
    type <- schema_types[i]

    if (field %in% names(data)) {
      if (type == "string") {
        data[[field]] <- as.character(data[[field]])
      } else if (type == "number") {
        data[[field]] <- as.numeric(data[[field]])
      } else if (type == "integer") {
        data[[field]] <- as.integer(data[[field]])
      } else if (type == "boolean") {
        data[[field]] <- as.logical(data[[field]])
      } else if (type == "date") {
        data[[field]] <- as.Date(data[[field]])
      } else if (type == "datetime") {
        data[[field]] <- as.POSIXct(data[[field]])
      } else {
        cli::cli_warn("No casting for {.field {field}} of type {.val {type}}")
      }
    }
  }
  data
}
