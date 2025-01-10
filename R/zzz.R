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

  return(persons)
}
