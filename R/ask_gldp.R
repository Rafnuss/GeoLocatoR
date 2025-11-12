#' Interactive GeoLocator Data Package Creator
#'
#' @description
#' This function interactively asks the user questions in the console
#' to collect all the information needed to create a GeoLocator Data Package.
#' It is a wrapper around [create_gldp()].
#'
#' @return A Geolocator Data Package object.
#' @export
ask_gldp <- function() {
  cli::cli_h1("GeoLocator Data Package Creator")

  # Title
  title <- ask_field("What is the title of your project? ", required = TRUE)

  # Contributors
  contributors <- list()
  add_more <- TRUE
  while (add_more) {
    cli::cli_h2("Add a contributor")
    title <- ask_field("Full name", fmt = "John Doe", required = TRUE)
    # givenName <- ask_field("Given name")
    # familyName <- ask_field("Family name")
    path <- ask_field("ORCID/URL")
    email <- ask_field("Email")
    organization <- ask_field("Organization")
    roles <- ask_field("Roles", fmt = "ContactPerson,Researcher", comma = TRUE)

    contributors <- append(contributors, list(
      list(
        title = title,
        # givenName = givenName,
        # familyName = familyName,
        path = path,
        email = email,
        organization = organization,
        roles = roles
      )
    ))
    add_more <- ask_yes_no("Add another contributor?")
  }

  # Embargo
  embargo <- ask_field("Embargo end date", fmt = "YYYY-MM-DD", default = "1970-01-01")

  # License
  license_name <- ask_field("License name", default = "CC-BY-4.0")
  licenses <- list(list(name = license_name))

  # ID
  id <- ask_field("DOI or unique ID")

  # Description (optional)
  description <- ask_field("Short description", fmt = "markdown allowed")

  # Version
  version <- ask_field("Version", default = "1.0.0")

  # Keywords
  # keywords <- ask_field("Keywords", comma = TRUE)
  # keywords <- if (!is.null(keywords)) strsplit(keywords, ",")[[1]]

  # Grants
  # grants <- ask_field("Grants", comma = TRUE)

  # Citation
  # citation <- ask_field("Bibliographic citation")

  # Related identifiers
  # relatedIdentifiers <- list()
  # add_more <- tolower(ask_field("Add related identifiers? [y/n]: ")) == "y"
  # while (add_more) {
  #   relationType <- ask_field("Relation type (e.g. IsPartOf, IsSupplementTo): ")
  #   relatedIdentifier <- ask_field("Identifier (DOI/URL/etc.): ")
  #   relatedIdentifierType <- ask_field("Identifier type (DOI, URL, ISBN): ")
  #   relatedIdentifiers <- append(relatedIdentifiers, list(
  #     list(
  #       relationType = relationType,
  #       relatedIdentifier = relatedIdentifier,
  #       relatedIdentifierType = relatedIdentifierType
  #     )
  #   ))
  #   add_more <- tolower(ask_field("Add another related identifier? [y/n]: ")) == "y"
  # }
  # if (length(relatedIdentifiers) == 0) relatedIdentifiers <- NULL

  # Now call the actual function
  pkg <- create_gldp(
    title = title,
    contributors = contributors,
    embargo = embargo,
    licenses = licenses,
    id = id,
    description = description,
    version = version,
    # relatedIdentifiers = relatedIdentifiers,
    # grants = grants,
    # keywords = keywords,
    # bibliographicCitation = citation
  )

  cli::cli_alert_success("GeoLocator Data Package created!")
  pkg
}

ask_field <- function(text, prompt = ">> ",
                      required = TRUE,
                      fmt = NULL,
                      default = NULL,
                      comma = FALSE) {
  if (required) {
    text <- paste0(text, " (required")
  } else {
    text <- paste0(text, " (optional")
  }
  if (!is.null(fmt)) {
    text <- paste0(text, ", e.g, ", fmt)
  }
  if (!is.null(default)) {
    text <- paste0(text, ", default:", default)
  }
  if (comma) {
    text <- paste0(text, ", comma seperated")
  }
  text <- paste0(text, "):")

  ans <- ""
  while (required && ans == "") {
    cli::cli_text(text)
    ans <- readline(prompt)

    if (ans == "") {
      if (required) {
        cli::cli_alert_danger("This field is required. Please enter a value.")
      } else {
        ans <- default
      }
    }
  }

  if (comma) {
    ans <- if (!is.null(ans)) strsplit(ans, ",")[[1]]
  }

  ans
}

ask_yes_no <- function(question, prompt = ">> ") {
  ans <- ""
  while (!ans %in% c("y", "n")) {
    cli::cli_text(question)
    ans <- tolower(readline(prompt))
    if (!ans %in% c("y", "n")) {
      cli::cli_alert_danger("Please answer with {.val y} or {.val n}.")
    }
  }
  ans == "y"
}
