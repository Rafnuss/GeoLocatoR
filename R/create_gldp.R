#' Create a GeoLocator Data Package
#'
#' This function create a [Data package](https://datapackage.org/standard/data-package/) for
#' GeoLocator data. This function is a wrapper of [`create_package()`] with additional
#' constrains following GeoLocatorDP. The arguments of the functions correspond to the [standard
#' Data Package properties](https://datapackage.org/standard/data-package/#properties).
#'
#' In addition, to the metadata entered as argument,
#'
#' You can find more information about how to enter these meta-data information in the [R package
#' description](https://r-pkgs.org/description.html#the-description-file).
#'
#' @param title A string providing a title or one sentence description for this package. See the
#' [Data Package specification](https://datapackage.org/standard/data-package/#title). It should be
#' plain text (no markup), capitalised like a title, NOT end in a period and less than 65
#' characters.
#' @param description A markdown-formatted string describing the package. See the
#' [Data Package specification](https://datapackage.org/standard/data-package/#description).You can
#' (and should!) use multiple sentences, but limited to a single paragraph.
#' @param contributors A list of contributors, where each contributor is a list with properties
#' including at leas `title` but also optionally `givenName`, `familyName`, `path`, `email`,
#' `roles`, and `organization`. See the [Data Package specification
#' ](https://datapackage.org/standard/data-package/#contributors).
#' For Role, we suggest to use R standards (`aut`, `cre`, `ctb`, `cph` or `fnd`) see [R package
#' documentations for more details
#' ](https://r-pkgs.org/description.html#sec-description-authors-at-r)
#' @param id A globally unique identifier for the package, such as a UUID or DOI. Defaults to NULL.
#' See the [Data Package specification](https://datapackage.org/standard/data-package/#id).
#' @param licenses A list of licenses under which the package is provided. Each license is a list
#' with properties `name` and `path` and optionally `title`. Defaults is a CC-BY-4.0 license. See
#' the [Data Package specification](https://datapackage.org/standard/data-package/#licenses).
#' @param homepage (optional) A URL for the home on the web related to this data package.
#' See the [Data Package specification](https://datapackage.org/standard/data-package/#homepage).
#' @param image (optional) A URL or path pointing to the location of an image representing the data
#' package. See the
#' [Data Package specification](https://datapackage.org/standard/data-package/#image).
#' @param version (optional) A version string identifying the version of the package, following
#' Semantic Versioning. Defaults to "1.0.0". See the
#' [Data Package specification](https://datapackage.org/standard/data-package/#version) and [Data
#' Package Version recipe](https://datapackage.org/recipes/data-package-version/).
#' @param embargo End date of the embargo. Default to `"1970-01-01"`. See the
#' [Data Package specification](https://datapackage.org/standard/data-package/#embargo).
#' @param keywords (optional) A list of keywords to assist users in searching for the package.
#' See the [Data Package specification](https://datapackage.org/standard/data-package/#keywords).
#' @param schema (optional) A URL to the JSON Table Schema that describes the data package. Defaults
#' to the GeoLocator Data Package profile. See the [Data Package specification
#' ](https://datapackage.org/standard/data-package/#dollar-schema).
#' @param name (optional) A string providing a name for the package. Defaults to a slugified version
#' of the title. See the [Data Package specification
#' ](https://datapackage.org/standard/data-package/#name).
#' @param citation (optional) A string providing a citation for the package. See the [Data Package
#' specification](https://datapackage.org/standard/data-package/#citation).
#' @param grants (optional) A list of grants that funded the creation of the package. See the [Data
#' Package specification](https://datapackage.org/standard/data-package/#grants).
#' @param related_identifiers (optional) A list of related identifiers for the package. Each related
#' identifier is a list with properties `relationType` and `relatedIdentifier`. See the [Data
#' Package specification](https://datapackage.org/standard/data-package/#related-identifiers).
#' @param references (optional) A list of references for the package. See the [Data Package
#' specification](https://datapackage.org/standard/data-package/#references).
#'
#' @return A list containing the descriptor for the data package.
#' @export
create_gldp <- function(
    title,
    contributors,
    schema = NULL,
    name = NULL,
    id = NULL,
    licenses = list(list(
      name = "CC-BY-4.0",
      title = "Creative Commons Attribution 4.0",
      path = "https://creativecommons.org/licenses/by/4.0/"
    )),
    description = NULL,
    homepage = NULL,
    image = NULL,
    version = NULL,
    embargo = "1970-01-01",
    keywords = NULL,
    citation = NULL,
    grants = NULL,
    related_identifiers = NULL,
    references = NULL) {
  # Assertions to check input validity
  assertthat::assert_that(assertthat::is.string(title))
  assertthat::assert_that(is.list(contributors))
  assertthat::assert_that(all(sapply(contributors, function(x) is.list(x) && !is.null(x$title))))

  if (is.null(schema)) {
    schema <- glue::glue(
      "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/main/",
      "geolocator-dp-profile.json"
    )
  }
  assertthat::assert_that(assertthat::is.string(schema))
  assertthat::assert_that(grepl("^https?://[[:alnum:].-]+/?", schema))

  if (is.null(name)) {
    # Generate name from title
    # Convert to lowercase
    name <- tolower(title)
    # Replace any characters that are not alphanumeric, ., -, or _
    name <- gsub("[^a-z0-9._-]", "-", name)
    # Remove leading or trailing non-alphanumeric characters
    name <- gsub("^[^a-z0-9]+|[^a-z0-9]+$", "", name)
    # Collapse multiple occurrences of ._- into a single -
    name <- gsub("[._-]+", "-", name)
  }
  name <- tolower(name)
  assertthat::assert_that(grepl("^[a-z0-9._-]+$", name))

  if (!is.null(id)) assertthat::assert_that(assertthat::is.string(id))

  assertthat::assert_that(is.list(licenses))
  assertthat::assert_that(all(sapply(
    licenses, function(x) {
      is.list(x) && (assertthat::is.string(x$name) || assertthat::is.string(x$path))
    }
  )))

  if (!is.null(description)) assertthat::assert_that(assertthat::is.string(description))
  if (!is.null(homepage)) {
    assertthat::assert_that(assertthat::is.string(homepage))
    assertthat::assert_that(grepl("^https?://[[:alnum:].-]+/?", homepage))
  }
  if (!is.null(image)) assertthat::assert_that(assertthat::is.string(image))
  if (!is.null(version)) assertthat::assert_that(assertthat::is.string(version))
  if (!is.null(keywords)) assertthat::assert_that(is.character(keywords))
  if (!is.null(citation)) assertthat::assert_that(assertthat::is.string(citation))
  if (!is.null(grants)) assertthat::assert_that(is.character(grants))
  if (!is.null(related_identifiers)) {
    assertthat::assert_that(is.list(related_identifiers))
    assertthat::assert_that(all(sapply(
      related_identifiers,
      \(x) is.list(x) && !is.null(x$relationType) && !is.null(x$relatedIdentifier)
    )))
  }
  if (!is.null(references)) assertthat::assert_that(is.character(references))
  assertthat::assert_that(assertthat::is.date(as.Date(embargo)))

  # Create the descriptor list
  descriptor <- list(
    "$schema" = schema,
    licenses = licenses,
    title = title,
    contributors = contributors,
    embargo = embargo
  )

  # Conditionally add optional elements
  if (!is.null(name)) descriptor$name <- name
  if (!is.null(id)) descriptor$id <- id
  if (!is.null(description)) descriptor$description <- description
  if (!is.null(homepage)) descriptor$homepage <- homepage
  if (!is.null(version)) descriptor$version <- version
  if (!is.null(image)) descriptor$image <- image
  if (!is.null(keywords)) descriptor$keywords <- keywords
  if (!is.null(citation)) descriptor$citation <- citation
  if (!is.null(grants)) descriptor$grants <- grants
  if (!is.null(related_identifiers)) descriptor$relatedIdentifiers <- related_identifiers
  if (!is.null(references)) descriptor$references <- references

  # Create frictionless package
  package <- frictionless::create_package(descriptor = descriptor)

  # Add other field automatically
  package <- update_gldp(package)

  class(package) <- c("geolocatordp", class(package))
  # This is the the version of the GeoLocator Data Package standard (as defined by the release
  # version on Gihub). Not to be confused with the version of the data pacakge being created.
  attr(package, "version") <- version(package)

  return(package)
}
