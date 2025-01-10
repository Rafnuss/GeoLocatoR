#' Create a GeoLocator Data Package
#'
#' @description
#' This function create a geolocator data package R object. The arguments of the functions
#' correspond to the [standard geolocator Data Package
#' properties](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/).
#'
#'
#' This function initiate a Geolocator Data Package R object. This function is similar to
#' [`frictionless::create_package()`] but required specific metadata provided as arguments to the
#' function rather than the list `descriptor`. These arguments correspond to field of
#' [`datapackage.json`](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/).
#'
#' A geolocator data package is also a Data Package object created by
#' [`frictionless::create_package()`
#' ](https://docs.ropensci.org/frictionless/reference/create_package.html) and most (all?) functions
#' from the frictionless package should work.
#'
#' Note that this function does not add any data resources, it returns an empty shell with only
#' project level metadata.
#'
#' @param title A string providing a title or one sentence description for this package. It should
#' be plain text (no markup), capitalised like a title, NOT end in a period and less than 65
#' characters. See [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#title)
#' and thw [Data Package specification](https://datapackage.org/standard/data-package/#title).
#' @param contributors A list of contributors, where each contributor is a list with properties
#' including at leas `title` but also optionally `givenName`, `familyName`, `path`, `email`,
#' `roles`, and `organization`. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#contributors), the
#' [Data Package specification](https://datapackage.org/standard/data-package/#contributors) and the
#' [R Packages](https://r-pkgs.org/description.html#sec-description-authors-at-r).
#' @param embargo End date of the embargo. Default to `"1970-01-01"`. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#embargo) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#embargo).
#' @param licenses A list of licenses under which the data is provided.  Usually, a single license
#' if sufficient and prefered. If you're not sure, checkout the [Creative Commons License Chooser
#' ](https://chooser-beta.creativecommons.org/) and the [Open Data Commons
#' ](https://opendatacommons.org/). `name` or `path` must be provided. Defaults is a CC-BY-4.0
#' license. See the [Geolocator DP
#' ](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#licenses) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#licenses).
#' @param id A globally unique identifier for the package, typically the DOI link of the
#' corresponding Zenodo repository which [can be reserved prior to publication
#' ](https://help.zenodo.org/docs/deposit/describe-records/reserve-doi/#reserve-doi). Default to
#' `NULL`. See the [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#id) and
#' the [Data Package specification](https://datapackage.org/standard/data-package/#id).
#' @param description A markdown-formatted string describing the package. You can
#' (and should!) use multiple sentences, but limited to a single paragraph. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#description) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#description).
#' @param version (optional) A version string identifying the version of the package, following
#' Semantic Versioning. Defaults to "1.0.0". See
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#version) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#version) and [Data
#' Package Version recipe](https://datapackage.org/recipes/data-package-version/).
#' @param relatedIdentifiers (optional) A list of related identifiers for the package. Each related
#' identifier is a list with properties `relationType` and `relatedIdentifier`. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#relatedIdentifiers) and
#' the [Camtrap DP specification
#' ](https://camtrap-dp.tdwg.org/metadata/#relatedIdentifiers).
#' @param grants (optional) A list of grants that funded the creation of the package. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#grants) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#grants).
#' @param keywords (optional) A list of keywords to assist users in searching for the package.
#' See the [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#keywords) and
#' the [Data Package specification](https://datapackage.org/standard/data-package/#keywords).
#' @param created Datetime on which this was created. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#created) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#created).
#' @param bibliographicCitation (optional) A string providing a citation for the package. See the
#' [Geolocator DP](https://raphaelnussbaumer.com/GeoLocator-DP/datapackage/#citation) and the
#' [Data Package specification](https://datapackage.org/standard/data-package/#citation).
#' @param schema (optional) A URL to the JSON Table Schema that describes the data package. Defaults
#' to the GeoLocator Data Package profile. See the [Data Package specification
#' ](https://datapackage.org/standard/data-package/#dollar-schema).
#'
#' @return A Geolocator Data Package object containing only metadata.
#'
#' @examples
#' # Create a Data Package with, at minimum a title and a list of contributors.
#' pkg <- create_gldp(
#'   title = "Geolocator Data Package example",
#'   contributors = list(
#'     list(
#'       title = "Raphaël Nussbaumer",
#'       roles = c("ContactPerson", "DataCurator", "ProjectLeader")
#'     ),
#'     list(
#'       title = "Yann Rime",
#'       roles = c("Researcher")
#'     )
#'   )
#' )
#'
#' pkg
#'
#' # See the structure of the (empty) Data Package
#' str(pkg)
#'
#' # Create a Data Package with all possible metadata
#' pkg <- create_gldp(
#'   title = "Geolocator Data Package example",
#'   contributors = list(
#'     list(
#'       title = "Raphaël Nussbaumer",
#'       roles = c("ContactPerson", "DataCurator", "ProjectLeader")
#'     ),
#'     list(
#'       title = "Yann Rime",
#'       roles = c("Researcher")
#'     )
#'   ),
#'   embargo = "2025-01-01",
#'   id = "https://doi.org/10.5281/zenodo.13829929",
#'   version = "1.0.1",
#'   relatedIdentifiers = list(
#'     list(
#'       relationType = "IsPartOf",
#'       relatedIdentifier = "10.5281/zenodo.11207081",
#'       relatedIdentifierType = "DOI"
#'     ),
#'     list(
#'       relationType = "IsSupplementTo",
#'       relatedIdentifier = "",
#'       relatedIdentifierType = "DOI"
#'     )
#'   ),
#'   grants = c("Swiss National Fundation grant no. 354251"),
#'   keywords = c("Woodland Kingfisher", "intra-african", "multi-sensor geolocator")
#' )
#'
#' @export
create_gldp <- function(
    title,
    contributors,
    id = NULL,
    embargo = "1970-01-01",
    licenses = list(list(
      name = "CC-BY-4.0",
      title = "Creative Commons Attribution 4.0",
      path = "https://creativecommons.org/licenses/by/4.0/"
    )),
    description = NULL,
    version = NULL,
    relatedIdentifiers = NULL,
    grants = NULL,
    keywords = NULL,
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    bibliographicCitation = NULL,
    schema = NULL) {
  # Assertions to check input validity
  assertthat::assert_that(assertthat::is.string(title))
  assertthat::assert_that(is.list(contributors))
  assertthat::assert_that(all(sapply(contributors, function(x) is.list(x) && !is.null(x$title))))

  if (is.null(schema) || is.na(schema)) {
    schema <- glue::glue(
      "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/v0.2/",
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

  if (is.null(description) || description == "") description <- NULL
  if (!is.null(description)) assertthat::assert_that(assertthat::is.string(description))
  if (!is.null(version)) assertthat::assert_that(assertthat::is.string(version))
  if (!is.null(keywords)) assertthat::assert_that(is.character(keywords))
  if (!is.null(bibliographicCitation))
    assertthat::assert_that(assertthat::is.string(bibliographicCitation))
  if (!is.null(grants)) assertthat::assert_that(is.character(grants))
  if (!is.null(relatedIdentifiers)) {
    assertthat::assert_that(is.list(relatedIdentifiers))
    assertthat::assert_that(all(sapply(
      relatedIdentifiers,
      \(x) is.list(x) && !is.null(x$relationType) && !is.null(x$relatedIdentifier)
    )))
  }
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
  if (!is.null(id)) descriptor$id <- id
  if (!is.null(description)) descriptor$description <- description
  if (!is.null(version)) descriptor$version <- version
  if (!is.null(keywords)) descriptor$keywords <- keywords
  if (is.null(bibliographicCitation)) descriptor$bibliographicCitation <- bibliographicCitation
  if (!is.null(grants)) descriptor$grants <- grants
  if (!is.null(relatedIdentifiers)) descriptor$relatedIdentifiers <- relatedIdentifiers

  # Create frictionless package
  package <- frictionless::create_package(descriptor = descriptor)

  class(package) <- c("geolocatordp", class(package))
  # This is the the version of the GeoLocator Data Package standard (as defined by the release
  # version on Gihub). Not to be confused with the version of the data pacakge being created.
  attr(package, "version") <- version(package)

  pkg <- package %>% update_bibliographicCitation()

  return(pkg)
}
