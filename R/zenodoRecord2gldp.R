#' Create or update the metadata of a GeoLocator Data Package based on a Zenodo Record
#'
#' @description
#' This function update the metadata of a [GeoLocator Data Package
#' ](https://datapackage.org/standard/data-package/) based on an `zen4R` ZenodoRecord provided. If no
#' package is provided (`pkg = NULL`), a new data package is created.
#'
#' All metadata are retrieved with the Zenodo API through the R package
#' [zen4R](https://github.com/eblondel/zen4R).
#'
#' Note that no data is actually retrieved/copied.
#'
#' @param zenodo_record A `zen4R` ZenodoRecord object from which to extract metadata.
#' @param pkg (optionall) A Geolocator Data Package object on which the metadata should be added. If
#' not provided, create a new one.
#'
#' @return An updated or new Geolocator Data Package object with metadata from the zenodo record.
#'
#' @export
zenodoRecord2gldp <- function(zenodo_record,
                              pkg = NULL) {
  z <- zenodo_record
  assertthat::assert_that(all(c("ZenodoRecord", "zen4RLogger", "R6") %in% class(z)))

  # Process contributors
  contributors <- purrr::map(z$metadata$creators, \(c) {
    list(
      title = c$person_or_org$name,
      givenName = c$person_or_org$given_name,
      familyName = c$person_or_org$family_name,
      path = glue::glue("https://orcid.org/{purrr::pluck(purrr::keep(c$person_or_org$identifiers,
                        ~ .x$scheme == 'orcid'), 1)$identifier}"),
      email = NULL,
      roles = c(c$role$id),
      organization = glue::glue_collapse(purrr::map_chr(c$affiliations, "name"), sep = ", ")
    )
  })

  # Determine embargo date
  embargo <- if (z$access$embargo$active) {
    z$access$embargo$until
  } else {
    "1970-01-01"
  }

  # Process licenses
  licenses <- purrr::map(z$metadata$rights, \(x) {
    list(
      name = x$id,
      title = x$title$en,
      path = x$props$url
    )
  })

  # Process related identifiers
  relatedIdentifiers <- purrr::map(z$metadata$related_identifiers, \(x) {
    list(
      relationType = x$relation_type$id,
      relatedIdentifier = x$identifier,
      resourceTypeGeneral = x$resource_type$id,
      relatedIdentifierType = x$scheme
    )
  })

  # Extract grants
  grants <- purrr::map_chr(z$metadata$funding, ~ .x$funder$name)

  # Extract keywords
  keywords <- purrr::map_chr(z$metadata$subjects, "subject")

  if (!is.null(pkg)) {
    check_gldp(pkg)
    pkg$title <- z$metadata$title
    pkg$contributors <- contributors
    pkg$embargo <- embargo
    pkg$licenses <- licenses
    pkg$id <- glue::glue("https://doi.org/{z$getConceptDOI()}")
    pkg$description <- z$metadata$description
    pkg$version <- z$metadata$version
    pkg$relatedIdentifiers <- relatedIdentifiers
    pkg$grants <- grants
    pkg$keywords <- keywords
    pkg$created <- z$metadata$publication_date
    # pkg$bibliographicCitation = format(bib)
    # pkg$schema = NULL

    pkg <- pkg %>%
      update_gldp_bibliographic_citation() %>%
      update_gldp_metadata()
  } else {
    # Create the GLDP package
    pkg <- create_gldp(
      title = z$metadata$title,
      contributors = contributors,
      embargo = embargo,
      licenses = licenses,
      id = glue::glue("https://doi.org/{z$getConceptDOI()}"),
      description = z$metadata$description,
      version = z$metadata$version,
      relatedIdentifiers = relatedIdentifiers,
      grants = grants,
      keywords = keywords,
      created = z$metadata$publication_date,
      # bibliographicCitation = format(bib),
      # schema = NULL,
    )
  }

  # Return
  return(pkg)
}
