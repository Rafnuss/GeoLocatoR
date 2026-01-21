#' Transform a GeoLocator Data Package to EML
#'
#' @description
#' Transforms a [GeoLocator Data Package](https://raphaelnussbaumer.com/GeoLocator-DP/)
#' to [Ecological Metadata Language (EML)](https://eml.ecoinformatics.org/).
#' The resulting EML file can be uploaded to an [IPT](https://www.gbif.org/ipt) for
#' publication to GBIF and/or OBIS. A corresponding Darwin Core Archive can be created
#' with [gldp_to_dwc()].
#'
#' @param package A GeoLocator Data Package object.
#' @param directory Path to local directory to write files to.
#'
#' @return `eml.xml` file written to disk.
#'   Invisibly, an [EML::eml] object.
#'
#' @section Transformation details:
#' Metadata are derived from the GeoLocator Data Package and transformed to EML.
#' The following properties are set:
#'
#' - `title`: Package title as provided in `package$title`.
#' - `abstract`: Package description as provided in `package$description`.
#' - `pubDate`: Publication year extracted from `package$created`.
#' - `creator`: Contributors with roles `"ProjectLeader"`, `"Researcher"`, or
#'   `"DataCurator"` as provided in `package$contributors`.
#' - `contact`: Contributors with role `"ContactPerson"` as provided in
#'   `package$contributors`.
#' - `associatedParty`: Other contributors as provided in `package$contributors`,
#'   including those with role `"RightsHolder"`.
#' - `intellectualRights`: License information from `package$licenses`.
#' - `keywords`: Keywords as provided in `package$keywords`.
#' - `packageId`: Package identifier as provided in `package$id`. If no ID is
#'   provided, a UUID is generated. As a result, no new DOI will be created when
#'   publishing to GBIF if `package$id` contains a DOI.
#'
#' Coverage information:
#' - `temporalCoverage`: Date range from `package$temporal` (if available),
#'   derived from measurement timestamps.
#' - `geographicCoverage`: Bounding box from `package$spatial` (if available),
#'   calculated from all locations in observations, paths, and pressurepaths.
#' - `taxonomicCoverage`: Species list from `package$taxonomic` (if available),
#'   derived from unique scientific names in tags.
#'
#' Methods:
#' - `methodStep`: Generic description indicating data were processed using the
#'   GeoLocator Data Package standard.
#'
#' The following EML properties are not set:
#' - `type`
#' - `subtype`
#' - `update frequency`
#' - `publishing organization`
#' - `project data`
#' - `citations`
#' - `collection data`
#'
#' @export
gldp_to_eml <- function(package, directory) {
  check_gldp(package)

  if (!requireNamespace("EML", quietly = TRUE)) {
    cli::cli_abort("The {.pkg EML} package is required for this function.")
  }
  if (!requireNamespace("uuid", quietly = TRUE)) {
    cli::cli_abort("The {.pkg uuid} package is required for this function.")
  }

  # Helper to make sure text is valid XML (e.g. replace HTML nbsp)
  sanitize_xml_text <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    gsub("&nbsp;", " ", x, fixed = TRUE)
  }

  # Title
  title <- sanitize_xml_text(package$title)

  # Creators and Contacts
  creators <- list()
  contacts <- list()
  associated_parties <- list()

  for (contributor in package$contributors) {
    # Normalise possibly missing fields
    given_name <- contributor$givenName
    family_name <- contributor$familyName
    organization_name <- contributor$organization
    email <- contributor$email

    # If absolutely no identifying info is available, skip this contributor
    if (is.null(given_name) && is.null(family_name) && is.null(organization_name)) {
      next
    }

    # Create EML person/party (role will be added later, if needed)
    party <- EML::set_responsibleParty(
      givenName = given_name,
      surName = family_name,
      organizationName = organization_name,
      electronicMailAddress = email,
      userId = if (!is.null(contributor$path) && grepl("orcid.org", contributor$path)) {
        list(directory = "https://orcid.org/", sub("https://orcid.org/", "", contributor$path))
      } else {
        NULL
      }
    )

    roles <- contributor$roles
    if (is.null(roles)) {
      roles <- character(0)
    }

    if ("ContactPerson" %in% roles) {
      contacts <- c(contacts, list(party))
    }

    if (any(c("ProjectLeader", "Researcher", "DataCurator") %in% roles)) {
      creators <- c(creators, list(party))
    } else if (!("ContactPerson" %in% roles) && length(roles) > 0) {
      # If not a creator or contact, add as associated party with first role (when available)
      party$role <- roles[1]
      associated_parties <- c(associated_parties, list(party))
    }

    if ("RightsHolder" %in% roles) {
      party$role <- "rightsHolder"
      associated_parties <- c(associated_parties, list(party))
    }
  }

  # Deduplicate creators if needed (simple check)
  # EML::set_responsibleParty returns a list, so we have a list of lists.

  # Abstract
  abstract <- list(para = sanitize_xml_text(package$description))

  # Keywords
  keyword_set <- list(
    keyword = sanitize_xml_text(package$keywords)
  )

  # Intellectual Rights
  intellectual_rights <- list(
    para = sanitize_xml_text(paste(
      "This work is licensed under a",
      purrr::map_chr(package$licenses, "name") |> paste(collapse = ", "),
      "license."
    ))
  )

  # Coverage
  coverage <- list()

  # Temporal
  if (!is.null(package$temporal)) {
    coverage$temporalCoverage <- list(
      rangeOfDates = list(
        beginDate = list(calendarDate = package$temporal$start),
        endDate = list(calendarDate = package$temporal$end)
      )
    )
  }

  # Spatial
  if (!is.null(package$spatial)) {
    # Extract bounding box from polygon
    coords <- package$spatial$coordinates
    # coords is array(dim=c(1,5,2))
    lons <- coords[1, , 1]
    lats <- coords[1, , 2]

    coverage$geographicCoverage <- list(
      geographicDescription = "Bounding box of all locations",
      boundingCoordinates = list(
        westBoundingCoordinate = min(lons),
        eastBoundingCoordinate = max(lons),
        northBoundingCoordinate = max(lats),
        southBoundingCoordinate = min(lats)
      )
    )
  }

  # Taxonomic
  if (!is.null(package$taxonomic)) {
    coverage$taxonomicCoverage <- list(
      taxonomicClassification = lapply(package$taxonomic, function(sp) {
        list(
          taxonRankName = "Species",
          taxonRankValue = sp
        )
      })
    )
  }

  # Methods (optional, maybe generic text)
  methods <- list(
    methodStep = list(
      description = list(
        para = "Data were processed using the GeoLocator Data Package standard."
      )
    )
  )

  # Dataset
  dataset <- list(
    title = title,
    creator = creators,
    contact = contacts,
    associatedParty = associated_parties,
    pubDate = format(as.Date(package$created), "%Y"),
    abstract = abstract,
    keywordSet = keyword_set,
    intellectualRights = intellectual_rights,
    coverage = coverage,
    methods = methods
  )

  # EML object
  eml <- list(
    packageId = if (is.null(package$id)) uuid::UUIDgenerate() else package$id,
    system = "uuid",
    dataset = dataset
  )

  # Write file
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  eml_path <- file.path(directory, "eml.xml")

  EML::write_eml(eml, eml_path)

  cli::cli_alert_success("EML metadata file written to {.file {eml_path}}")

  invisible(eml)
}
