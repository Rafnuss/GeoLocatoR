#' Merge Two GeoLocator Data Packages
#'
#' Merges two GeoLocator Data Package objects (`x` and `y`) into a single combined package.
#' The metadata properties from both packages are merged according to specific rules,
#' and resource data is combined based on their presence in either package.
#'
#' **Metadata merging rules:**
#' - **title**: Combined from both packages, separated by a "/".
#' - **contributors**: Combined from both packages, with duplicates removed.
#' - **embargo**: Set to the latest date from both packages.
#' - **licenses**: Combined from both packages, with duplicates removed.
#' - **id**: Removed from the merged package.
#' - **description**: Combined as two separate paragraphs, with a newline separator.
#' - **version**: Removed from the merged package.
#' - **relatedIdentifiers**: Combined, with duplicates removed.
#' - **grants**: Combined from both packages, with duplicates removed.
#' - **keywords**: Combined from both packages, with duplicates removed.
#' - **created**: Set to the current timestamp at the time of merging.
#' - **bibliographicCitation**: Removed from the merged package.
#' - Custom properties from `x` are retained in the merged package.
#'
#' **Resource merging logic:**
#' - Each resource is checked for its presence in both `x` and `y`.
#' - Data from both sources is combined if the resource exists in either `x` or `y`.
#' - Resources are only included if they exist in at least one of the packages.
#'
#' @param x A GeoLocator Data Package object.
#' @param y A GeoLocator Data Package object.
#' @return A GeoLocator Data Package object containing the merged data from both `x` and `y`.
#'
#' @export
merge_gldp <- function(x, y) {
  # Validate input packages
  check_gldp(x)
  check_gldp(y)

  # Check if the versions are the same
  if (version(x) != version(y)) {
    cli_warn(c(
      "!" = "The versions of {.pkg x} ({version(x)}) and {.pkg y} ({version(y)}) differ.",
      ">" = "This might cause merging to fail."
    ))
  }

  # Check for duplicate tag_ids
  common_tags <- intersect(tags(x)$tag_id, tags(y)$tag_id)
  if (length(common_tags) > 0) {
    cli_abort(c(
      "Duplicate {.field tag_id} detected: {col_red(paste(common_tags, collapse = ', '))}.",
      "x" = "Merging may fail due to duplicate tag IDs.",
      "i" = "Consider renaming or deleting one of the conflicting tag IDs."
    ))
  }

  # Merge related identifiers (DOIs, if available, will be added)
  relatedIdentifiers <- unique(c(x$relatedIdentifiers, y$relatedIdentifiers))
  add_related_id <- function(id, related_ids) {
    if (grepl("doi", id %||% "")) {
      new_related_id <- list(
        relationType = "isDerivedFrom",
        relatedIdentifier = id,
        resourceTypeGeneral = "Dataset",
        relatedIdentifierType = "DOI"
      )
      related_ids <- c(related_ids, list(new_related_id))
    }
    related_ids
  }
  relatedIdentifiers <- add_related_id(x$id, relatedIdentifiers)
  relatedIdentifiers <- add_related_id(y$id, relatedIdentifiers)

  # Combine metadata fields
  xy <- create_gldp(
    title = paste(x$title, y$title, sep = " / "), # Combine titles
    contributors = unique(c(x$contributors, y$contributors)), # Remove duplicates
    licenses = unique(c(x$licenses, y$licenses)), # Remove duplicates
    embargo = format(max(as.Date(x$embargo), as.Date(y$embargo)), "%Y-%m-%d"), # Latest embargo date
    id = NULL, # Remove ID
    description = paste(x$description, y$description, sep = "\n"), # Combine descriptions
    version = NULL, # Remove version
    relatedIdentifiers = relatedIdentifiers, # Merge related identifiers
    grants = unique(c(x$grants, y$grants)), # Remove duplicates
    keywords = unique(c(x$keywords, y$keywords)), # Remove duplicates
    created = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"), # Current timestamp
    bibliographicCitation = NULL # Remove bibliographic citation
  )

  # Merge resources from both packages
  res_x <- frictionless::resources(x)
  res_y <- frictionless::resources(y)
  res <- unique(c(res_x, res_y)) # Combine resource names without duplicates

  # Iterate over all resources and merge their data if available
  xy <- purrr::reduce(res, function(xy, r) {
    # Read resource data if it exists in x or y
    data_x <- if (r %in% res_x) frictionless::read_resource(x, resource_name = r) else NULL
    data_y <- if (r %in% res_y) frictionless::read_resource(y, resource_name = r) else NULL

    # Only add the resource if data is available in either x or y
    if (!is.null(data_x) | !is.null(data_y)) {
      combined_data <- bind_rows(data_x, data_y) # Combine data from x and y
      xy <- add_gldp_resource(
        package = xy,
        resource_name = r,
        data = combined_data,
        cast_type = TRUE # Ensure data types are correctly cast
      )
    }

    xy
  }, .init = xy)

  # Final update for any remaining metadata or properties
  xy <- update_gldp(xy)

  xy
}
