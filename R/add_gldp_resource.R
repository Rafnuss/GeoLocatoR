#' Add a Resource with Schema to a Package
#'
#' This function adds a resource to a package and ensures that the data conforms to the schema
#' defined for that resource. The schema is used to validate and potentially modify the data frame
#' before adding it to the package.
#'
#' @param package The package object to which the resource will be added.
#' @param resource_name A character string specifying the name of the resource. This name is used
#' to locate the schema file.
#' @param data A data frame containing the data to be added as a resource. The data frame will be
#' adjusted according to the schema.
#' @param cast_type A logical value indicating whether the data frame should be cast to the types
#' specified in the schema. Defaults to `FALSE`.
#' @inheritParams frictionless::add_resource
#'
#' @details
#' The schema for the resource is fetched from a JSON file located in a specified directory.
#' The function adjusts the data frame according to the schema's `fieldsMatch` property:
#' \itemize{
#'   \item \code{"equal"}: The data frame must have exactly the same fields as defined in the
#'   schema.
#'   \item \code{"subset"}: The data frame must have at least the fields defined in the schema,
#'   but may have additional fields. Fields not present in the schema are added to the schema.
#'   \item \code{"superset"}: The data frame may have fields not present in the schema, but must
#'   include all fields defined in the schema.
#'   \item \code{"partial"}: The data frame must have at least one field defined in the schema.
#' }
#'
#' @return The updated package object with the new resource added.
#'
#' @examples
#' \dontrun{
#' my_package <- some_package_function()
#' my_data <- data.frame(a = 1:5, b = letters[1:5])
#' updated_package <- add_gldp_resource(my_package, "my_resource", my_data)
#' }
#'
#' @export
add_gldp_resource <- function(package,
                              resource_name,
                              data,
                              cast_type = FALSE,
                              replace = FALSE,
                              delim = ",") {
  check_gldp_pkg(package)

  # Validate resource_name
  pkg_schema <- jsonlite::fromJSON(package$`$schema`,
                                   simplifyDataFrame = FALSE, simplifyVector = TRUE)
  possible_resources <-
    pkg_schema$allOf[[2]]$properties$resources$items$oneOf[[1]]$properties$name$enum

  if (!resource_name %in% possible_resources) {
    cli::cli_abort(c(
      "x {.val {resource_name}} is not a valid resource.",
      "i Possible resources are: {.val {possible_resources}}."
    ))
  }

  schema <- jsonlite::fromJSON(glue::glue(
    "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/{version(package)}/{resource_name}-table-schema.json"
  ), simplifyDataFrame = FALSE, simplifyVector = TRUE)

  # https://github.com/frictionlessdata/frictionless-r/issues/254

  schema_fields <- sapply(schema$fields, \(x) x$name)
  schema_type <- sapply(schema$fields, \(x) x$type)
  # schema_required <- sapply(schema$fields, \(x) x$constraints$required)
  # data_fields <- names(data)

  if (schema$fieldsMatch == "equal") {
    # The data source MUST have exactly the same fields as defined in the fields array.
    # Fields MUST be mapped by their names.
    # NOT SURE THE ORDER BY NAME IS CHECKED!!!
  }

  if (schema$fieldsMatch == "subset" || schema$fieldsMatch == "partial") {
    # The data source MUST have all the fields defined in the fields array, but MAY have more.
    # Fields MUST be mapped by their names.

    # Create a schema from the data (adding all possible field)
    schema_data <- frictionless::create_schema(data)

    # Add fields not existing in the initial schema
    for (f in schema_data$fields) {
      if (!(f$name %in% schema_fields)) {
        schema$fields <- append(schema$fields, list(f))
      }
    }

    # Update schema_fields
    schema_fields <- sapply(schema$fields, \(x) x$name)
  }

  if (schema$fieldsMatch == "superset" || schema$fieldsMatch == "partial") {
    # superset: The data source MUST only have fields defined in the fields array, but MAY have
    # fewer. Fields MUST be mapped by their names.

    for (i in seq_along(schema_fields)) {
      # Check if column already exists
      if (!(schema_fields[i] %in% names(data))) {
        na_type <- switch(schema_type[i],
          string = NA_character_,
          number = NA_real_,
          integer = NA_integer_,
          date = as.Date(NA), # NA_Date_,
          duration = NA_character_,
          NA
        )
        # Add the column with NA values of the specified type
        data <- data %>% mutate(!!schema_fields[i] := na_type)
      }
    }
  }

  if (schema$fieldsMatch == "partial") {
    # Partial: The data source MUST have at least one field defined in the fields array.
    # Fields MUST be mapped by their names.
    # NOT CHECKING FOR AT LEAST ONE FIELD
  }

  # Update schema_fields
  data <- data %>% select(all_of(schema_fields))

  if (cast_type) {
    data <- add_gldp_resource_cast(data, schema_fields, schema_type)
  }

  package <- frictionless::add_resource(
    package = package,
    resource_name = resource_name,
    data = data,
    schema = schema,
    replace = replace,
    delim = delim
  )

  return(package)
}

#' @noRd
add_gldp_resource_cast <- function(data, schema_fields, schema_types) {
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
      } else if (type == "bolean") {
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
  return(data)
}
