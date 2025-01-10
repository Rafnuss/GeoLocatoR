#' Validate a GeoLocator Data Package
#'
#' This function performs a comprehensive validation of a GeoLocator Data Package by checking the
#' package metadata, profile, and resources. The validation includes verifying that the package
#' conforms to the GeoLocator Data Package profile and that each resource adheres to its schema.
#'
#' If `quiet` is `TRUE`, the function suppresses the output of the `cli` package's messages.
#'
#' @param pkg An object of class `"geolocatordp"` representing the GeoLocator Data Package to be
#' validated.
#' @param quiet A logical indicating whether to suppress messages from the `cli` package. Defaults
#' to `FALSE`.
#'
#' @return A logical value indicating whether the package validation was successful (`TRUE`) or
#' failed (`FALSE`).
#'
#' @export
check_gldp <- function(pkg, quiet = FALSE) {
  check_gldp_pkg(pkg)

  if (quiet) {
    options(cli.default_handler = function(...) { })
  }

  checked <- check_gldp_profile(pkg)
  checked <- checked & check_gldp_resources(pkg)
  checked <- checked & check_gldp_coherence(pkg)
  checked <- checked & check_gldp_observations(observations(pkg))

  if (checked) {
    cli_alert_success("Package is valid.")
  } else {
    cli_alert_danger("Package validation failed.")
  }

  if (quiet) {
    options(cli.default_handler = NULL)
  }

  invisible(checked)
}

#' Validate the GeoLocator Data Package Metadata
#'
#' This function checks that the provided object is a valid GeoLocator Data Package and conforms to
#' the expected structure. It verifies that the object has the `"geolocatordp"` class and performs
#' a general package validation using the `frictionless` package.
#'
#' @param pkg An object of class `"geolocatordp"` to be validated.
#'
#' @return The validated GeoLocator Data Package object (invisible).
#'
#' @noRd
check_gldp_pkg <- function(pkg) {
  frictionless::check_package(pkg)

  if (!("geolocatordp" %in% class(pkg))) {
    cli_abort(
      c(
        "{.arg pkg} must be a GeoLocator Data Package object.",
        "x" = "{.arg pkg} is missing a {.val geolocatordp} class."
      ),
    )
  }
  invisible(pkg)
}

#' @noRd
check_gldp_profile <- function(pkg) {
  schema <- jsonlite::fromJSON(pkg$`$schema`, simplifyVector = FALSE)

  required <- unlist(schema$allOf[[2]]$required)
  properties <- schema$allOf[[2]]$properties

  cli_h2("Check GeoLocator DataPackage profile")
  checked <- check_gldp_object(pkg, required, properties)

  if (checked) {
    cli_alert_success("Package is consistent with the profile.")
  } else {
    cli_alert_danger("Package is not consistent with the profile.")
  }

  invisible(checked)
}

#' @noRd
check_gldp_resources <- function(pkg) {
  cli_h3("Check GeoLocator DataPackage Resources")

  checked <- TRUE

  for (i in seq_along(pkg$resources)) {
    resource <- pkg$resources[[i]]

    if (resource$profile == "tabular-data-resource" &&
      (resource$name %in% c(
        "tags", "observations", "measurements", "staps", "twilights", "paths",
        "edges", "pressurepaths"
      ))) {
      schema <- jsonlite::fromJSON(
        glue::glue(
          "https://raw.githubusercontent.com/Rafnuss/GeoLocator-DP/refs/heads/main/{resource$name}",
          "-table-schema.json"
        ),
        simplifyVector = FALSE
      )
      checked <- checked & check_gldp_table(resource$data, schema)
    } else {
      cli_h2("Check GeoLocator DataPackage Resources {.field {resource$name}}")
      cli_alert_warning("Could not check {.field {resource$name}}")
      checked <- FALSE
    }
  }

  if (checked) {
    cli_alert_success("Package's ressources are valid.")
  } else {
    cli_alert_danger("Package's ressources validation failed.")
  }

  invisible(checked)
}

#' @noRd
check_gldp_table <- function(data, schema) {
  cli_h2("Check GeoLocator DataPackage Resources {.field {schema$name}}")

  if (is.null(data)) {
    cli_alert_danger("data is not available.")
    return(FALSE)
  }

  schema_fields <- stats::setNames(
    schema$fields,
    sapply(schema$fields, \(x) x$name)
  )

  checked <- check_gldp_fields_match(names(schema_fields), names(data), schema$fieldsMatch[[1]])

  fields <- intersect(names(schema_fields), names(data))

  for (field in fields) {
    prop <- schema_fields[[field]]
    if (!is.null(prop$constraints)) {
      prop <- c(prop, prop$constraints)
      prop$constraints <- NULL # Remove the 'constraints' field after merging
    }

    checked <- checked & check_gldp_item(data[[field]], prop, glue::glue("{schema$name}${field}"))
  }

  if (checked) {
    cli_alert_success("Table {.field {schema$name}} is consistent with the schema.")
  } else {
    cli_alert_danger("Table {.field {schema$name}} is not consistent with the schema.")
  }

  invisible(checked)
}

#' @noRd
check_gldp_object <- function(obj, required, properties, name = "") {
  name <- glue::glue("{name}{ifelse(name=='','','$')}")

  checked <- TRUE
  for (field in names(obj)) {
    if (field %in% names(properties)) {
      checked <- checked &
        check_gldp_item(obj[[field]], properties[[field]], glue::glue("{name}{field}"))

      if (properties[[field]]$type == "object") {
        checked <- checked & check_gldp_object(
          obj[[field]], properties[[field]]$required, properties[[field]]$properties, field
        )
      }

      if (properties[[field]]$type == "array") {
        for (i in seq_len(length(obj[[field]]))) {
          checked <- checked &
            check_gldp_item(
              obj[[field]][[i]],
              properties[[field]]$items,
              glue::glue("{name}{field}[[{i}]]")
            )
        }
      }
    } else {
      if (!(field %in% c("directory"))) {
        cli_alert_warning("{.field {field}} does not exist in schema.")
      }
    }
  }

  purrr::walk(required, function(r) {
    if (is.null(obj[[r]])) {
      cli_alert_danger("{.field {name}{r}} is required but missing.")
      checked <<- FALSE
    }
  })
  return(checked)
}

#' @noRd
check_gldp_item <- function(item, prop, field) {
  checked <- TRUE

  # checked type
  checked <- checked && check_type(item, prop$type, field)

  # checked format
  checked <- checked && check_format(item, prop$format, field)


  # Check if 'required' property exists and is TRUE
  required <- FALSE
  if (!is.null(prop$required) && is.logical(prop$required) && prop$required) {
    required <- TRUE
    if (is.null(item)) {
      cli_alert_danger("{.field {field}} is required but missing.")
      checked <- FALSE
    } else {
      nn <- is.na(item)

      # If any NULL or NA items exist, report and set checked to FALSE
      if (any(nn)) {
        cli_alert_danger("{.field {field}} is required but {sum(nn)} item{?s} are {.val NA}")
        checked <- FALSE
      }
    }
  }

  # Unique
  if (!is.null(prop$unique) && prop$unique) {
    # Check for duplicates in 'item'
    duplicates <- duplicated(item) & !is.na(item)

    if (any(duplicates)) {
      cli_alert_danger("{.field {field}} must be unique but {sum(duplicates)} duplicate{?s} found.")
      checked <- FALSE
    }
  }

  # Check if 'minLength' property exists and is specified
  if (!is.null(prop$minLength)) {
    lengths <- nchar(as.character(item)) # Convert to character for length check
    too_short <- lengths < prop$minLength

    if (any(too_short)) {
      cli_alert_danger("{.field {field}} has {sum(too_short)} item{?s} shorter than the minimum
                       length of {prop$minLength}.")
      checked <- FALSE
    }
  }

  # Check if 'maxLength' property exists and is specified
  if (!is.null(prop$maxLength)) {
    lengths <- nchar(as.character(item)) # Convert to character for length check
    too_long <- lengths > prop$maxLength

    if (any(too_long)) {
      cli_alert_danger("{.field {field}} has {sum(too_long)} item{?s} longer than the maximum
                       length of {prop$maxLength}.")
      checked <- FALSE
    }
  }

  # Check if 'minItems' property exists and is specified
  if (!is.null(prop$minItems)) {
    if (length(item) < prop$minItems) {
      cli_alert_danger("{.field {field}} must contain at least {prop$minItems} item{?s}, but only
                       {length(item)} item{?s} provided.")
      checked <- FALSE
    }
  }

  # Check if 'maxItems' property exists and is specified
  if (!is.null(prop$maxItems)) {
    if (length(item) > prop$maxItems) {
      cli_alert_danger("{.field {field}} must contain no more than {prop$maxItems} item{?s}, but
                       {length(item)} item{?s} provided.")
      checked <- FALSE
    }
  }

  # Check if 'minimum' property exists and is specified (assuming numeric)
  if (!is.null(prop$minimum)) {
    below_min <- item < prop$minimum

    if (any(below_min, na.rm = TRUE)) {
      cli_alert_danger("{.field {field}} has {sum(below_min, na.rm = TRUE)} item{?s} below the
                       minimum value of {prop$minimum}.")
      checked <- FALSE
    }
  }

  # Check if 'maximum' property exists and is specified (assuming numeric)
  if (!is.null(prop$maximum)) {
    above_max <- item > prop$maximum

    if (any(above_max, na.rm = TRUE)) {
      cli_alert_danger("{.field {field}} has {sum(above_max, na.rm = TRUE)} item{?s} above the
                       maximum value of {prop$maximum}.")
      checked <- FALSE
    }
  }

  # Check if 'enum' property exists and is specified
  if (!is.null(prop$enum)) {
    # Check if each item is one of the allowed values in the enum
    if (required) {
      in_enum <- item %in% unlist(prop$enum)
    } else {
      # Allow NA if not required
      in_enum <- (item %in% unlist(prop$enum)) | is.na(item)
    }


    if (any(!in_enum)) {
      invalid_values <- item[!in_enum] # nolint
      cli_alert_danger(
        "{.field {field}} has {sum(!in_enum)} item{?s} that are not in the allowed values:
      {.val {paste(prop$enum, collapse = ', ')}}. Invalid value{?s}:
      {.val {paste(unique(invalid_values), collapse = ', ')}}"
      )
      checked <- FALSE
    }
  }

  # Check if 'pattern' property exists and is specified
  if (!is.null(prop$pattern)) {
    # Apply the regular expression to each item
    pattern_match <- grepl(prop$pattern, as.character(item[!is.na(item)]))

    # Identify items that do not match the pattern
    not_matching <- !pattern_match

    if (any(not_matching)) {
      cli_alert_danger("{.field {field}} has {sum(not_matching)} item{?s} that do not match the
                       required pattern: {.val {prop$pattern}}.")
      checked <- FALSE
    }
  }

  if (checked) {
    cli_alert_success("{.field {field}} is valid.")
  }


  return(checked)
}



#' @noRd
check_gldp_fields_match <- function(schema_fields, data_fields, fields_match) {
  if (is.null(fields_match)) {
    fields_match <- "exact"
  }

  checked <- TRUE
  # Perform the check based on the fieldsMatch value
  if (fields_match == "exact") {
    if (!identical(schema_fields, data_fields)) {
      cli_alert_danger("Exact match failed. Schema fields: {schema_fields}, Data fields:
                       {data_fields}")
      checked <- FALSE
    }
  } else if (fields_match == "equal") {
    if (!setequal(schema_fields, data_fields)) {
      cli_alert_danger("Equal match failed. Schema fields: {schema_fields}, Data fields:
                       {data_fields}")
      checked <- FALSE
    }
  } else if (fields_match == "subset") {
    if (!all(schema_fields %in% data_fields)) {
      missing_fields <- schema_fields[!schema_fields %in% data_fields] # nolint
      cli_alert_danger("Subset match failed. Missing fields: {missing_fields}")
      checked <- FALSE
    }
  } else if (fields_match == "superset") {
    if (!all(data_fields %in% schema_fields)) {
      extra_fields <- data_fields[!data_fields %in% schema_fields] # nolint
      cli_alert_danger("Superset match failed. Extra fields: {extra_fields}")
      checked <- FALSE
    }
  } else if (fields_match == "partial") {
    if (!any(schema_fields %in% data_fields)) {
      cli_alert_danger("Partial match failed. No schema fields found in data.")
      checked <- FALSE
    }
  }

  invisible(checked)
}


#' @noRd
check_format <- function(value, format, field) {
  if (is.null(format)) {
    return(TRUE)
  }

  format_map <- list(
    `date-time` = function(v) grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", v),
    date = function(v) grepl("^\\d{4}-\\d{2}-\\d{2}$", v),
    time = function(v) grepl("^\\d{2}:\\d{2}:\\d{2}$", v),
    `utc-millisec` = function(v) is.numeric(v),
    regex = function(v) {
      tryCatch(
        {
          grepl(v, "")
        },
        error = function(e) FALSE
      )
    },
    color = function(v) {
      grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$|^(red|blue|green|yellow|black|white)$", v)
    },
    style = function(v) grepl("^.+: .+;$", v),
    phone = function(v) grepl("^\\+?[0-9 .-]{7,}$", v),
    uri = function(v) grepl("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?", v),
    email = function(v) grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", v),
    `ip-address` = function(v) grepl("^\\d{1,3}(\\.\\d{1,3}){3}$", v),
    ipv6 = function(v) grepl("^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$", v),
    `host-name` = function(v) grepl("^[a-zA-Z0-9.-]+$", v),
    textarea = function(v) grepl("^(\\S.*(?:\\r?\\n\\S.*)*)$", v) | v == ""
  )


  checked <- TRUE
  if (format %in% names(format_map)) {
    format_func <- format_map[[format]]
    # checked the value using the corresponding validation function
    if (!format_func(value)) {
      cli_alert_danger("Format mismatch for {.field {field}}: Expected format {.val {format}},
                       but got {.val {value}}.")
      checked <- FALSE
    }
  } else {
    cli_alert_danger("Unknown expected format {.val {format}} for {.field {field}}.")
    checked <- FALSE
  }
  return(checked)
}

#' @noRd
check_type <- function(value, type, field) {
  if (is.null(type)) {
    return(TRUE)
  }

  type_map <- list(
    string = function(v) typeof(v) == "character",
    number = function(v) typeof(v) %in% c("integer", "double"),
    integer = function(v) typeof(v) == "integer",
    boolean = function(v) typeof(v) == "logical",
    object = function(v) typeof(v) == "list",
    array = function(v) typeof(v) %in% c("list", "character", "integer", "double"),
    null = function(v) is.null(v),
    datetime = function(v) {
      inherits(v, "POSIXct") ||
        grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", v)
    },
    date = function(v) inherits(v, "POSIXct") || grepl("^\\d{4}-\\d{2}-\\d{2}$", v),
    time = function(v) inherits(v, "POSIXct") || grepl("^\\d{2}:\\d{2}:\\d{2}$", v),
    year = function(v) inherits(v, "POSIXct") || grepl("^\\d{4}$", v),
    yearmonth = function(v) inherits(v, "POSIXct") || grepl("^\\d{4}-\\d{2}$", v),
    duration = function(v) {
      grepl("^P(?:\\d+Y)?(?:\\d+M)?(?:\\d+D)?(?:T(?:\\d+H)?(?:\\d+M)?(?:\\d+S)?)?$", v)
    },
    geopoint = function(v) grepl("^\\s*-?\\d+(\\.\\d+)?,\\s*-?\\d+(\\.\\d+)?\\s*$", v),
    geojson = function(v) {
      TRUE || grepl(glue::glue(
        "^\\s*\\{\\s*\"type\"\\s*:\\s*\"Feature\"\\s*,\\s*\"geometry\"\\s*:\\s*\\{\\s*\"type",
        "\"\\s*:\\s*\"Point\"\\s*,\\s*\"coordinates\"\\s*:\\s*\\[\\s*-?\\d+(\\.\\d+)?,",
        "\\s*-?\\d+(\\.\\d+)?\\s*\\]\\s*\\}\\s*\\}\\s*$"
      ), v)
    },
    any = function(v) TRUE # 'any' accepts everything
  )


  checked <- TRUE
  # Check if the expected type is in the type_map
  if (type %in% names(type_map)) {
    if (!type_map[[type]](value)) {
      cli_alert_danger("Type mismatch for {.field {field}}: Expected `{type}`, but got
                       `{typeof(value)}`.")
      checked <- FALSE
    }
  } else {
    cli_alert_danger("Unknown expected type `{type}` for {.field {field}}.")
    checked <- FALSE
  }

  return(checked)
}


#' @noRd
check_gldp_coherence <- function(pkg) {
  cli_h3("Check GeoLocator DataPackage Coherence")
  checked <- TRUE

  min_res_required <- c("tags", "observations", "measurements")
  res_missing <- min_res_required[!(min_res_required %in% sapply(pkg$resources, \(x) x$name))]
  if (length(res_missing) > 0) {
    cli_alert_warning("{.pkg pkg} is missing {.val {res_missing}}. \\
                      We could not check package coherence.")
    checked <- FALSE
    return(checked)
  }

  t <- tags(pkg)
  o <- observations(pkg)
  m <- measurements(pkg)

  # different scientific_name on the same ring_number
  t %>%
    filter(!is.na(.data$ring_number)) %>%
    group_by(.data$ring_number) %>%
    filter(n_distinct(.data$scientific_name) > 1) %>%
    distinct(.data$ring_number) %>%
    pull(.data$ring_number) %>%
    purrr::walk(~ cli_alert_danger(
      "Multiple scientific names used for ring_number {.strong {}}", .
    ))

  # Missing tag_id in tags while present measurements
  midmissing <- unique(m$tag_id[!(m$tag_id %in% t$tag_id)])
  if (length(midmissing) > 1) {
    cli_alert_danger(
      "{.field tags} is missing {.field tag_id}={.val {midmissing}} which are present in \\
    {.field measurements}."
    )
    cli_alert_info(
      "All {.field tag_id} presents in the resource {.field measurements} need to also be present \\
    in the resource {.field tags}."
    )
    checked <- FALSE
  }

  # Missing ring_number in observations while present in tags
  tringmissing <- unique(t$ring_number[!(t$ring_number %in% o$ring_number)])
  if (length(tringmissing) > 1) {
    cli_alert_danger(
      "{.field observations} is missing {.field ring_number}={.val {tringmissing}} which are \\
      present in {.field tags}."
    )
    cli_alert_info(
      "All {.field ring_number} present in the resource {.field tags} need tto also be present \\
    in the resource {.field observations}."
    )
    checked <- FALSE
  }

  # Observations
  # Check for combinations in 'o' that are not present in 't'
  invalid_combinations <- o %>%
    filter(!is.na(.data$tag_id)) %>%
    anti_join(t, by = c("tag_id", "ring_number"))
  if (nrow(invalid_combinations) > 0) {
    cli_alert_danger(
      "The following {.field tag_id} and {.field ring_number} combinations in \\
      {.field observation} are not present in {.field tags}:"
    )
    print(invalid_combinations)
    checked <- FALSE
  }

  if (checked) {
    cli_alert_success("Package is internally coherent.")
  } else {
    cli_alert_danger("Package is not coherent.")
  }

  invisible(checked)
}


#' @noRd
check_gldp_observations <- function(o) {
  cli_h3("Check Observations Coherence")
  checked <- TRUE

  o <- o %>%
    arrange(
      .data$ring_number, .data$datetime,
      factor(.data$observation_type,
        levels = c("capture", "retrieval", "equipment", "sighting", "other")
      )
    )

  # Check 1: tag_id is only associated with a single ring_number
  inconsistent_tag_ids <- o %>%
    filter(!is.na(.data$tag_id)) %>%
    group_by(.data$tag_id) %>%
    summarize(unique_ring_numbers = n_distinct(.data$ring_number)) %>%
    filter(.data$unique_ring_numbers > 1)

  if (nrow(inconsistent_tag_ids) > 0) {
    cli_alert_danger("{nrow(inconsistent_tag_ids)} tag_id(s) are associated with multiple \\
                     ring_numbers. Check: {inconsistent_tag_ids$tag_id}")
    checked <- FALSE
  }

  # Check 2: equipment or retrieval must have a tag_id
  missing_tag_id <- o %>%
    filter(.data$observation_type %in% c("equipment", "retrieval") & is.na(.data$tag_id))

  if (nrow(missing_tag_id) > 0) {
    error_tag <- unique(missing_tag_id$tag_id) # nolint
    cli_alert_danger("{length(error_tag)} equipment or retrieval observation{?s} {?is/are} \\
                          missing a tag_id. Check: {.field {error_tag}}")
    checked <- FALSE
  }

  # Check 3: equipment and retrieval can only have a device status present.
  obs_equi_retrieval_without_present <- o %>%
    filter(.data$observation_type %in% c("equipment", "retrieval") &
      .data$device_status != "present")

  if (nrow(obs_equi_retrieval_without_present) > 0) {
    error_tag <- unique(obs_equi_retrieval_without_present$tag_id) # nolint
    cli_alert_danger("{length(error_tag)} equipment or retrieval observation{?s} don't have a  \\
                    device status 'present'. Check: {.field {error_tag}}")
    checked <- FALSE
  }

  # Check 4: capture-missing and capture-present must have a tag_id
  missing_tag_id <- o %>%
    filter(.data$observation_type == "capture" &
      (.data$device_status %in% c("missing", "present")) &
      is.na(.data$tag_id))

  if (nrow(missing_tag_id) > 0) {
    error_ring_number <- unique(missing_tag_id$ring_number) # nolint
    cli_alert_danger(
      "{length(error_ring_number)} capture observation{?s} with a device status missing or \\
                    present {?is/are} missing a tag_id. Check: {.field {error_ring_number}}"
    )
    checked <- FALSE
  }

  # Check 5: No second tag_id attached without a prior retrieval (or capture with missing.)
  multiple_tags_without_retrieval <- o %>%
    group_by(.data$ring_number) %>%
    filter((.data$observation_type %in% c("retrieval", "equipment")) |
      (.data$observation_type == "capture" & .data$device_status == "missing")) %>%
    filter((lag(.data$tag_id) != .data$tag_id) & !(lag(.data$observation_type) == "retrieval" |
      lag(.data$device_status) == "missing"))

  if (nrow(multiple_tags_without_retrieval) > 0) {
    error_tag <- unique(multiple_tags_without_retrieval$tag_id) # nolint
    cli_alert_danger(
      "{length(error_tag)} instance{?s} where a second tag_id  is attached without a prior \\
    retrieval or capture-missing. Check: {.field {error_tag}}"
    )
    checked <- FALSE
  }

  # Check 6: A tag_id must follow an equipment event
  tag_without_equipment <- o %>%
    group_by(.data$ring_number, .data$tag_id) %>%
    filter(!is.na(.data$tag_id)) %>%
    filter(!any(.data$observation_type == "equipment"))

  if (nrow(tag_without_equipment) > 0) {
    error_tag <- unique(tag_without_equipment$tag_id) # nolint
    cli_alert_danger(
      "{length(error_tag)} tag_id{?s} {?was/were} recorded without a preceding equipment event. \\
      Check: {.field {error_tag}}"
    )
    checked <- FALSE
  }

  # Check 8: duplicate
  duplicate_observations <- o %>%
    group_by(.data$ring_number, .data$datetime, .data$observation_type) %>%
    filter(n() > 1)

  if (nrow(duplicate_observations) > 0) {
    error_tag <- unique(duplicate_observations$tag_id) # nolint
    cli_alert_danger(
      "{length(error_tag)} duplicate observations found. Check: {.field {error_tag}}"
    )
    checked <- FALSE
  }

  # Check 9: Invalid transition
  invalid_transitions <- o %>%
    group_by(.data$ring_number) %>%
    filter(.data$device_status != "unknown") %>%
    mutate(
      previous_status = lag(.data$device_status),
      previous_type = lag(.data$observation_type)
    ) %>%
    filter(
      (.data$device_status == "missing" & is.na(.data$previous_status)) |
        (.data$device_status == "missing" & .data$previous_status == "none") |
        (.data$device_status == "none" & .data$previous_status == "present" &
          .data$previous_type != "retrieval") |
        (.data$device_status == "present" & .data$previous_status == "none" &
          .data$observation_type != "equipment")
    )

  if (nrow(invalid_transitions) > 0) {
    error_ring_number <- unique(invalid_transitions$ring_number)
    cli_alert_danger(
      "{length(error_ring_number)} invalid device_status transitions found. \\
      Check: {.field {error_ring_number}}"
    )
    checked <- FALSE
  }

  if (checked) {
    cli_alert_success("{.field observations} table is coherent.")
  } else {
    cli_alert_danger("{.field observations} is not coherent.")
  }

  invisible(checked)
}
