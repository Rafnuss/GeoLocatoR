#' Print a GeoLocator Data Package
#'
#' Prints a human-readable summary of a GeoLocator Data Package, as an
#' extension of [print.datapackage()].
#'
#' @param x GeoLocator Data Package object, as returned by `read_geolocatordp()`.
#' @param ... Further arguments, they are ignored by this function.
#'
#' @return [print()] with a summary of the GeoLocator Data Package object.
#' @family print functions
#' @export
print.geolocatordp <- function(x, ...) {
  # check_geolocatordp() not necessary: print only triggered for geolocatordp object

  check_gldp(x)

  cli::cli_h3("A GeoLocator Data Package ({version(x)})")

  cli::cli_bullets(c("*" = "{.field title}: {.val {x$title}}"))

  contributors <- sapply(x$contributors, \(x) {
    str <- x$title
    # if (!is.null(x$givenName)) {
    #  str <- paste0(str, " ", x$givenName)
    # }
    # if (!is.null(x$familyName)) {
    #  str <- paste0(str, " ", x$familyName)
    # }
    if (!is.null(x$email)) {
      str <- paste0(str, " ({.email ", x$email, "})")
    }
    if (!is.null(x$roles)) {
      str <- paste0(str, " (", paste(x$roles, collapse = ", "), ")")
    }
    if (!is.null(x$path)) {
      str <- paste0(str, " - {.url ", x$path, "}")
    }
    return(str)
  })
  cli::cli_bullets(c("*" = "{.field contributors}:"))
  for (ctr in contributors) {
    cli::cli_bullets(c(" " = ctr))
  }

  embargo_date <- as.POSIXct(x$embargo, format = "%Y-%m-%d", tz = "UTC") # nolint
  cli::cli_bullets(c("*" = "{.field embargo}: {.val {embargo_date}}"))

  licenses <- sapply(x$licenses, \(x) {
    if (!is.null(x$title)) {
      str <- x$title
      if (!is.null(x$name)) {
        str <- paste0(str, " (", x$name, ")")
      }
    } else {
      str <- x$name
    }
    if (!is.null(x$path)) {
      str <- paste0(str, " - {.url ", x$path, "}")
    }
    return(str)
  })
  cli::cli_bullets(c("*" = paste0("{.field licenses}: ", licenses)))

  if (!is.null(x$id)) {
    cli::cli_bullets(c("*" = "{.field id}: {.url {x$id}}"))
  }

  if (!is.null(x$description)) {
    cli::cli_bullets(c("*" = "{.field description}: {.val {x$description}}"))
  }

  if (!is.null(x$version)) {
    cli::cli_bullets(c("*" = "{.field version}: {.val {x$version}}"))
  }

  if (!is.null(x$relatedIdentifiers)) {
    ris <- sapply(x$relatedIdentifiers, \(x) {
      if (x$relatedIdentifierType == "doi" && !grepl("^https?://", x$relatedIdentifier)) {
        x$relatedIdentifier <- paste0("https://doi.org/", x$relatedIdentifier)
      }
      paste0(x$relationType, " {.url ", x$relatedIdentifier, "}")
    })
    cli::cli_bullets(c("*" = "{.field relatedIdentifiers}:"))
    for (ri in ris) {
      cli::cli_bullets(c(" " = ri))
    }
  }

  if (!is.null(x$grants)) {
    cli::cli_bullets(c("*" = "{.field grants}: {.val {x$grants}}"))
  }

  if (!is.null(x$keywords)) {
    cli::cli_bullets(c("*" = "{.field keywords}: {.val {x$keywords}}"))
  }

  created_datetime <- as.POSIXct(x$created, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") # nolint
  cli::cli_bullets(c("*" = "{.field created}: {.val {created_datetime}}"))

  if (!is.null(x$bibliographicCitation)) {
    cli::cli_bullets(c("*" = "{.field bibliographicCitation}: {.val {x$bibliographicCitation}}"))
  }

  cli::cli_bullets(c("*" = "{.field spatial}: {.val {x$spatial}}"))
  cli::cli_bullets(
    c("*" = "{.field temporal}: {.val {x$temporal$start}} to {.val {x$temporal$end}}")
  )
  cli::cli_bullets(c("*" = "{.field taxonomic}: {.val {x$taxonomic}}"))

  if (!is.null(x$referenceLocation)) {
    cli::cli_bullets(c("*" = "{.field referenceLocation}: {.val {x$referenceLocation}}"))
  }

  cli::cli_bullets(c("*" = "{.field numberTags}:"))
  for (nt in names(x$numberTags)) {
    if (x$numberTags[[nt]] > 0) {
      cli::cli_bullets(c(" " = "{.strong {nt}}: {.val {x$numberTags[[nt]]}}"))
    }
  }


  # cli::cli_bullets(c("*" = "{.field schema}: {.url {x$`$schema`}}"))

  cli::cli_h3("{length(x$resources)} resource{?s}{?./:/:}")
  if (length(x$resources) > 0) {
    purrr::walk(x$resources, \(x) {
      cat_bullet(format_inline("{x$name}"))
    })
  }

  # Provide help
  cat_line(
    format_inline(
      "Use {.fun unclass} to print the Geolocator Data Package as a list."
    ),
    col = "silver"
  )

  invisible(x)
}


bullets <- function(pkg, x) {
  val <- pkg[[x]]
  if (!is.null(val)) {
    if (is.data.frame(val)) {
      cli::cli_bullets(c("*" = "{.field {x}}:"))
      cli::cat_print(val)
    } else if (is.list(val) && length(val) > 1) {
      cli::cli_bullets(c("*" = "{.field {x}}:"))
      for (n in names(val)) {
        cli::cli_bullets(c(" " = "{.field {n}}: {.val {val[[n]]}}"))
      }
    } else {
      cli::cli_bullets(c("*" = "{.field {x}}: {.val {val}}"))
    }
  }
}
