#' Generate `tags` from GeoPressureR parameters
#'
#' This function converts a list of parameters into a single `tags` tibble according to the
#' [GeoLocator Data Package specification](https://raphaelnussbaumer.com/GeoLocator-DP/core/tags/).
#'
#' @param params A list of GeoPressureR parameter objects. These parameters should have been
#' generated during the GeoPressure workflow. See [`GeoPressureR::param_create()`
#' ](https://raphaelnussbaumer.com/GeoPressureR/reference/param_create.html) for more information.
#'
#' @return A tibble with columns `tag_id`, `manufacturer`, `scientific_name`, `ring_number`,
#' `model`, and `firmware` (if `soi_settings` is present).
#'
#' @export
params2t <- function(params) {
  params %>%
    purrr::map(\(param) {
      t <- tibble::tibble(
        tag_id = param$id,
        manufacturer = param$tag_create$manufacturer,
        scientific_name = param$bird_create$scientific_name,
        ring_number = NA_character_
      )

      t$manufacturer[t$manufacturer == "soi"] <- "Swiss Ornithological Institue"
      t$manufacturer[t$manufacturer == "migratetech"] <- "Migrate Technology"
      t$manufacturer[t$manufacturer == "lund"] <- "Lund CAnMove"

      if ("soi_settings" %in% names(param)) {
        s <- param$soi_settings
        t <- t %>%
          mutate(
            model = s$`HW Version`,
            firmware = s$`FW Version`
          )
      }

      if ("migratec_model" %in% names(param)) {
        t <- t %>%
          mutate(
            model = param$migratec_model
          )
      }

      t
    }) %>%
    purrr::list_rbind()
}
