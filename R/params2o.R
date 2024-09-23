#' Generate `observations` from GeoPressureR parameters
#'
#' This function converts a list of parameters into a single `observations` tibble according to the
#' [GeoLocator Data Package specification
#' ](https://raphaelnussbaumer.com/GeoLocator-DP/core/observations/).
#'
#' @param params A list of GeoPressureR parameter objects. These parameters should have been
#' generated during the GeoPressure worflow. See [`GeoPressureR::param_create()`
#' ](https://raphaelnussbaumer.com/GeoPressureR/reference/param_create.html) for more information.
#'
#' @return A tibble with columns `tag_id`, `manufacturer`, `scientific_name`, `ring_number`,
#' `model`, and `firwmare` (if `soi_settings` is present).
#'
#' @export
params2o <- function(params) {
  params %>%
    purrr::map(\(param){
      o0 <- tibble::tibble(
        ring_number = NA_character_,
        tag_id = param$id,
        datetime = as.Date(NA),
        location_name = NA_character_,
        longitude = NA_real_,
        latitude = NA_real_,
        life_stage = "0",
        sex = "U",
        observation_comments = "Automatically computed with `params2o()`"
      )

      oe <- o0 %>% mutate(
        datetime = param$tag_create$crop_start,
        observation_type = "equipment"
      )

      or <- o0 %>% mutate(
        datetime = param$tag_create$crop_end,
        observation_type = "retrieval"
      )

      if ("known" %in% names(param$tag_set_map)) {
        known <- param$tag_set_map$known

        id <- known$stap_id == 1
        if (any(id)) {
          oe <- oe %>% mutate(
            longitude = known$known_lon[id],
            latitude = known$known_lat[id],
            observation_comments = "Automatically computed from `known$stap_id==1` with `params2o()`"
          )
        }

        id <- known$stap_id == -1
        if (any(id)) {
          or <- or %>% mutate(
            longitude = known$known_lon[id],
            latitude = known$known_lat[id],
            observation_comments = "Automatically computed from `known$stap_id==-1` with `params2o()`"
          )
        }
      }

      o <- bind_rows(oe, or)

      return(o)
    }) %>%
    purrr::list_rbind()
}
