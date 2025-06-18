#' Run the WEMo Wave Energy Model
#'
#' Combines fetch, bathymetry, and wind data to estimate wave height and energy metrics
#' at each site using the WEMo (Wave Energy Model) approach. This function wraps
#' several core components: calculating wave height via `[build_wind_wave()]` and aggregating
#' results for each site.
#'
#' @param fetch A data frame or `sf` object containing fetch geometry and required variables:
#' `efetch`, `depths`, `distances`, `speed`, `proportion`, and `site`.
#' Each row corresponds to a fetch ray.
#' @param site_points An `sf` object of site points to join results with.
#'
#' @return A list of two elements:
#' \describe{
#'   \item{`wemo_details`}{a data frame of detailed wave outputs (one per fetch ray).}
#'   \item{`wemo_output`}{a summarized data frame of wave energy estimates for each site.}
#' }
#'
#' @details
#' This function loops over each fetch ray, estimates wave height and related wave
#' characteristics using `[build_wind_wave()]`, calculates the relative wave energy
#' (RWE) using the wind direction proportion, and returns both detailed and summarized
#' results.
#'
#' @seealso [build_wind_wave()]
#'
#' @export
#'
WEMo <- function(fetch, site_points){

  wemo_details <- lapply(seq_along(fetch$geometry), function(i){
    dplyr::bind_cols(
      fetch[i,],
      build_wind_wave(
        fetch = fetch$efetch[[i]],
        depths = fetch$depths[[i]],
        distances = fetch$distances[[i]],
        wind_speed = fetch$speed[[i]]
      )
    )
  }) %>%
    dplyr::bind_rows()

  wemo_output <- wemo_details %>%
    dplyr::mutate(RWE = .data$WEI * .data$proportion/100,
                  site_name = as.character(.data$site)) %>%
    dplyr::group_by(.data$site) %>%
    dplyr::reframe(
      RWE = sum(.data$RWE, na.rm = T),
      avg_wave_height = mean(.data$wave_height_final, na.rm = T),
      max_wave_height = max(.data$wave_height_final, na.rm = T),
      direction_of_max_wave = paste(list(.data$direction[which(.data$wave_height_final == .data$max_wave_height)]))
    ) %>%
    dplyr::right_join(site_points)

  return(list(
    wemo_details,
    wemo_output
  ))

}
