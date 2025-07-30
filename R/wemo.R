#' Run the WEMo Wave Energy Model
#'
#' @description `wemo()` Combines fetch, bathymetry, and wind data to estimate
#'   wave height and energy metrics at each site using WEMo (Wave Energy Model)
#'   from wind-waves.
#'
#'   First, the function builds a wind-wave over input bathymetry and returns an
#'   estimate of final wave height and related wave characteristics (e.g., wave
#'   energy index, wave period, wave number, celerity).
#'
#'   Then, the function summarizes the waves from every direction for each site
#'   and computes the summary statistic `RWE`.
#'
#' @param fetch A data frame or `sf` object containing fetch geometry and
#'   required variables: `efetch`, `depths`, `distances`, `speed`, `proportion`.
#'   Expects the output from [`prepare_wemo_inputs()`]. Each row
#'   corresponds to a fetch ray.
#'
#' @return A list of two elements:
#' \describe{
#'   \item{`wemo_details`}{the original `fetch` object with the following stats about each wave arriving at the site from the direction of the fetch ray added:
#'     \itemize{
#'       \item `wave_height_final` - Final wave height at the end of the fetch (m)
#'       \item `WEI` - Wave Energy Index
#'       \item `wave_period` - Estimated wave period (s)
#'       \item `wave_number` - Average wave number (rad/m)
#'       \item `celerity_final` - Final wave phase speed (m/s)
#'       \item `nnumber_final` - Final group velocity coefficient
#'     }}
#'   \item{`wemo_final`}{sf object with all the variables from `site_point` in addition to the following:\itemize{
#'       \item `RWE` - Total relative wave exposure at the site
#'       \item `avg_wave_height` - Mean wave height across all fetch rays (m)
#'       \item `max_wave_height` - Maximum wave height across all fetch rays (m)
#'       \item `direction_of_max_wave` - Direction(s) of maximum wave height (Degrees from North)
#'     }}
#' }
#'
#' @details This function builds and propagates waves arriving at the site from
#'   each fetch ray segment-by-segment, adjusting for shoaling, wave breaking,
#'   and changing depth. Deep and shallow water regimes are handled using
#'   different formulas. The function uses a Newton-Raphson method to compute
#'   wave number. The values in the `wemo_final` return are calculated from
#'   summarizing (mean and max wave height) of all fetch rays. Relative Wave
#'   Energy (`RWE`) is calculated by multiplying the `REI` from each wave by the
#'   proportion of time that wind blows from the direction of the ray each ray
#'   and summing for all fetch rays
#'
#'
#' @seealso [`prepare_wemo_inputs()`]
#' @export
#'
#' @examples
#' \dontrun{
#' # Example assumes fetch object includes efetch, depths, distances, speed, and direction
#' result <- wemo(fetch = fetch, site_points = site_pts)
#' details <- result$wemo_details
#' summary <- result$wemo_output
#' }
wemo <- function(fetch){

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

  vertices <- sf::st_coordinates(fetch)
  # Extract first point of each LINESTRING

  start_points <- unique(vertices[seq(1, nrow(vertices), by = 2), c("X", "Y")]) %>% as.data.frame()

  start_points$site = unique(fetch$site)

  # Convert to POINT geometries
  start_points <- sf::st_as_sf(as.data.frame(start_points), coords = c("X", "Y"), crs = sf::st_crs(fetch))


  wemo_final <- wemo_details %>%
    dplyr::mutate(RWE = .data$WEI * .data$proportion/100,
                  site_name = as.character(.data$site)) %>%
    dplyr::group_by(.data$site) %>%
    dplyr::reframe(
      RWE = sum(.data$RWE, na.rm = T),
      avg_wave_height = mean(.data$wave_height_final, na.rm = T),
      max_wave_height = max(.data$wave_height_final, na.rm = T),
      direction_of_max_wave = paste(list(.data$direction[which(.data$wave_height_final == .data$max_wave_height)])),
      avg_fetch = mean(.data$fetch, na.rm = T),
      max_fetch = max(.data$fetch, na.rm = T),
      avg_efetch = mean(.data$efetch, na.rm = T),
      max_efetch = max(.data$efetch, na.rm = T)
    ) %>%
    dplyr::right_join(start_points, by = dplyr::join_by('site')) %>%
    sf::st_as_sf()

  return(list(
    wemo_details = wemo_details,
    wemo_final = wemo_final
  ))

}
