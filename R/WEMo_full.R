#' Run Full WEMo Workflow - Calculate Fetch, Interrogate Bathymetry, and Build Wind Waves
#'
#' This is the full workflow to do wave energy modeling using WEMo. It
#' calculates fetch, effective fetch, interrogates bathymetry, merges wind data,
#' builds waves along each fetch ray and then summarizes the wind driven waves
#' each `site_point`
#'
#'@details
#' First fetch is calculated for each `site_point` in each direction specified by `directions`. The function performs the following operations:
#' \enumerate{
#'   \item Ensures coordinate reference systems (CRS) match between input layers
#'   \item Filters out sites located on land
#'   \item For each site and direction, creates a ray extending to max_fetch distance
#'   \item Identifies intersections between rays and shoreline polygons
#'   \item Calculates distances from sites to nearest intersection points
#'   \item Returns fetch rays as linestring geometries with associated measurements
#' }
#' First, the function builds a wind-wave over input bathymetry and returns an
#' estimate of final wave height and related wave characteristics (e.g., wave
#' energy index, wave period, wave number, celerity).
#'
#' Then, the function summarizes the waves from every direction for each site
#' and computes the summary statistic `RWE`.
#'
#'
#' @param site_points An `sf` or `SpatVector` point object representing the location(s)
#'   for wave exposure analysis. Will be coerced to `sf`.
#' @param shoreline An `sf` or `SpatVector` polygon representing land or
#'   shoreline features.
#' @param bathy A `SpatRaster` containing bathymetric data (in meters, negative
#'   or positive depending on `depths_or_elev`).
#' @param wind_data A data frame with wind direction (`direction`), speed
#'   (`speed`), and proporation wind blows from the direction (`proportion`)
#' @param directions Numeric vector of directions (in degrees) to cast fetch
#'   lines.
#' @param max_fetch Maximum fetch length (in map units, typically meters).
#'   Default is 1000.
#' @param sample_dist Distance (in map units) between sample points along each
#'   fetch line. Default is 5.
#' @param water_level Numeric. The water level relative to bathymetry (in
#'   meters).
#' @param depths_or_elev Character, either `'depths'` or `'elev'`. Specifies
#'   whether bathymetry is interpreted as water depth (positive down) or
#'   elevation (negative down). If `'elev'`, values are flipped to represent
#'   depth.
#' @param extra_at_start Logical. Whether to add an extra bathymetry sample at
#'   the fetch origin. Default is `TRUE`.
#'
#'
#' @return A list of two elements:
#' \describe{
#'   \item{`wemo_details`}{the original `fetch` object with the following stats about each wave arriving at the site from the direction of the fetch ray added:
#'     \itemize{
#'       \item `wave_height_final` – Final wave height at the end of the fetch (m)
#'       \item `WEI` – Wave Energy Index
#'       \item `wave_period` – Estimated wave period (s)
#'       \item `wave_number` – Average wave number (rad/m)
#'       \item `celerity_final` – Final wave phase speed (m/s)
#'       \item `nnumber_final` – Final group velocity coefficient
#'     }}
#'   \item{`wemo_output`}{sf object with all the variables from `site_point` in addition to the following:\itemize{
#'       \item `RWE` – Total relative wave exposure at the site
#'       \item `avg_wave_height` – Mean wave height across all fetch rays (m)
#'       \item `max_wave_height` – Maximum wave height across all fetch rays (m)
#'       \item `direction_of_max_wave` – Direction(s) of maximum wave height (Degrees from North)
#'     }}
#' }
#'
#' @seealso [find_fetch()], [prepare_WEMo_inputs()], [WEMo()]
#' @export
#'
wemo_full <- function(site_points,
         shoreline,
         bathy,
         wind_data,
         directions,
         max_fetch = 1000,
         sample_dist = 5,
         water_level,
         depths_or_elev = 'elev',
         extra_at_start = T) {

  wemo_inputs <- prepare_WEMo_inputs(
    site_points = site_points,
    bathy = bathy,
    shoreline = shoreline,
    wind_data = wind_data,
    directions = directions,
    max_fetch = max_fetch,
    sample_dist = sample_dist,
    water_level = water_level,
    depths_or_elev = depths_or_elev,
    extra_at_start =  extra_at_start
  )

  wemo_results <- WEMo(fetch = wemo_inputs)

  return(wemo_results)
}
