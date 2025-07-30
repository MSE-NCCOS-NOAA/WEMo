#' Prepare Input Data for WEMo Model
#'
#' This function chains together the core components needed to prepare a fetch
#' dataset for wave energy modeling using the WEMo framework. It calculates
#' fetch lines, effective fetch, interrogates bathymetry, and merges wind data,
#' returning a spatial object ready for wave modeling.
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
#' @return An `sf` object containing the fetch lines with columns:
#' \describe{
#'   \item{efetch}{Effective fetch distance (in meters)}
#'   \item{depths}{List-column of depths along each fetch line}
#'   \item{distances}{List-column of distances between sample points}
#'   \item{direction}{Direction of the fetch line}
#'   \item{speed}{Wind speed (from `wind_data`) corresponding to direction}
#' }
#'
#' @export
#'
prepare_wemo_inputs <- function(site_points,
                                shoreline,
                                bathy,
                                wind_data,
                                directions,
                                max_fetch = 10000,
                                sample_dist = 5,
                                water_level,
                                depths_or_elev = 'elev',
                                extra_at_start = T) {

  depths_or_elev <- tolower(depths_or_elev)

  # calculate fetch
  fetch <- find_fetch(
    site_points = site_points,
    polygon_layer = shoreline,
    directions = directions,
    max_fetch = max_fetch
  )

  # calculate effective fetch
  e_fetch <- effective_fetch(fetch = fetch)

  # interrogate the bathy and get list of depths and distances for each depth point
  e_fetch_with_bathy <- interrogate_bathy(
    fetch = e_fetch,
    bathy_raster = bathy,
    sample_dist = sample_dist,
    water_level = water_level,
    depths_or_elev = depths_or_elev,
    extra_at_start = extra_at_start
  )

  e_fetch_with_bathy_wind <- dplyr::left_join(e_fetch_with_bathy, wind_data, by = dplyr::join_by('direction'))

  e_fetch_with_bathy_wind <- sf::st_as_sf(e_fetch_with_bathy_wind)

  return(e_fetch_with_bathy_wind)
}
