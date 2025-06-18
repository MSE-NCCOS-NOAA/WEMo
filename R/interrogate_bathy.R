#' Interrogate Bathymetry Along Fetch Rays
#'
#' Extract bathymetry data along multiple fetch rays and return updated input dataset.
#'
#' @description
#' This function processes multiple fetch rays, extracting bathymetry values along each
#' ray and storing the results as list columns in the input spatial dataframe.
#'
#' @param fetch A spatial dataframe (sf) containing fetch ray geometries
#' @param bathy_raster A SpatRaster object containing bathymetry data
#' @param sample_dist Numeric. The distance between sampling points along each ray (default: 10)
#' @param depths_or_elev Character string denoting if `bathy_raster` stores depths or elevation values. Defaults to `'elev'`.
#'   Must be either `'depths'` or `'elev'`.
#'   - `'depths'`: bathymetry values are depths (more positive values are deeper). `bathy_raster` values are added with `water_level` directly.
#'   - `'elev'`: (Default) bathymetry values are elevations (more positive values are shallower/above water). `bathy_rater` values are multiplied by -1 before adding `water_level`.
#' @param water_level the water level that you want to calculate depths for
#' @param extra_at_start Logical. If TRUE (default), when ray length doesn't
#'    divide evenly by sample_dist places extra point at the start. If FALSE,
#'    places extra point at the end.
#'
#' @return A spatial dataframe with added list columns:
#'   \item{bathy}{List column containing bathymetry values for each fetch ray}
#'   \item{distances}{List column containing distance values for each fetch ray}
#'
#' @details
#' This function processes each row of the input fetch dataframe, extracting bathymetry
#' values along the corresponding geometry. Results are stored as list columns, allowing
#' each row to contain vectors of different lengths.
#'
#' @examples
#' \dontrun{
#' # Process all fetch rays with 10m sampling
#' fetch_with_bathy <- interrogate_bathy(fetch_rays, bathy_raster, sample_dist = 10)
#' }
#' @export
interrogate_bathy <- function(fetch, bathy_raster, sample_dist = 10, depths_or_elev = "elev", water_level = 0, extra_at_start = T){
  fetch_with_bathy <-
    lapply(seq_along(fetch$geometry), function(i){
      fetch_ray <- fetch[i, ]
      extracted_bathy <- extract_bathy_along_fetch(bathy_raster, fetch_ray, sample_dist = sample_dist)

      if(depths_or_elev == 'depths') {
        depths <- extracted_bathy[["bathy"]] + water_level
      } else if(depths_or_elev == 'elev') {
        depths <- -1 * extracted_bathy[["bathy"]] + water_level
      }

      tibble::tibble(
        geometry = fetch_ray$geometry,
        bathy = list(extracted_bathy[["bathy"]]),
        distances = list(extracted_bathy[["distances"]]),
        depths = list(depths)
      ) %>%
        dplyr::bind_cols(sf::st_drop_geometry(fetch_ray), .)
    }) %>%
    dplyr::bind_rows()
  return(fetch_with_bathy)
}
