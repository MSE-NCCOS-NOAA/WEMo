#' Interrogate Bathymetry Along Fetch Rays
#'
#' Extract bathymetry data along multiple fetch rays and return updated input
#' dataset.
#'
#' @description This function processes multiple fetch rays, extracting
#' bathymetry values along each ray and storing the results as list columns in
#' the input spatial dataframe.
#'
#' @param fetch A spatial dataframe (sf) containing fetch ray geometries
#' @param bathy_raster A SpatRaster object containing bathymetry data
#' @param sample_dist Numeric. The distance between sampling points along each
#'   ray (default: 10)
#' @param depths_or_elev Character string denoting if `bathy_raster` stores
#'   depths or elevation values. Defaults to `'elev'`. Must be either `'depths'`
#'   or `'elev'`.
#'   - `'depths'`: bathymetry values are depths (more positive values are deeper). `bathy_raster` values are added with `water_level` directly.
#'   - `'elev'`: (Default) bathymetry values are elevations (more positive values are shallower/above water). `bathy_rater` values are multiplied by -1 before adding `water_level`.
#' @param water_level the water level that you want to calculate depths for
#'
#' @return A spatial dataframe with added list columns: \item{bathy}{List column
#'   containing bathymetry values for each fetch ray} \item{distances}{List
#'   column containing distance values for each fetch ray}
#'
#' @details This function processes each row of the input fetch dataframe,
#' extracting bathymetry values along the corresponding geometry. Results are
#' stored as list columns, allowing each row to contain vectors of different
#' lengths.
#'
#' @examples
#' \dontrun{
#' # Process all fetch rays with 10m sampling
#' fetch_with_bathy <- interrogate_bathy(fetch_rays, bathy_raster, sample_dist = 10)
#' }
#'
#' @keywords internal
interrogate_bathy <- function(fetch,
                              bathy_raster,
                              sample_dist = 10,
                              depths_or_elev = "elev",
                              water_level = 0) {
  depths_or_elev <- tolower(depths_or_elev)

  fetch_with_bathy <-
    lapply(seq_along(fetch$geometry), function(i){
      fetch_ray <- fetch[i, ]

      extracted_bathy <- extract_bathy_along_fetch(
        bathy_raster,
        fetch_ray,
        sample_dist = sample_dist
      )

      if(depths_or_elev == 'depths') {
        depths <- extracted_bathy[["bathy"]] + water_level
      } else if(depths_or_elev == 'elev') {
        depths <- -1 * extracted_bathy[["bathy"]] + water_level
      }

      # if (any(depths < 0, na.rm = TRUE)) {
      #   warning("Some depths are negative at site '", fetch_ray$site,
      #           "'. Consider a more appropriate water_level or shoreline contour.\n Clipping all depths to 0.0001")
      #   depths <- pmax(depths, 0.0001)
      #
      # }

      #Check and warn if any NAs are extracted from the bathy indicating non overlap of rays and raster
      if (any(is.na(depths))) {
        # Build the specific warning message using columns from fetch_ray
        msg <- paste0(
          "NA values extracted from bathymetry at site '",
          fetch_ray$site,
          "' at direction '",
          fetch_ray$direction,
          "'. ",
          "This usually means the fetch ray extends beyond the bathy raster's coverage. ",
          "Ensure bathymetry raster has full coverage for `max_fetch`."
        )
        warning(msg, call. = FALSE)
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

#' Extract Bathymetry Along Fetch Ray
#'
#' Extract bathymetry values at regular intervals along a fetch ray from a
#' raster.
#'
#' @description This function generates sampling points along a fetch ray and
#' extracts corresponding bathymetry values from a raster dataset.
#'
#' @param bathy_raster A SpatRaster object containing bathymetry data
#' @param fetch_ray A spatial vector object (SpatVector) or sf object
#'   representing a linear geometry (fetch ray)
#' @param sample_dist Numeric. The distance between sampling points along the
#'   ray
#'
#' @return A list containing: \item{bathy}{Numeric vector of bathymetry values
#'   extracted at each sampling point} \item{distances}{Numeric vector of
#'   distances from ray start for each sampling point}
#'
#' @details This function is a wrapper that combines point generation along the
#' ray with raster value extraction
#'
#' @keywords internal
extract_bathy_along_fetch <- function(bathy_raster,
                                      fetch_ray,
                                      sample_dist) {
  # Check if input is SpatVector, convert if necessary
  if (inherits(fetch_ray, "SpatVector")) {
    ray_vect <- fetch_ray
  } else{
    ray_vect <- terra::vect(fetch_ray)
  }
  # Generate sampling points along the ray
  samp_points <- generate_points_along_ray(ray_vect, sample_dist)

  # Extract bathymetry values at sampling points
  bathy_values <- terra::extract(bathy_raster, samp_points$points, xy = F, ID = F, bind = T)

  # Return bathymetry values and distances
  return(
    list(
      bathy = terra::values(bathy_values)[[1]],
      distances = samp_points$distances
    )
  )
}

#' Generate Points Along Ray
#'
#' Generate equally spaced sampling points along a fetch ray geometry.
#'
#' @description
#' This function takes a linear geometry (fetch ray) and generates sampling points
#' at regular intervals along its length. The function handles the positioning of
#' points when the total length doesn't divide evenly by the sample distance.
#'
#' @param fetch_ray A spatial vector object (SpatVector) or sf object representing
#'   a linear geometry (fetch ray)
#' @param sample_dist Numeric. The distance between sampling points along the ray
#'
#' @return A list containing:
#'   \item{points}{SpatVector of point geometries along the ray}
#'   \item{distances}{Numeric vector of distances from previous point}
#'
#' @details
#' When the total ray length doesn't divide evenly by sample_dist, there will be
#' a single shorter distance between some points.
#'
#' @keywords internal
generate_points_along_ray <- function(fetch_ray, sample_dist) {
  # Check if input is SpatVector, convert if necessary
  if (inherits(fetch_ray, "SpatVector")) {
    ray_vect <- fetch_ray
  } else {
    ray_vect <- terra::vect(fetch_ray)
  }

  # Get line coordinates
  coords <- terra::geom(ray_vect)[, c("x", "y")]

  # Calculate total ray length
  total_length <- terra::perim(ray_vect)

  # Create target distances - equally spaced points and the final point
  target_distances <- unique(sort(c(seq(from = total_length, to = 0, by = -sample_dist), 0)))

  # make the vector start with the far point
  distances <- rev(diff(target_distances))
  target_distances <- rev(target_distances)

  # Interpolate coordinates at target distances
  x_interp <- stats::approx(c(0, total_length), coords[, 1], xout = target_distances)$y
  y_interp <- stats::approx(c(0, total_length), coords[, 2], xout = target_distances)$y

  # Create SpatVector points
  sample_coords <- cbind(x_interp, y_interp)
  ray_points_vect <- terra::vect(sample_coords, type = "points", crs = terra::crs(ray_vect))

  return(list(points = ray_points_vect, distances = distances))
}
