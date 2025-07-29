#' Generate a Grid of Points Over a Spatial Extent
#'
#' Creates a regular grid of points covering the (optionally expanded) extent of
#' a spatial object at a specified resolution. Accepts either an `sf` object or
#' a `SpatVector` as input. The output will match the input class.
#'
#' @param site_point A spatial point or set of points, either an `sf` object or
#'   a `SpatVector`. The extent of this object defines the initial bounds of the
#'   grid.
#' @param expansion_dist Numeric vector of length 1 or 2. Distance (in map
#'   units) to expand the bounding box in the x and y directions. If a single
#'   value is provided, it is used for both directions. To use the exact extent
#'   of `site_point`, set this to `0`.
#' @param resolution Numeric vector of length 1 or 2. The spacing between grid
#'   points in the x and y directions (in map units). If a single value is
#'   provided, it is used for both directions.
#'
#' @return A grid of points with regular spacing, returned as the same class as
#'   `site_point` (either `sf` or `SpatVector`). Each point includes a `site`
#'   variable identifying its row index (1 to n).
#'
#' @examples
#' # single point ----------------------------
#' site_pt <- terra::vect(
#'   data.frame(x = -76.67587, y = 34.71413),
#'   geom = c("x", "y"),
#'   crs = "EPSG:4326"
#'   )
#'
#' # Create a grid of points covering an area expanded by 0.001° (x) and 0.0002°
#' # (y) # around the input point, with grid points spaced every 0.0001° (x) and
#' # 0.00005° (y)
#' grid_pts <- generate_grid_points(
#'   site_point = site_pt,
#'   expansion_dist = c(0.001, 0.0002),
#'   resolution = c(0.0001, 0.00005)
#'  )
#'
#' # terra::plot(grid_pts)
#'
#' # multiple points ---------------------------
#' site_pts <- terra::vect(
#'   data.frame(
#'     x = c(346536.4, 346986.4, 346986.4),
#'     y = c(3842620,  3842670,  3843270)),
#'   geom = c("x", "y"),
#'   crs = "EPSG:26918")
#'
#' # create a grid of points covering an area expanded by 1000 map units
#' # (meters) with points spaced every 50 map units (meters)
#' grid_pts <- generate_grid_points(
#'   site_point = site_pts,
#'   expansion_dist = 1000,
#'   resolution = 50
#'  )
#'
#' # terra::plot(grid_pts)
#'
#' @export
generate_grid_points <- function(site_point, expansion_dist, resolution){
  # Record input class to determine output type
  input_is_sf <- inherits(site_point, "sf")

  # Convert sf to SpatVector if needed
  if (input_is_sf) {
    site_point <- terra::vect(site_point)
  } else if (!inherits(site_point, "SpatVector")) {
    stop("site_point must be of class 'sf' or 'SpatVector'.")
  }

  # divide the input by 2 since the total expansion will require half the input
  # added to either side
  expansion_dist <- expansion_dist/2

  # If only one expansion_dist value is provided, use the same for both x and y
  # directions
  if (length(expansion_dist) == 1) {
    expansion_dist[2] <- expansion_dist[1]
  }

  # If only one resolution value is provided, use the same for both x and y
  # directions
  if (length(resolution) == 1) {
    resolution[2] <- resolution[1]
  }

  # # check if the site object is SpatVector or can be coerced to a SpatVector
  # if (!inherits(try(terra::vect(site_point), silent = TRUE), "try-error")) {
  #   site_point <- terra::vect(site_point)
  # }else {
  #   if(!inherits(site_point, "SpatVector")){
  #     stop("site_point cannot be coerced to a SpatVector.")
  #   }
  # }

  # Get the bounding box of the site point
  bbox <- terra::ext(site_point)

  # Expand xmin by the expansion distance
  xmin <- bbox$xmin - expansion_dist[1]
  # Expand xmax by the expansion distance
  xmax <- bbox$xmax + expansion_dist[1]
  # Expand ymin by the expansion distance
  ymin <- bbox$ymin - expansion_dist[2]
  # Expand ymax by the expansion distance
  ymax <- bbox$ymax + expansion_dist[2]

  # expanded bounding box
  x <- terra::ext(xmin, xmax, ymin, ymax)

  # Calc center point
  center <- data.frame(x = (x$xmin + x$xmax)/2, y = (x$ymin + x$ymax)/2)

  # Create sequences of x and y values based on the resolution and extent
  x_seq <- c(
    seq(
      from = center$x,
      to = x$xmax,
      by = resolution[1]
    ),
    seq(
      from = center$x,
      to = x$xmin,
      by = -resolution[1]
    )
  )

  x_seq <- sort(unique(x_seq))

  y_seq <- c(
    seq(
      from = center$y,
      to = x$ymax,
      by = resolution[2]
    ),
    seq(
      from = center$y,
      to = x$ymin,
      by = -resolution[2]
    )
  )

  y_seq <- sort(unique(y_seq))

  # Create a grid of points
  grid <- expand.grid(x = x_seq, y = y_seq)

  # Convert the grid to a SpatVector (points)
  spatial_grid <- terra::vect(grid, geom = c('x', 'y'),  crs = terra::crs(site_point))

  # Add a site variable numbering each point
  spatial_grid$site <- seq_len(nrow(spatial_grid))

  # Convert back to sf if original input was sf
  if (input_is_sf) {
    return(sf::st_as_sf(spatial_grid))
  } else {
    return(spatial_grid)
  }

  return(spatial_grid)
}
