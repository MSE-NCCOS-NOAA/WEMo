#' Generate a Grid of Points Over a Spatial Extent
#'
#' Creates a regular grid of points covering the (optionally expanded) extent of a `SpatVector` object
#' at a specified resolution. Returns the grid as a `SpatVector` of point geometry.
#'
#' @param site_point A `SpatVector`. The spatial object whose extent will define the bounds of the grid.
#' @param expansion_dist Numeric vector of length 1 or 2. Distance (in map units) to expand the bounding box
#'        in the x and y directions. If a single value is provided, it is used for both directions.
#'        To prevent expansion and use the exact bounding box of `site_point`, set this to `0`.
#' @param resolution Numeric vector of length 1 or 2. The spacing between grid points
#'        in the x and y directions (in map units). If a single value is provided,
#'        it is used for both directions.
#'
#' @return A `SpatVector` of points spaced regularly over the (expanded) extent of `site_point`.
#'
#' @examples
#'
#' site_pt <- terra::vect(data.frame(x = -76.67587, y = 34.71413), geom = c("x", "y"), crs = "EPSG:4326")
#'
#' # Create a grid of points covering an area expanded by 0.001° (x) and 0.0002° (y)
#' # around the input point, with grid points spaced every 0.0001° (x) and 0.00005° (y)
#' grid_pts <- generate_grid_points(site_point = site_pt, expansion_dist = c(0.001, 0.0002), resolution = c(0.0001, 0.00005))
#' terra::plot(grid_pts)
#'
#' site_pts <- terra::vect(points)
#'
#' # create a grid of points covering an area expanded by 1000 map units (meters) with points spaced every 50 map units (meters)
#' grid_pts <- generate_grid_points(site_point = site_pts, expansion_dist = 1000, resolution = 50)
#'
#' terra::plot(grid_pts)
#'
#' @export
generate_grid_points <- function(site_point, expansion_dist, resolution){
  # divide the input by 2 since the total expansion will require half the input added to either side
  expansion_dist <- expansion_dist/2

  # If only one expansion_dist value is provided, use the same for both x and y directions
  if (length(expansion_dist) == 1) {
    expansion_dist[2] <- expansion_dist[1]
  }

  # If only one resolution value is provided, use the same for both x and y directions
  if (length(resolution) == 1) {
    resolution[2] <- resolution[1]
  }

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

  # Create sequences of x and y values based on the resolution and extent
  x_seq <- seq(from = x$xmin, to = x$xmax, by = resolution[1])
  y_seq <- seq(from = x$ymin, to = x$ymax, by = resolution[2])

  # Create a grid of points
  grid <- expand.grid(x = x_seq, y = y_seq)

  # Convert the grid to a SpatVector (points)
  spatial_grid <- terra::vect(grid, geom = c('x', 'y'),  crs = terra::crs(site_point))

  return(spatial_grid)
}
