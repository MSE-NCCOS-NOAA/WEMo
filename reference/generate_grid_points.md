# Generate a Grid of Points Over a Spatial Extent

Creates a regular grid of points covering the (optionally expanded)
extent of a spatial object at a specified resolution. Accepts either an
`sf` object or a `SpatVector` as input. The output will match the input
class.

## Usage

``` r
generate_grid_points(site_point, expansion_dist, resolution)
```

## Arguments

- site_point:

  A spatial point or set of points, either an `sf` object or a
  `SpatVector`. The extent of this object defines the initial bounds of
  the grid.

- expansion_dist:

  Numeric vector of length 1 or 2. Distance (in map units) to expand the
  bounding box in the x and y directions. If a single value is provided,
  it is used for both directions. To use the exact extent of
  `site_point`, set this to `0`.

- resolution:

  Numeric vector of length 1 or 2. The spacing between grid points in
  the x and y directions (in map units). If a single value is provided,
  it is used for both directions.

## Value

A grid of points with regular spacing, returned as the same class as
`site_point` (either `sf` or `SpatVector`). Each point includes a `site`
variable identifying its row index (1 to n).

## Examples

``` r
# single point ----------------------------
site_pt <- terra::vect(
  data.frame(x = -76.67587, y = 34.71413),
  geom = c("x", "y"),
  crs = "EPSG:4326"
  )

# Create a grid of points covering an area expanded by 0.001° (x) and 0.0002°
# (y) # around the input point, with grid points spaced every 0.0001° (x) and
# 0.00005° (y)
grid_pts <- generate_grid_points(
  site_point = site_pt,
  expansion_dist = c(0.001, 0.0002),
  resolution = c(0.0001, 0.00005)
 )

# terra::plot(grid_pts)

# multiple points ---------------------------
site_pts <- terra::vect(
  data.frame(
    x = c(346536.4, 346986.4, 346986.4),
    y = c(3842620,  3842670,  3843270)),
  geom = c("x", "y"),
  crs = "EPSG:26918")

# create a grid of points covering an area expanded by 1000 map units
# (meters) with points spaced every 50 map units (meters)
grid_pts <- generate_grid_points(
  site_point = site_pts,
  expansion_dist = 1000,
  resolution = 50
 )

# terra::plot(grid_pts)
```
