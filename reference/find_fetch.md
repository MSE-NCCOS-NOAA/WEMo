# Calculate Wind Fetch Distances to Shoreline

This function calculates wind fetch distances from water-based sites to
shorelines in multiple directions. Wind fetch represents the distance
over which wind can act on a water surface without obstruction by land,
making it a critical parameter in coastal and marine studies for wave
modeling, habitat analysis, and environmental assessments.

## Usage

``` r
find_fetch(
  site_points,
  polygon_layer,
  directions = c(0, 45, 90, 135, 180, 225, 270, 315),
  max_fetch = 10000
)
```

## Arguments

- site_points:

  An `sf` object containing point geometries representing the sites
  where fetch distances will be calculated. Sites located on land
  (intersecting with polygon_layer) are automatically filtered out.

- polygon_layer:

  An `sf` object containing polygon geometries representing land masses
  or shorelines. Fetch distances are calculated to the boundaries of
  these polygons.

- directions:

  A numeric vector of angular directions (in compass degrees) for which
  to calculate fetch distances. Default is 8 equally-spaced directions
  from 0 to 315 (every 45). Directions follow compass convention where 0
  = North, 90 = East, 180 = South, 270 = West.

- max_fetch:

  A numeric value specifying the maximum fetch distance to consider (in
  map units). If no shoreline intersection is found within this
  distance, the fetch is recorded as the maximum value. Default is
  10,000 units.

## Value

An `sf` object containing linestring geometries representing fetch rays
with the following columns:

- geometry:

  Linestring geometries showing fetch rays from each site

- direction:

  Direction angle (in degrees) for each fetch ray

- fetch:

  Calculated fetch distance (in map units)

- site_id:

  Integer identifier linking each ray to its originating site

## Details

The function performs the following operations:

1.  Ensures coordinate reference systems (CRS) match between input
    layers

2.  Filters out sites located on land

3.  For each site and direction, creates a ray extending to max_fetch
    distance

4.  Identifies intersections between rays and shoreline polygons

5.  Calculates distances from sites to nearest intersection points

6.  Returns fetch rays as linestring geometries with associated
    measurements

Direction angles are converted from compass degrees to mathematical
radians internally using a helper function
[compassDegrees_to_radianDegrees](https://mse-nccos-noaa.github.io/WEMo/reference/compassDegrees_to_radianDegrees.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with default 8 directions and 10,000 map units max fetch
fetch_rays <- find_fetch_sf(water_sites, land_polygons)

# Custom directions - cardinal directions only
n_headings = 4
headings <- seq(0, 360 - 360 / n_headings, length.out = n_headings)

fetch_rays <- find_fetch_sf(
  site_points = sites,
  polygon_layer = coastline,
  directions = headings,
  max_fetch = 5000
)

# High resolution analysis every 10 degrees
n_headings = 36
headings <- seq(0, 360 - 360 / n_headings, length.out = n_headings)
fetch_rays <- find_fetch_sf(
  site_points = sites,
  polygon_layer = coastline,
  directions = headings,
  max_fetch = 1000
)

} # }
```
