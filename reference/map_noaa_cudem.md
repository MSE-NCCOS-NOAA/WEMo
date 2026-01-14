# Interactively Map NOAA CUDEM Tiles

Generate an interactive map showing the geographic extent of all
available NOAA CUDEM (Continuously Updated Digital Elevation Model)
tiles. Optionally, users can specify a site location (`lon`, `lat`) and
a radius (`radius_m`) to highlight relevant tiles near a point of
interest.

## Usage

``` r
map_noaa_cudem(lon = NULL, lat = NULL, radius_m = 10000)
```

## Arguments

- lon:

  Optional. A numeric value representing the longitude of the center
  point in decimal degrees (WGS84). If not provided, the full tile index
  is displayed without zooming or highlighting nearby tiles.

- lat:

  Optional. A numeric value representing the latitude of the center
  point in decimal degrees (WGS84). Must be provided if `lon` is given.

- radius_m:

  A numeric value specifying the radius in meters to search for CUDEM
  tiles around the center point. Defaults to 10,000. Ignored if `lon`
  and `lat` are not specified.

## Value

Invisibly returns `TRUE`. The primary output is an interactive `leaflet`
map printed to the viewer.

## Examples

``` r
if (interactive()) {
  # Map showing only the full tile index
  map_noaa_cudem()

  # Map showing tiles near a specific location
  longitude <- -76.67
  latitude <- 34.72
  search_radius_meters <- 5000

  map_noaa_cudem(lon = longitude, lat = latitude, radius_m = search_radius_meters)
}
```
