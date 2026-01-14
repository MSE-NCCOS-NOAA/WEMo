# Extract shoreline from a bathymetric raster using a contour threshold

This function identifies land areas from a bathymetric raster by
applying a contour threshold, converts the result into polygon geometry,
and optionally saves it to a shapefile. If `contour` is greater than the
max value in `bathy` a warning is displayed and `contour` is set to the
max value from `bathy`

## Usage

``` r
generate_shoreline_from_bathy(
  bathy,
  contour,
  save_output = FALSE,
  filename = "shoreline.shp"
)
```

## Arguments

- bathy:

  A `SpatRaster` object representing bathymetric or elevation data.

- contour:

  A numeric value representing the elevation threshold to define land.

- save_output:

  Logical. If `TRUE`, the output polygon will be saved to a shapefile.
  Default is `FALSE`.

- filename:

  A string giving the name of the shapefile to save if
  `save_output = TRUE`. Default is `"shoreline.shp"`.

## Value

A `sf` polygon object representing the extracted land areas (shoreline).

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with a bathymetric raster
shoreline <- shoreline_from_bathy(bathy = bathy_raster, contour = 0)

# Save the result to a shapefile
shoreline <- shoreline_from_bathy(
  bathy_raster,
  contour = 0,
  save_output = TRUE,
  filename = "shoreline.shp"
 )
} # }
```
