# Interrogate Bathymetry Along Fetch Rays

This function processes multiple fetch rays, extracting bathymetry
values along each ray and storing the results as list columns in the
input spatial dataframe.

## Usage

``` r
interrogate_bathy(
  fetch,
  bathy_raster,
  sample_dist = 10,
  depths_or_elev = "elev",
  water_level = 0
)
```

## Arguments

- fetch:

  A spatial dataframe (sf) containing fetch ray geometries

- bathy_raster:

  A SpatRaster object containing bathymetry data

- sample_dist:

  Numeric. The distance between sampling points along each ray (default:
  10)

- depths_or_elev:

  Character string denoting if `bathy_raster` stores depths or elevation
  values. Defaults to `'elev'`. Must be either `'depths'` or `'elev'`.

  - `'depths'`: bathymetry values are depths (more positive values are
    deeper). `bathy_raster` values are added with `water_level`
    directly.

  - `'elev'`: (Default) bathymetry values are elevations (more positive
    values are shallower/above water). `bathy_rater` values are
    multiplied by -1 before adding `water_level`.

- water_level:

  the water level that you want to calculate depths for

## Value

A spatial dataframe with added list columns:

- bathy:

  List column containing bathymetry values for each fetch ray

- distances:

  List column containing distance values for each fetch ray

## Details

Extract bathymetry data along multiple fetch rays and return updated
input dataset.

This function processes each row of the input fetch dataframe,
extracting bathymetry values along the corresponding geometry. Results
are stored as list columns, allowing each row to contain vectors of
different lengths.

## Examples

``` r
if (FALSE) { # \dontrun{
# Process all fetch rays with 10m sampling
fetch_with_bathy <- interrogate_bathy(fetch_rays, bathy_raster, sample_dist = 10)
} # }
```
