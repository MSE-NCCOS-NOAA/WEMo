# Extract Bathymetry Along Fetch Ray

This function generates sampling points along a fetch ray and extracts
corresponding bathymetry values from a raster dataset.

## Usage

``` r
extract_bathy_along_fetch(bathy_raster, fetch_ray, sample_dist)
```

## Arguments

- bathy_raster:

  A SpatRaster object containing bathymetry data

- fetch_ray:

  A spatial vector object (SpatVector) or sf object representing a
  linear geometry (fetch ray)

- sample_dist:

  Numeric. The distance between sampling points along the ray

## Value

A list containing:

- bathy:

  Numeric vector of bathymetry values extracted at each sampling point

- distances:

  Numeric vector of distances from ray start for each sampling point

## Details

Extract bathymetry values at regular intervals along a fetch ray from a
raster.

This function is a wrapper that combines point generation along the ray
with raster value extraction
