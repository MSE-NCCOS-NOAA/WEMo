# Generate Points Along Ray

This function takes a linear geometry (fetch ray) and generates sampling
points at regular intervals along its length. The function handles the
positioning of points when the total length doesn't divide evenly by the
sample distance.

## Usage

``` r
generate_points_along_ray(fetch_ray, sample_dist)
```

## Arguments

- fetch_ray:

  A spatial vector object (SpatVector) or sf object representing a
  linear geometry (fetch ray)

- sample_dist:

  Numeric. The distance between sampling points along the ray

## Value

A list containing:

- points:

  SpatVector of point geometries along the ray

- distances:

  Numeric vector of distances from previous point

## Details

Generate equally spaced sampling points along a fetch ray geometry.

When the total ray length doesn't divide evenly by sample_dist, there
will be a single shorter distance between some points.
