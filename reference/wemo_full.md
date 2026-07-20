# Run Full WEMo Workflow - Calculate Fetch, Interrogate Bathymetry, and Build Wind Waves

This is the full workflow to do wave energy modeling using WEMo. It
calculates fetch, effective fetch, interrogates bathymetry, merges wind
data, builds waves along each fetch ray and then summarizes the wind
driven waves each `site_point`

## Usage

``` r
wemo_full(
  site_points,
  shoreline,
  bathy,
  wind_data,
  directions = unique(wind_data$direction),
  max_fetch = 10000,
  sample_dist = 5,
  water_level,
  depths_or_elev = "elev"
)
```

## Arguments

- site_points:

  An `sf` or `SpatVector` point object representing the location(s) for
  wave exposure analysis. Will be coerced to `sf`.

- shoreline:

  An `sf` or `SpatVector` polygon representing land or shoreline
  features.

- bathy:

  A `SpatRaster` containing bathymetric data (in meters, negative or
  positive depending on `depths_or_elev`).

- wind_data:

  A data frame with wind direction (`direction`), speed (`speed`), and
  proportion wind blows from the direction (`proportion`)

- directions:

  Numeric vector of directions (in degrees) to cast fetch lines.

- max_fetch:

  Maximum fetch length (in map units). Default is 10,000.

- sample_dist:

  Distance (in map units) between sample points along each fetch line.
  Default is 5.

- water_level:

  Numeric. The water level relative to bathymetry (in meters).

- depths_or_elev:

  Character, either `'depths'` or `'elev'`. Specifies whether bathymetry
  is interpreted as water depth (positive down) or elevation (negative
  down). If `'elev'`, values are flipped to represent depth.

## Value

A list of two elements:

- `wemo_details`:

  the original `fetch` object with the following stats about each wave
  arriving at the site from the direction of the fetch ray added:

  - `wave_height_final` - Final wave height at the end of the fetch (m)

  - `WEI` - Wave Energy Index

  - `wave_period` - Estimated wave period (s)

  - `wave_number` - Average wave number (rad/m)

  - `celerity_final` - Final wave phase speed (m/s)

  - `nnumber_final` - Final group velocity coefficient

- `wemo_output`:

  sf object with all the variables from `site_point` in addition to the
  following:

  - `RWE` - Total relative wave exposure at the site

  - `avg_wave_height` - Mean wave height across all fetch rays (m)

  - `max_wave_height` - Maximum wave height across all fetch rays (m)

  - `direction_of_max_wave` - Direction(s) of maximum wave height
    (Degrees from North)

## Details

First fetch is calculated for each `site_point` in each direction
specified by `directions`. The function performs the following
operations:

1.  Ensures coordinate reference systems (CRS) match between input
    layers

2.  Filters out sites located on land

3.  For each site and direction, creates a ray extending to max_fetch
    distance

4.  Identifies intersections between rays and shoreline polygons

5.  Calculates distances from sites to nearest intersection points

6.  Returns fetch rays as linestring geometries with associated
    measurements

First, the function builds a wind-wave over input bathymetry and returns
an estimate of final wave height and related wave characteristics (e.g.,
wave energy index, wave period, wave number, celerity).

Then, the function summarizes the waves from every direction for each
site and computes the summary statistic `RWE`.

## See also

[`find_fetch()`](https://mse-nccos-noaa.github.io/WEMo/reference/find_fetch.md),
[`prepare_wemo_inputs()`](https://mse-nccos-noaa.github.io/WEMo/reference/prepare_wemo_inputs.md),
[`wemo()`](https://mse-nccos-noaa.github.io/WEMo/reference/WEMo.md)
