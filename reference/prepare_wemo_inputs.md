# Prepare Input Data for WEMo Model

This function chains together the core components needed to prepare a
fetch dataset for wave energy modeling using the WEMo framework. It
calculates fetch lines, effective fetch, interrogates bathymetry, and
merges wind data, returning a spatial object ready for wave modeling.

## Usage

``` r
prepare_wemo_inputs(
  site_points,
  shoreline,
  bathy,
  wind_data,
  directions,
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
  proporation wind blows from the direction (`proportion`)

- directions:

  Numeric vector of directions (in degrees) to cast fetch lines.

- max_fetch:

  Maximum fetch length (in map units, typically meters). Default is
  1000.

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

An `sf` object containing the fetch lines with columns:

- efetch:

  Effective fetch distance (in meters)

- depths:

  List-column of depths along each fetch line

- distances:

  List-column of distances between sample points

- direction:

  Direction of the fetch line

- speed:

  Wind speed (from `wind_data`) corresponding to direction
