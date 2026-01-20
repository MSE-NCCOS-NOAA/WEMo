# Package index

## Core Modeling Functions

- [`wemo_full()`](https://mse-nccos-noaa.github.io/WEMo/reference/wemo_full.md)
  : Run Full WEMo Workflow - Calculate Fetch, Interrogate Bathymetry,
  and Build Wind Waves
- [`wemo()`](https://mse-nccos-noaa.github.io/WEMo/reference/WEMo.md) :
  Run the WEMo Wave Energy Model
- [`build_wind_wave()`](https://mse-nccos-noaa.github.io/WEMo/reference/build_wind_wave.md)
  : Generate and Propagate a Wind Wave Over Variable Bathymetry
- [`find_fetch()`](https://mse-nccos-noaa.github.io/WEMo/reference/find_fetch.md)
  : Calculate Wind Fetch Distances to Shoreline
- [`effective_fetch()`](https://mse-nccos-noaa.github.io/WEMo/reference/effective_fetch.md)
  : Calculate Effective Fetch for Wind Energy Transfer
- [`update_depths()`](https://mse-nccos-noaa.github.io/WEMo/reference/update_depths.md)
  : Update Depths in a WEMo input

## Gathering and Preparing Input Data

### Wind Data

- [`get_wind_data()`](https://mse-nccos-noaa.github.io/WEMo/reference/get_wind_data.md)
  : Retrieve and clean wind data for WEMo
- [`summarize_wind_data()`](https://mse-nccos-noaa.github.io/WEMo/reference/summarize_wind_data.md)
  : Summarize wind data by direction and intensity
- [`plot_wind_rose()`](https://mse-nccos-noaa.github.io/WEMo/reference/plot_wind_rose.md)
  : Plot a Wind Rose Diagram

### Bathymetry

- [`map_noaa_cudem()`](https://mse-nccos-noaa.github.io/WEMo/reference/map_noaa_cudem.md)
  : Interactively Map NOAA CUDEM Tiles
- [`get_noaa_cudem()`](https://mse-nccos-noaa.github.io/WEMo/reference/get_noaa_cudem.md)
  : Download NOAA CUDEM (Continuously Updated DEM) Topo-Bathy Rasters

### Shoreline and Site Points

- [`generate_grid_points()`](https://mse-nccos-noaa.github.io/WEMo/reference/generate_grid_points.md)
  : Generate a Grid of Points Over a Spatial Extent
- [`generate_shoreline_from_bathy()`](https://mse-nccos-noaa.github.io/WEMo/reference/generate_shoreline_from_bathy.md)
  : Extract shoreline from a bathymetric raster using a contour
  threshold

### Data Preparation

- [`prepare_wemo_inputs()`](https://mse-nccos-noaa.github.io/WEMo/reference/prepare_wemo_inputs.md)
  : Prepare Input Data for WEMo Model

## Example Datasets

- [`PI_points`](https://mse-nccos-noaa.github.io/WEMo/reference/PI_points.md)
  : Example Site Points for WEMo
- [`PI_shoreline`](https://mse-nccos-noaa.github.io/WEMo/reference/PI_shoreline.md)
  : Example Shoreline Polygon for WEMo
- [`PI_wind_data`](https://mse-nccos-noaa.github.io/WEMo/reference/PI_wind_data.md)
  : Example Wind Data for WEMo examples
