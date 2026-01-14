# Retrieve and clean wind data for WEMo

This function is a wrapper around a workflow to find and download wind
data using the
[import_ghcn_stations()](https://openair-project.github.io/worldmet/reference/import_ghcn_stations.html)
and
[import_ghcn_hourly()](https://openair-project.github.io/worldmet/reference/import_ghcn_hourly.html)
functions from the **worldmet** package. It finds up to 5 nearby NOAA
GHCN weather stations based on a geographic point and downloads hourly
wind data.

## Usage

``` r
get_wind_data(site_point, years, which_station = "ask")
```

## Arguments

- site_point:

  A spatial point object (either `sf` or coercible to `sf`) indicating
  the target location.

- years:

  A vector of years (e.g., `2002:2024`) for which to download wind data.

- which_station:

  Either:

  - `"ask"`: interactively choose from 5 closest stations;

  - an integer (1-5): pick the nth closest station;

  - a string station code in the form "USAF-WBAN".

## Value

A data frame with cleaned wind direction and wind speed, along with
station code, timestamp, and date components (year, month, day).

## Details

This function utilizes data from **Global Historical Climatology Network
(GHCN)** dataset. GHCN aggregates hourly meteorological observations
from numerous fixed, land-based stations. maintained by NOAA, the U.S.
Air Force, and many other meteorological agencies (Met Services) around
the world.

## References

For more information on the GHCN and to view an interactive map of
stations, see
https://www.ncei.noaa.gov/products/global-historical-climatology-network-hourly

## Examples

``` r
# Create a POINT geometry from coordinates and cast explicitly
site_point <- sf::st_sf(
  geometry = sf::st_sfc(sf::st_point(c(-76.67587, 34.71413))),
  crs = 4326
  )

# 1. Prompt user to select a station interactively
if (FALSE) { # \dontrun{
wind_data_ask <- get_wind_data(
  site_point,
  years = 2022:2023,
  which_station = "ask"
 )
} # }

# 2. Automatically use the 2nd closest station
if (FALSE) { # \dontrun{
wind_data_index <- get_wind_data(
  site_point,
  years = 2022:2023,
  which_station = 2
 )
} # }

# 3. Manually specify a station code
if (FALSE) { # \dontrun{
# this way you don't need a site_point
wind_data_manual <- get_wind_data(
  site_point = NULL,
  years = 2022:2023,
  which_station = "USW00093765"
 )
} # }
```
