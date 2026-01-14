# Example Wind Data for WEMo examples

Wind history data from

## Usage

``` r
PI_wind_data
```

## Format

### `PI_wind_data`

A tibble with 184,947 rows and 73 columns

- code:

  the ISD station code

- time:

  datetime when the observation was made

- year:

  year when the observation was made

- month:

  month when the observation was made

- day:

  day when the observation was made

- wind_direction:

  direction from which the wind blows. degrees off north

- speed:

  speed of the wind. meters per second

## Source

created by: get_wind_data( site_point = NULL, years = 2023:2024,
which_station = "723090-13754" )
