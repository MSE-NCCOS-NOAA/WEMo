# Summarize wind data by direction and intensity

Generates a summary data.frame suitable for passing to
[`plot_wind_rose()`](https://mse-nccos-noaa.github.io/WEMo/reference/plot_wind_rose.md)
for plotting or
[`wemo()`](https://mse-nccos-noaa.github.io/WEMo/reference/WEMo.md) for
modeling. Input data should be a log of wind readings and must have
columns `wind_direction` and `wind_speed`. Data generated from
[`get_wind_data()`](https://mse-nccos-noaa.github.io/WEMo/reference/get_wind_data.md)
is ready to be used as input. Input `wind_direction` values are coerced
into values 0-359.9. The function can further coerce or snap input
`wind_direction`s to a user supplied vector of suitable directions
(`directions`). This is often necessary with input data from public data
sets that have occasional anomalous readings. If `directions` is not
supplied the output is based on all unique `wind_direction`.

## Usage

``` r
summarize_wind_data(
  wind_data,
  wind_percentile,
  directions = NULL,
  wind_speed_na.rm = TRUE
)
```

## Arguments

- wind_data:

  A data frame containing at least the columns `wind_direction` and
  `wind_speed`

- wind_percentile:

  either 'mean' or a numeric between 0 and 1 specifying the percentile
  of `wind_speed` to calculate

- directions:

  numeric vector of wind directions to snap input `wind_direction` to.
  This is useful if input `wind_data$wind_data` has anomolous
  `wind_direction` readings

- wind_speed_na.rm:

  logical. indicating if `wind_speed` of `NA` indicates bad data
  (`TRUE`, the default) or calm wind (`FALSE`). If `FALSE`, the `NA`
  value will increase the `n` and `proportion` values but not effect the
  `speed` value in the returned object

## Value

A tibble summarizing wind by direction and intensity, with columns:

- direction:

  Wind direction (degrees, with 360 converted to 0)

- n:

  Count of observations in each direction

- proportion:

  Percentage of total observations in each direction

- speed:

  Mean or percentile wind speed for each direction

## Examples

``` r
set.seed(1) #for reproduceability
# generate some example data
wind_data <- data.frame(
  wind_direction = sample(c(90, 180, 270, 360), size = 25, replace = TRUE),
  wind_speed = runif(25, 0, 25)
)

# Summarize using the 95th percentile for wind speed
summarize_wind_data(wind_data, wind_percentile = 0.95)
#> # A tibble: 4 × 4
#>   direction     n proportion  speed
#>       <dbl> <int>      <dbl>  <dbl>
#> 1         0     1          4  0.335
#> 2        90    10         40 20.8  
#> 3       180     7         28 20.6  
#> 4       270     7         28 18.9  

# Summarize using the mean wind speed
summarize_wind_data(wind_data, wind_percentile = "mean")
#> # A tibble: 4 × 4
#>   direction     n proportion  speed
#>       <dbl> <int>      <dbl>  <dbl>
#> 1         0     1          4  0.335
#> 2        90    10         40 14.2  
#> 3       180     7         28 14.8  
#> 4       270     7         28 12.5  

# create some dummy data with NAs in the speed and direction columns
wind_data <- data.frame(
  wind_direction = c(runif(n = 5, min = 0, max = 315),rep(NA, 10)),
  wind_speed = c(runif(5, 0, 25), rep(NA, 5), rep(0, 5))
)

# remove the NAs in the speed column. perhaps these are data collection errors
summarize_wind_data(wind_data, wind_percentile = 'mean',
  directions = c(90, 180, 270, 360), wind_speed_na.rm = TRUE)
#> # A tibble: 4 × 4
#>   direction     n proportion speed
#>       <dbl> <int>      <dbl> <dbl>
#> 1        90     2         20 13.4 
#> 2       180     2         20  7.73
#> 3       270     1         10  7.91
#> 4        NA     5         50  0   

# keep the NAs in the speed column. perhaps these represent calm data
# (though normally 0 would indicate calm)
summarize_wind_data(wind_data, wind_percentile = 'mean',
  directions = c(90, 180, 270, 360), wind_speed_na.rm = FALSE)
#> # A tibble: 4 × 4
#>   direction     n proportion speed
#>       <dbl> <int>      <dbl> <dbl>
#> 1        90     2      13.3  13.4 
#> 2       180     2      13.3   7.73
#> 3       270     1       6.67  7.91
#> 4        NA    10      66.7   0   
```
