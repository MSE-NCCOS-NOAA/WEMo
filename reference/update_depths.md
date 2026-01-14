# Update Depths in a WEMo input

Update the depth values in a object created by
[`prepare_wemo_inputs()`](https://mse-nccos-noaa.github.io/WEMo/reference/prepare_wemo_inputs.md).

## Usage

``` r
update_depths(wemo_input, depth_diff)
```

## Arguments

- wemo_input:

  `sf` object with column `depths` which is a list of vectors

- depth_diff:

  numeric. indicates the amount to increase (or decrease for negative
  values)

## Value

`sf` object ready for processing in
[`wemo()`](https://mse-nccos-noaa.github.io/WEMo/reference/WEMo.md)
