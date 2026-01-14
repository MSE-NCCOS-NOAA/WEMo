# Convert compass degrees (direction) to radian degrees

Convert compass degrees (direction) to radian degrees

## Usage

``` r
compassDegrees_to_radianDegrees(compassDegrees)
```

## Arguments

- compassDegrees:

  compass direction or heading in degrees

## Value

a numerical vector

## Examples

``` r
# North on a compass is 0 deg
compassDegrees_to_radianDegrees(0)
#> [1] 90

# East on a compass is 90 deg
compassDegrees_to_radianDegrees(90)
#> [1] 0

# South on a compass is 180 deg
compassDegrees_to_radianDegrees(180)
#> [1] 270

# West on a compass is 270 deg
compassDegrees_to_radianDegrees(270)
#> [1] 180

# North on a compass is ALSO 360 deg
compassDegrees_to_radianDegrees(360)
#> [1] 90
```
