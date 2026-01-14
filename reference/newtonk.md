# Compute Wave Number via Newton-Raphson Iteration

Solves the wave dispersion equation for wave number (`k`) using the
Newton-Raphson method, given water depth and an initial deep-water wave
number estimate.

## Usage

``` r
newtonk(h, ko)
```

## Arguments

- h:

  Numeric. Water depth (in meters).

- ko:

  Numeric. Deep-water wave number estimate (radians per meter).

## Value

Numeric. Converged wave number `k` (radians per meter).

## Details

This function iteratively solves the dispersion relation: \$\$k =
\frac{\omega^2}{g \tanh(kh)}\$\$ using Newton-Raphson iteration. The
input `ko` is used as an initial guess and refined to yield a more
accurate wave number accounting for finite depth. The method terminates
if the relative change in successive estimates is below `1e-6` or after
20 iterations.
