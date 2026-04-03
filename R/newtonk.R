#' Compute Wave Number via Newton-Raphson Iteration
#'
#' Solves the wave dispersion equation for wave number (`k`) using the
#' Newton-Raphson method, given water depth and an initial deep-water wave number estimate.
#'
#' @param h Numeric. Water depth (in meters).
#' @param ko Numeric. Deep-water wave number estimate (radians per meter).
#'
#' @return Numeric. Converged wave number `k` (radians per meter).
#'
#' @details
#' This function iteratively solves the dispersion relation:
#' \deqn{k = \frac{\omega^2}{g \tanh(kh)}}
#' using Newton-Raphson iteration. The input `ko` is used as an initial guess
#' and refined to yield a more accurate wave number accounting for finite depth.
#' The method terminates if the relative change in successive estimates is below `1e-6`
#' or after 20 iterations.
#'
#'@keywords internal
newtonk <- function(h, ko) {
  eps <- 1e-6

  # Prevent pathological depths
  h <- max(h, 0.001)

  # Initial guess
  k <- ko * tanh(ko * h)

  for (i in 1:20) {
    kh <- k * h

    tanh_kh <- tanh(kh)
    sech2_kh <- 1 / cosh(kh)^2

    f  <- ko - k * tanh_kh
    fp <- -tanh_kh - k * h * sech2_kh

    # Guard against divide-by-zero
    if (abs(fp) < 1e-10) break

    kn <- k - f / fp

    # Prevent negative or zero k
    if (kn <= 0) kn <- k / 2

    # Convergence check
    if (abs(kn - k) < eps * max(1, k)) {
      return(kn)
    }

    k <- kn
  }

  return(k)
}
