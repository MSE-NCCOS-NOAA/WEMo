test_that("build_wind_wave handles wind_speed = 0", {
  # Setup valid mock inputs for fetch, depths, and distances
  mock_fetch <- 100
  mock_depths <- c(5, 4, 3)
  mock_distances <- c(50, 50)

  # 1. Test wind_speed exactly equal to 0
  result <- build_wind_wave(
    fetch = mock_fetch,
    depths = mock_depths,
    distances = mock_distances,
    wind_speed = 0
  )

  # Expect a clean dataframe structure back
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # All physical metrics should evaluate cleanly to zero
  expect_equal(result$wave_height_final, 0)
  expect_equal(result$WEI, 0)
  expect_equal(result$wave_period, 0)
  expect_equal(result$wave_number, 0)
  expect_equal(result$celerity_final, 0)
  expect_equal(result$nnumber_final, 0)
})

test_that("build_wind_wave handles wind_speed < 0", {
  # Setup valid mock inputs for fetch, depths, and distances
  mock_fetch <- 100
  mock_depths <- c(5, 4, 3)
  mock_distances <- c(50, 50)

  # Test negative wind_speed
  result <- build_wind_wave(
    fetch = mock_fetch,
    depths = mock_depths,
    distances = mock_distances,
    wind_speed = -1*runif(1, 0, 100)
  )

  # All physical metrics should evaluate cleanly to zero
  expect_equal(result$wave_height_final, 0)
  expect_equal(result$WEI, 0)
  expect_equal(result$wave_period, 0)
  expect_equal(result$wave_number, 0)
  expect_equal(result$celerity_final, 0)
  expect_equal(result$nnumber_final, 0)
})

test_that("build_wind_wave handles wind_speed > 0 but < 0.01", {
  # Setup valid mock inputs for fetch, depths, and distances
  mock_fetch <- 100
  mock_depths <- c(5, 4, 3)
  mock_distances <- c(50, 50)

  # Test very small wind_speed
  result <- build_wind_wave(
    fetch = mock_fetch,
    depths = mock_depths,
    distances = mock_distances,
    wind_speed = runif(1, 0, 0.01)
  )

  # All physical metrics should evaluate cleanly to zero
  expect_true(result$wave_period > 0)
  expect_true(result$wave_number > 0)
  expect_true(result$celerity_final > 0)
  expect_true(result$nnumber_final > 0)
})

test_that("build_wind_wave handles wind_speed = NA", {
  # Setup valid mock inputs for fetch, depths, and distances
  mock_fetch <- 100
  mock_depths <- c(5, 4, 3)
  mock_distances <- c(50, 50)

  # Test very small wind_speed
  result <- build_wind_wave(
    fetch = mock_fetch,
    depths = mock_depths,
    distances = mock_distances,
    wind_speed = NA
  )

  # All physical metrics should evaluate cleanly to zero
  expect_equal(result$wave_height_final, NA)
  expect_equal(result$WEI, NA)
  expect_equal(result$wave_period, NA)
  expect_equal(result$wave_number, NA)
  expect_equal(result$celerity_final, NA)
  expect_equal(result$nnumber_final, NA)
})
