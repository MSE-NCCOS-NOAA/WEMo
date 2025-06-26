test_that("output directions range between 0 and 360", {
  wind_data <- data.frame(
    wind_direction = -720:720,
    wind_speed = 10
  )

  output <- summarize_wind_data(wind_data, 'mean')

  expect_true(all(output$direction<360 & output$direction>=0))
})

test_that("Sum of proportions equals 100", {
  wind_data <- data.frame(
    wind_direction = sample(c(0, 90, 180, 270), replace = TRUE, size = 100)
  )

  wind_data$wind_speed  <-  runif(nrow(wind_data), 0, 10)

  output <- summarize_wind_data(wind_data, 'mean')

  expect_equal(sum(output$proportion), 100, tolerance = 1e-6)
})


test_that("NA handling for wind_speed works", {
  wind_data <- data.frame(
    wind_direction = sample(c(0, 90, 180, 270), replace = TRUE, size = 100)
  )

  wind_data$wind_speed  <-  sample(c(0:5, NA), replace = TRUE, size = nrow(wind_data))

  num_ws_nas <- sum(is.na(wind_data$wind_speed))

  output_rm <- summarize_wind_data(wind_data, wind_percentile = "mean", wind_speed_na.rm = TRUE)
  expect_equal(sum(output_rm$n), nrow(wind_data)-num_ws_nas)

  output_keep <- summarize_wind_data(wind_data, wind_percentile = "mean", wind_speed_na.rm = FALSE)
  expect_equal(sum(output_keep$n), nrow(wind_data))
})


test_that("wind_direction is set to NA where wind_speed is 0", {
  wind_data <- data.frame(
    wind_direction = sample(c(0, 90, 180, 270), replace = TRUE, size = 100)
  )

  wind_data$wind_speed  <-  sample(c(0:5), replace = TRUE, size = nrow(wind_data))

  output <- summarize_wind_data(wind_data, 'mean')

  # same amount of NA directions as 0's in the input
  expect_equal(output$n[is.na(output$direction)], sum(wind_data$wind_speed == 0))
})


test_that("Direction binning snaps directions correctly", {
  wind_data <- data.frame(
    wind_direction = runif(100, 0, 359)
  )

  wind_data$wind_speed  <-  sample(c(1:5), replace = TRUE, size = nrow(wind_data))

  wanted_directions <- sort(round(runif(8, 0, 359)))

  output <- summarize_wind_data(wind_data, 'mean', directions = wanted_directions)

  expect_equal(unique(wanted_directions), output$direction)
})

test_that("Direction binning snaps directions correctly even with directions that aren't in the data set", {
  wind_data <- data.frame(
    wind_direction = runif(100, 0, 180)
  )

  wind_data$wind_speed  <-  sample(c(1:5), replace = TRUE, size = nrow(wind_data))

  wanted_directions <- seq(0, 360, by = 45)

  output <- summarize_wind_data(wind_data, 'mean', directions = wanted_directions)

  expect_equal(unique(wanted_directions), output$direction)
})
