test_that("interrogate_bathy issues a specific warning for NA values", {
  # Create a small bathymetry raster (100m x 100m)
  bathy_raster <- terra::rast(
    xmin = 334000, xmax = 334100, ymin = 3845000, ymax = 3845100,
    resolution = 10,
    crs = "EPSG:32618"
  )

  terra::values(bathy_raster) <- 1:100

  # Create a fetch data frame with one ray that goes outside the raster
  fetch_data <- data.frame(
    site = "A",
    direction = 135,
    geometry = sf::st_sfc(sf::st_linestring(matrix(c(334050, 3845050, 334120, 3845120), ncol = 2, byrow = TRUE)))
  )

  fetch_sf <- sf::st_as_sf(fetch_data, crs = sf::st_crs(bathy_raster))

  expect_warning(
    WEMo:::interrogate_bathy(fetch = fetch_sf, bathy_raster = bathy_raster, sample_dist = 10),
    regexp = "NA values extracted from bathymetry at site 'A' at direction '135'."
  )
})
