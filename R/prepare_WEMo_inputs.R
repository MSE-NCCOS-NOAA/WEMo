prepare_WEMo_inputs <- function(site,
                                shoreline,
                                bathy,
                                wind_data,
                                directions,
                                max_fetch = 1000,
                                sample_dist = 5,
                                water_level,
                                depths_or_elev = 'elev',
                                extra_at_start = T ) {
  # calculate fetch
  fetch <- find_fetch(
    site_layer = site,
    polygon_layer = shoreline,
    directions = directions,
    max_fetch = max_fetch
  )

  # calculate effective fetch
  e_fetch <- effective_fetch(fetch = fetch)

  # interrogate the bathy and get list of depths and distances for each depth point
  e_fetch_with_bathy <- interrogate_bathy(
    fetch = e_fetch,
    bathy_raster = bathy,
    sample_dist = sample_dist,
    depths_or_elev = depths_or_elev,
    water_level = water_level,
    extra_at_start = extra_at_start
  )

  e_fetch_with_bathy_wind <- dplyr::left_join(e_fetch_with_bathy, wind_data)

  e_fetch_with_bathy_wind <- sf::st_as_sf(e_fetch_with_bathy_wind)

  return(e_fetch_with_bathy_wind)
}
