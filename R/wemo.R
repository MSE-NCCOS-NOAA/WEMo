WEMo <- function(fetch, depths, distances, wind_data){

  wemo_details <- lapply(seq_along(fetch$geometry), function(i){
    bind_cols(
      fetch[i,],
      build_wind_wave(
        fetch = eff_fetch_sf_with_bathy$efetch[[i]],
        depths = eff_fetch_sf_with_bathy$depths[[i]],
        distances = eff_fetch_sf_with_bathy$distances[[i]],
        wind_speed = eff_fetch_sf_with_bathy$speed[[i]]
      )
    )
  }) %>%
    bind_rows()

  wemo_output <- wemo_details %>%
    dplyr::mutate(RWE = WEI * proportion/100,
                  site_name = as.character(site)) %>%
    dplyr::group_by(site) %>%
    dplyr::reframe(
      RWE = sum(RWE, na.rm = T),
      avg_wave_height = mean(wave_height_final, na.rm = T),
      max_wave_height = max(wave_height_final, na.rm = T),
      direction_of_max_wave = paste(list(direction[which(wave_height_final == max_wave_height)])),
      # avg_wave_period = mean(wave_period, na.rm = T),
      # max_wave_period = max(wave_period, na.rm = T)
    ) %>%
    left_join(points_sf, .)

  return(list(
    wemo_details,
    wemo_output
  ))

}
