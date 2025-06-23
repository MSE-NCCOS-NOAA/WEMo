wind_rose <- function(wind_data){
  # Extract calm wind percentage
  calm_data <- wind_data %>% dplyr::filter(is.na(direction))
  calm_pct <- if (nrow(calm_data) > 0) round(calm_data$proportion, 1) else 0

  wind_data_long <- wind_data %>%
    # mutate(directions = ifelse(wind_direction == 360, 0, wind_direction)) %>%
    dplyr::arrange(direction) %>%
    dplyr::filter(!is.na(direction)) %>%
    tidyr::pivot_longer(
      cols = -c(direction, n, proportion),
      names_to = "variable",
      values_to = "value"
    )

  # Create wind rose plot
  p <- wind_data %>%
    dplyr::filter(!is.na(direction)) %>%
    ggplot(aes(as.factor(direction), proportion/100, fill = speed))+
      geom_col()+
      scale_x_discrete(breaks = seq(0, 350, by = 90),
                       labels = c("N", "E", "S", "W"), drop = FALSE) +
      scale_y_continuous(breaks = (0:10)/100, labels = scales::percent, expand = expansion(mult = c(0.03, 0)))+
      # facet_wrap(~variable)+
      coord_polar(start =  2 * pi -pi / 36, direction=1)+
      labs(x = NULL,
           y = "Percent Time Wind Blows from Direction",
           fill = "Wind Speed",
           caption = paste0("calm winds account for ", calm_pct, "% of the record"))

  return(p)
}
