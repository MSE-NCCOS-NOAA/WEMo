#' Plot a Wind Rose Diagram
#'
#' Generates a polar bar plot (wind rose) showing the distribution of wind
#' directions and corresponding wind speeds.
#'
#' @param wind_data A data frame containing summarized wind data. It must
#'   include:
#'   \describe{
#'     \item{\code{direction}}{Wind direction in degrees (0–359), with \code{NA} indicating calm winds.}
#'     \item{\code{proportion}}{Proportion of time the wind blew from that direction (as a percentage, 0–100).}
#'     \item{\code{speed}}{Wind speed value to use for color fill.}
#'   }
#'
#' @return A ggplot2 object showing the wind rose.
#'
#' @details The function filters out rows where direction is \code{NA} for the
#' plot, but still uses them to calculate and display the percent of calm winds
#' in the caption.
#'
#' Wind direction labels are displayed as compass directions (N, E, S, W), and
#' the plot is rendered in polar coordinates.
#'
#' @examples
#' require(ggplot2)
#' set.seed(1)  # for reproducibility
#'
#' # Create random wind data
#' n_directions = 36
#' wind_data <- data.frame(
#'   direction = c(NA, seq(0, 350,  length.out = n_directions)),
#'   proportion = runif(n_directions + 1, min = 0.5, max = 5),
#'   speed = runif(n_directions + 1, min = 0.1, max = 5)
#'  )
#'
#'  wind_rose_plot <- wind_rose(wind_data)
#'
#'  # view the plot
#'  wind_rose_plot
#'
#'  # can add your own labels with the
#'  wind_rose_plot +
#'   labs(
#'     title = "Wind at My Site",
#'     subtitle = "2000-2025",
#'     fill = "95th %tile\nwind speed"
#'    )
#'
#'  # Change the color scale
#'  wind_rose_plot + scale_fill_continuous(type = 'viridis')
#'
#' @export
wind_rose <- function(wind_data){
  # Extract calm wind percentage
  calm_data <-  dplyr::filter(is.na(wind_data$direction))
  calm_pct <- if (nrow(calm_data) > 0) round(calm_data$proportion, 1) else 0

  n_directions <- nrow(dplyr::filter(!is.na(wind_data$direction)))

  # Create wind rose plot
  p <- dplyr::filter(!is.na(wind_data$direction)) %>%
    dplyr::arrange(.data$direction) %>%
    ggplot(aes(as.factor(.data$direction), .data$proportion/100, fill = .data$speed))+
      geom_col()+
      scale_x_discrete(breaks = seq(0, 350, by = 90),
                       labels = c("N", "E", "S", "W"), drop = FALSE) +
      scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.03, 0.05)))+
      coord_polar(start = 2 * pi -pi / n_directions, direction=1)+
      labs(x = NULL,
           y = "Percent Time Wind Blows from Direction",
           fill = "Wind Speed",
           caption = paste0("calm winds account for ", calm_pct, "% of the record"))

  return(p)
}

