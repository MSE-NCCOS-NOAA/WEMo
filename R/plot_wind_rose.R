#' Plot a Wind Rose Diagram
#'
#' Generates a wind rose plot where the length of bars corresponds to the
#' duration that wind is blowing from a direction and the color to the wind
#' speed.
#'
#' @param wind_data A data frame containing summarized wind data. It must
#'   include:
#'   \describe{
#'     \item{`direction`}{Wind direction in degrees (0-359), with `NA` indicating calm winds.}
#'     \item{`proportion`}{Proportion of time the wind blew from that direction (as a percentage, 0-100).}
#'     \item{`speed`}{Wind speed value to use for color fill.}
#'   }
#'
#' @return A ggplot2 object of the wind rose plot.
#'
#' @details The function filters out rows where direction is `NA` for the plot,
#'   but still uses them to calculate and display the percent of calm winds in
#'   the caption.
#'
#'   Wind direction labels are displayed as compass directions (N, E, S, W), and
#'   the plot is rendered in polar coordinates.
#'
#' @examples
#' require(ggplot2)
#' set.seed(1)  # for reproducibility
#'
#' # Create random wind data
#' n_directions = 16
#' wind_data <- data.frame(
#'   direction = c(NA, seq(0, 360,  length.out = n_directions+1)[-(n_directions+1)]),
#'   proportion = runif(n_directions + 1, min = 0.5, max = 5),
#'   speed = runif(n_directions + 1, min = 0.1, max = 5)
#'  ) %>%
#'  dplyr::mutate(proportion = proportion/sum(proportion)*100)
#'
#'  wind_rose_plot <- plot_wind_rose(wind_data)
#'
#'  # view the plot
#'  wind_rose_plot
#'
#'  # can add your own labels with the
#'  wind_rose_plot +
#'   labs(
#'     title = "Wind at My Site",
#'     subtitle = "2000-2025",
#'     fill = "95th %tile wind speed"
#'    )
#'
#'  # Change the color scale
#'  wind_rose_plot + scale_fill_continuous(type = 'viridis')
#' @export
#'
#' @importFrom ggplot2 geom_col scale_x_discrete scale_y_continuous coord_polar labs aes ggplot expansion ggplot_build
#' @importFrom scales percent
#'
plot_wind_rose <- function(wind_data){
  # Extract calm wind percentage
  calm_data <-  dplyr::filter(wind_data, is.na(.data$direction))
  calm_pct <- if (nrow(calm_data) > 0) round(calm_data$proportion, 1) else 0

  wind_data <- dplyr::filter(wind_data, !is.na(.data$direction))
  n_directions <- nrow(wind_data)

  # Create wind rose plot
  rose_plot <- wind_data %>%
    dplyr::arrange(.data$direction) %>%
    ggplot(aes(as.factor(.data$direction), .data$proportion/100, fill = .data$speed))+
    geom_col()+
    scale_x_discrete(breaks = seq(0, 270, by = 90),
                     labels = c("N", "E", "S", "W"), drop = FALSE) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.03, 0)))+
    coord_polar(start = 2 * pi -pi / n_directions, direction=1)+
    labs(x = NULL,
         y = "Percent Time Wind Blows from Direction",
         fill = "Wind Speed",
         caption = paste0("calm winds account for ", calm_pct, "% of the record"))

  built_plot <- ggplot_build(rose_plot)

  suppressMessages(
    rose_plot <- rose_plot +
      scale_y_continuous(
        limits = built_plot$layout$panel_params[[1]]$r.range,
        breaks = built_plot$layout$panel_params[[1]]$r.major,
        labels = scales::percent,
        expand = expansion(mult = c(0.03, 0))
      )
  )

  # putting this dummy code here to allow tidyterra to be required. I want it in the vignettes, but want user to not have to download it
  if(FALSE){
    tidyterra::geom_spatraster()
  }

  return(rose_plot)
}
