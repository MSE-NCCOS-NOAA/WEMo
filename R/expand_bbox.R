expand_bbox <- function(bbox, X, Y, X2 = X, Y2 = Y,
                        crs_out = 4326) {
  # bbox <- bbox %>%
  #   st_as_sfc() %>%
  #   sf::st_transform(crs = 4326) %>%
  #   st_bbox()


  bbox["xmin"] <- bbox["xmin"] - X
  bbox["xmax"] <- bbox["xmax"] + X2
  bbox["ymin"] <- bbox["ymin"] - Y
  bbox["ymax"] <- bbox["ymax"] + Y2

  bbox %>%
    st_as_sfc() %>%
    # sf::st_transform(crs = crs_out) %>%
    st_bbox()
  return(bbox)
}
