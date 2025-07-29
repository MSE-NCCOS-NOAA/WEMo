library(dplyr)
library(gglogo)
library(sf)
library(extrafont)
library(ggplot2)
extrafont::font_import()
y
extrafont::loadfonts()
fonts()

font_active <- "Courier New"
sticker_fonts = c(
  "Ubuntu",
  "Courier New",
  "Fira Sans",
  "Fira Code",
  "Roboto",
  "Lato",
  "Cooper Black",
  'Arial Black',
  'Shrikhand'

)
for(font_active in sticker_fonts){

  # Step 1: Create polygons from text
  polygons <- createPolygons(c("W", "E", "M", "o"), font = font_active)

  # ggplot(polygons, aes(x, y))+
  #   geom_polygon(color = 'black', fill = NA)+
  #   facet_wrap(pathGroup~.)

  easting = 347040.79
  northing = 3842840.45
  x_spacing = 2.5
  polygons_df <- polygons %>%
    mutate(x = x*100 + easting,
           y = y*100 + northing)

  grp_name = "W"
  max_x <- filter(polygons_df, group == grp_name) %>%
    pull(x) %>%
    max()

  for(grp_name  in unique(polygons_df$group)[2:4]){
    min_x <- filter(polygons_df, group == grp_name ) %>%
      pull(x) %>%
      min()

    polygons_df <- polygons_df %>%
      mutate(x = ifelse(group == grp_name , x - min_x + max_x + x_spacing, x))

    max_x <- filter(polygons_df, group == grp_name ) %>%
      pull(x) %>%
      max()
  }
  polygons_df %>% filter(is.na(x))
  # Step 2: Build list of coordinate matrices per letter (group and pathGroup)
  polygons_nested <- polygons_df %>%
    group_by(pathGroup) %>%
    summarise(coords = list(as.matrix(cbind(x, y))), .groups = "drop")

  # Step 3: Convert to sf POLYGONs
  polygons_sf <- polygons_nested %>%
    mutate(geometry = st_sfc(lapply(coords, function(m) st_polygon(list(m))))) %>%
    st_as_sf(crs = 32618)

  library(ggplot2)
  library(tidyterra)

  PI_bathy <- terra::rast(system.file("extdata", "PI_bathy.tif", package = "WEMo"))
  # Step 2: Buffer by 1000 meters
  polygons_buffer <- terra::buffer(terra::vect(polygons_sf), width = 350)

  # Step 3: Crop raster to buffer extent
  PI_bathy_crop <- terra::crop(PI_bathy, polygons_buffer)
  # PI_bathy_crop[PI_bathy_crop$Bathymetry_mNAVD88 > 0] <- NA
  # ggplot() +
  #   geom_spatraster(data = PI_bathy_crop) +
  #   # geom_sf(data = polygons_sf, fill = NA, color = 'black', linewidth = 1.5) +
  #   geom_sf(data = polygons_sf, fill = 'white', color = NA) +
  #   scale_fill_viridis_c(option = "C", na.value = 'gray')
  #

  # polygons_sf_wgs <- st_transform(polygons_sf, crs = "WGS84")
  library(leaflet)
  #
  # leaflet() %>%
  #   addProviderTiles("CartoDB.Positron") %>%
  #   addPolygons(data = polygons_sf_wgs,
  #               fillColor = "blue",
  #               fillOpacity = 0.7,
  #               color = "black",
  #               weight = 1,
  #               popup = ~pathGroup
  #   )


  # Hexagon -----------------------------------------------------------------

  height <- 500              # total height in meters
  radius <- height / 2       # distance from center to top/bottom

  center <- st_coordinates(st_centroid(st_union(polygons_sf)))[1:2]

  # Calculate hexagon coordinates (pointy-topped)
  angles <- seq(0, 300, by = 60)  # degrees
  angles_rad <- (angles + 90) * pi / 180  # rotate so point is at top

  # Compute x,y coordinates
  hex_coords <- cbind(
    center[1] + radius * cos(angles_rad),
    center[2] + radius * sin(angles_rad)
  )

  # Close the polygon
  hex_coords <- rbind(hex_coords, hex_coords[1, ])

  # Create sf polygon
  hexagon_sf <- st_sf(
    geometry = st_sfc(st_polygon(list(hex_coords))),
    crs = 32618
  )

  # hexagon_wgs <- hexagon_sf %>%
  #   st_transform(crs = "WGS84")
  #
  # leaflet() %>%
  #   addProviderTiles("CartoDB.Positron") %>%
  #   addPolygons(data = polygons_sf_wgs,
  #               fillColor = "blue",
  #               fillOpacity = 0.7,
  #               color = "black",
  #               weight = 1,
  #               popup = ~pathGroup
  #   ) %>%
  #   addPolygons(data = hexagon_wgs)

  # hex save  ---------------------------------------------------------------
  # ggplot() +
  #   geom_spatraster(data = PI_bathy_crop) +
  #   # geom_sf(data = polygons_sf, fill = NA, color = 'black', linewidth = 1.5) +
  #   geom_sf(data = polygons_sf, fill = 'white', color = NA) +
  #   geom_sf(data = hexagon_sf, fill = NA, color = "white", linewidth = 1.5) +
  #   scale_fill_viridis_c(option = "mako", na.value = "black") +
  #   theme_void()

  library(sf)

  # CRS and center
  crs_use <- 32618
  center <- st_coordinates(st_centroid(st_union(polygons_sf)))[1:2]

  # Outer square: 700m x 700m
  half_side <- 260
  square_coords <- matrix(c(
    center[1] - half_side, center[2] - half_side,
    center[1] + half_side, center[2] - half_side,
    center[1] + half_side, center[2] + half_side,
    center[1] - half_side, center[2] + half_side,
    center[1] - half_side, center[2] - half_side
  ), ncol = 2, byrow = TRUE)

  # Hexagon hole: height = 500m
  hex_height <- 500
  hex_radius <- hex_height / 2

  hex_coords <- function(center, radius) {
    angles <- seq(0, 300, by = 60) + 90  # Rotate so pointy-top
    angles_rad <- angles * pi / 180
    cbind(
      center[1] + radius * cos(angles_rad),
      center[2] + radius * sin(angles_rad)
    )
  }
  hex_ring <- rbind(hex_coords(center, hex_radius), hex_coords(center, hex_radius)[1, ])
  hex_ring <- hex_ring[nrow(hex_ring):1, ]  # Reverse for hole

  # Build polygon with hole
  poly_with_hole <- st_sf(
    geometry = st_sfc(st_polygon(list(square_coords, hex_ring))),
    crs = crs_use
  )

  # Plot
  # plot(poly_with_hole$geometry, col = "khaki", border = "black", asp = 1)

  # ggplot() +
  #   geom_spatraster(data = PI_bathy_crop, show.legend = F) +
  #   # geom_sf(data = polygons_sf, fill = NA, color = 'black', linewidth = 1.5) +
  #   geom_sf(data = polygons_sf, fill = 'white', color = NA) +
  #   geom_sf(data = fetch_sf) +
  #   geom_sf(data = shoreline, color = "white", fill = NA) +
  #   geom_sf(data = poly_with_hole, fill = 'black', color = "white", linewidth = 1.5) +
  #   scale_fill_viridis_c(option = "D", na.value = "black") +
  #   theme_void()

  library(WEMo)
  shoreline <- generate_shoreline_from_bathy(bathy = PI_bathy_crop, contour = 0) %>%
    st_transform(crs = st_crs(polygons_sf))

  union_polys <- st_union(polygons_sf, poly_with_hole,)
  union_polys <- st_union(union_polys, shoreline)

  # leaflet() %>%
  #   addProviderTiles("CartoDB.Positron") %>%
  #   addPolygons(data = union_polys %>% st_transform(crs = "WGS84"))

  grid_points <- generate_grid_points(site_point = poly_with_hole, expansion_dist = 0, resolution = c(150, 150))
  # grid_points <- generate_grid_points2(site_point = poly_with_hole, expansion_dist = 500, resolution = c(175,175))
  #
  # leaflet() %>%
  #   addProviderTiles("CartoDB.Positron") %>%
  #   addPolygons(data = poly_with_hole %>% st_transform(crs = "WGS84")) %>%
  #   leaflet::addCircleMarkers(data = grid_points %>% st_transform(crs = "WGS84"))

  # fetch_sf <- find_fetch(site_points = grid_points, polygon_layer = union_polys, directions = seq(0, 350, by = 10), max_fetch = 500)

  logo_bathy = terra::crop(PI_bathy_crop, hexagon_sf)
  shoreline_logo <- st_crop(shoreline, poly_with_hole)

  # ggplot() +
  #   geom_spatraster(data = logo_bathy, show.legend = F) +
  #   # geom_sf(data = polygons_sf, fill = NA, color = 'black', linewidth = 1.5) +
  #   geom_sf(data = polygons_sf, fill = 'white', color = NA) +
  #   geom_sf(data = fetch_sf, alpha = 1, linewidth = .1, aes(color = fetch), show.legend = F) +
  #   # geom_sf(data = grid_points, color = 'red')+
  #   geom_sf(data = shoreline_logo, color = "black", fill = NA) +
  #   geom_sf(data = poly_with_hole, fill = 'black', color = "white", linewidth = 1.5) +
  #   # scale_fill_viridis_c(option = "D", na.value = "black") +
  #   # scale_fill
  #   scale_color_viridis_c(option = "H")+
  #   theme_void()

  # wemo run ----------------------------------------------------------------

  results <- wemo_full(
    site_point = grid_points,
    shoreline = union_polys,
    bathy = PI_bathy_crop,
    wind_data = summarize_wind_data(
      wind_data = PI_wind_data,
      wind_percentile = 0.99,
      directions = seq(0, 350, by = 10)
    ),
    directions = seq(0, 350, by = 10),
    max_fetch = 1000,
    water_level = 0
  )

 p <- ggplot() +
    geom_spatraster(data = logo_bathy, show.legend = F) +
    geom_sf(data = results$wemo_details, aes(color = wave_height_final), show.legend = F, linewidth = 1)+
    # geom_sf(data = results$wemo_final, color = 'black')+
    geom_sf(data = polygons_sf, fill = NA, color = 'black', linewidth = 1.5) +
    geom_sf(data = polygons_sf, fill = 'white', color = NA) +
    geom_sf(data = shoreline_logo, color = "black", fill = NA, linewidth = .75) +
    geom_sf(data = poly_with_hole, fill = 'white', color = "black", linewidth = 2) +
    theme_void() +
    scale_fill_gradient(low = "black", high = "white")+
    scale_color_viridis_c(option = 'H')

  ggsave(plot = p, filename = paste0("./logos/", font_active, ".png"), width = 6.5, height = 6.5, units = 'in', dpi = 330)

}
