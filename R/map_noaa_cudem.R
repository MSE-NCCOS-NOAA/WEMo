#' @title Interactively Map NOAA CUDEM Tiles
#'
#' @description Generate an interactive map showing the geographic extent of all
#'   available NOAA CUDEM (Continuously Updated Digital Elevation Model) tiles.
#'   Optionally, users can specify a site location (`lon`, `lat`) and a radius
#'   (`radius_m`) to highlight relevant tiles near a point of interest.
#'
#' @param lon Optional. A numeric value representing the longitude of the center
#'   point in decimal degrees (WGS84). If not provided, the full tile index is
#'   displayed without zooming or highlighting nearby tiles.
#' @param lat Optional. A numeric value representing the latitude of the center
#'   point in decimal degrees (WGS84). Must be provided if `lon` is given.
#' @param radius_m A numeric value specifying the radius in meters to search
#'   for CUDEM tiles around the center point. Defaults to 10,000. Ignored if
#'   `lon` and `lat` are not specified.
#'
#' @return Invisibly returns `TRUE`. The primary output is an interactive
#'   `leaflet` map printed to the viewer.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Map showing only the full tile index
#'   map_noaa_cudem()
#'
#'   # Map showing tiles near a specific location
#'   longitude <- -76.67
#'   latitude <- 34.72
#'   search_radius_meters <- 5000
#'
#'   map_noaa_cudem(lon = longitude, lat = latitude, radius_m = search_radius_meters)
#' }
map_noaa_cudem <- function(lon = NULL, lat = NULL, radius_m = 10000) {
  # Load tile index from package extdata
  tile_path <- system.file("extdata", "NOAA_CUDEM_tiles.shp", package = "WEMo")
  if (tile_path == "") stop("Tile index file not found in package data.")

  tile_index <- sf::st_read(tile_path, quiet = TRUE)
  tile_index_wgs <- st_transform(tile_index, 4326)

  m <- leaflet() %>%
    addProviderTiles("OpenStreetMap") %>%
    addPolygons(
      data = tile_index_wgs,
      fillColor = "transparent",
      color = "black",
      weight = 1,
      popup = ~ paste0(
        "<strong>Location:</strong> ",
        location,
        "<br>",
        "<strong>Mission ID:</strong> ",
        missionid,
        "<br>",
        "<a href='",
        url,
        "' target='_blank'>Download Tile</a>"
      )
    )

  if(is.null(lon)|is.null(lat)){
    print(m)
    return(invisible(T))
  } else {
    # Check input types
    stopifnot("lon, lat, and radius_m must be numeric" = is.numeric(lon), is.numeric(lat), is.numeric(radius_m))

    # Create sf point in WGS84
    pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)

    # Transform to match tile CRS
    pt <- sf::st_transform(pt, sf::st_crs(tile_index))
    buffer <- sf::st_buffer(pt, dist = radius_m)

    # Subset tiles that intersect buffer
    intersecting_tiles <- tile_index[sf::st_intersects(tile_index, buffer, sparse = FALSE), ]

    # Transform everything to WGS84 for leaflet
    pt_wgs <- st_transform(pt, 4326)
    buffer_wgs <- st_transform(buffer, 4326)
    intersecting_tiles_wgs <- st_transform(intersecting_tiles, 4326)

    # Get bounding box of buffer
    bbox <- st_bbox(buffer_wgs)

    # Create leaflet map
    m <- m %>%
      addCircleMarkers(
        data = pt_wgs,
        radius = 5,
        color = "red",
        fill = TRUE,
        fillOpacity = 1
      ) %>%
      addPolygons(
        data = intersecting_tiles_wgs,
        fillColor = "transparent",
        color = "cyan",
        weight = 5,
        popup = ~ paste0(
          "<strong>Location:</strong> ",
          location,
          "<br>",
          "<strong>Mission ID:</strong> ",
          missionid,
          "<br>",
          "<a href='",
          url,
          "' target='_blank'>Download Tile</a>"
        )
      )  %>%
      fitBounds(
        lng1 = bbox[["xmin"]],
        lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]],
        lat2 = bbox[["ymax"]]
      )

    print(m)
    return(invisible(T))
  }
}
