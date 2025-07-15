#' Download NOAA NCEI CUDEM (Continuously Updated DEM)
#'
#' Downloads Bathymetric-Topographic rasters files from the NOAA NCEI
#' Continuously Updated Digital Elevation Model (CUDEM) – Ninth Arc-Second
#' Resolution. The tiles are selected based on proximity to a user-defined point
#' (longitude/latitude) and radius (in meters), and intersecting tiles are
#' downloaded via their individual URLs stored in a built-in tile index.
#'
#' A tile index is used to identify proximate raster files. This file is included in this
#' package and can be downloaded separately from
#' [coast.noaa.gov](https://coast.noaa.gov/htdata/raster2/elevation/NCEI_ninth_Topobathy_2014_8483/#:~:text=com/dem/NCEI_ninth_Topobathy_2014_8483/.-,Meta%20Info,-Tile%20Index%3A)
#'
#' @section Data Source: The dataset is provided by the **NOAA National Centers
#'   for Environmental Information (NCEI)**:
#'
#' - NOAA NCEI CUDEM Info/Metadata/Download Page:
#'   \url{https://coast.noaa.gov/htdata/raster2/elevation/NCEI_ninth_Topobathy_2014_8483/}
#'
#'
#' @param lon Longitude of the center point (in decimal degrees, WGS84).
#' @param lat Latitude of the center point (in decimal degrees, WGS84).
#' @param radius_m Radius around the point, in meters, within which to download intersecting tiles.
#' @param dest_dir Directory to save downloaded files. Created if it does not exist. Default is `"NOAA_CUDEM"`.
#' @param overwrite Logical. If `TRUE`, re-download files even if they already exist locally. Default is `FALSE`.
#'
#' @return Invisibly returns `TRUE` if downloads completed, or `NULL` if no tiles were within the radius.
#'
#' Download Tiles Near a Point Within a Radius
#'
#' ...
#'
#' @importFrom sf st_read st_sfc st_point st_transform st_buffer st_intersects st_crs
#' @importFrom utils download.file
#' @export
download_NOAA_CUDEM <- function(tile_index, lon, lat, radius_m, dest_dir = "NOAA_CUDEM", overwrite = FALSE) {
  # Check input types
  stopifnot("lon, lat, and radius_m must be numeric" = is.numeric(lon), is.numeric(lat), is.numeric(radius_m))

  # Load tile index from package extdata
  tile_path <- system.file("extdata", "NOAA_CUDEM_tiles.shp", package = "WEMo")
  if (tile_path == "") stop("Tile index file not found in package data.")

  tile_index <- sf::st_read(tile_path, quiet = TRUE)

  # Create sf point in WGS84
  pt <- st_sfc(st_point(c(lon, lat)), crs = 4326) %>%
    st_transform(st_crs(tile_index))  # Transform to match tile CRS

  # Buffer the point to make a search area (in meters)
  buffered_pt <- st_buffer(pt, dist = radius_m)

  # Find tiles that intersect the buffer
  selected_tiles <- tile_index[st_intersects(tile_index, buffered_pt, sparse = FALSE), ]

  if (nrow(selected_tiles) == 0) {
    message("No tiles found within the specified radius.")
    return(invisible(NULL))
  }

  # Create destination directory if needed
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  message(sprintf("%d rasters found within %d meters of point", nrow(selected_tiles), radius_m))

  # Loop and download each selected tile
  for (i in seq_len(nrow(selected_tiles))) {
    url <- selected_tiles$url[i]
    filename <- selected_tiles$location[i]
    dest_file <- file.path(dest_dir, filename)

    if (!overwrite && file.exists(dest_file)) {
      message(sprintf("Skipping existing file: %s", filename))
      next
    }

    message(sprintf("Downloading: %s", url))
    tryCatch(
      utils::download.file(url, destfile = dest_file, mode = "wb", quiet = TRUE),
      error = function(e) {
        warning(sprintf("Failed to download %s: %s", url, e$message))
      }
    )
  }

  invisible(TRUE)
}
