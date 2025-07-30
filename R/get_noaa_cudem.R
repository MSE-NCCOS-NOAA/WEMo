#' Download NOAA CUDEM (Continuously Updated DEM) Topo-Bathy Rasters
#'
#' Download (and optionally make into a cropped mosaic raster)
#' Bathymetric-Topographic rasters files from from NOAA's Continuously Updated
#' Digital Elevation Model (CUDEM) near a specified location. Rasters are at
#' Ninth Arc-Second Resolution.
#'
#' @details NOAA's CUDEM is a large raster layer that is broken into smaller
#'   rasters called tiles. NOAA provides a tile index file that defines the
#'   boundaries of these tiles and allows users to identify and download the
#'   relevant tiles for their uses. The function identifies relevant tiles by
#'   their proximity (intersecting with a buffer around the point of radius
#'   `radius_m`) to the provided location. Rasters are downloaded to the user
#'   defined directory `dest_dir`.
#'
#'   If `mosaic_and_crop` is `TRUE`, downloaded rasters are stitched together in
#'   a mosaic raster and then cropped to a square centered on the provided point
#'   with with edges of length two times `radius_m`. This cropped mosaic raster
#'   is saved to `det_dir` with filename `output_file`. If `mosaic_and_crop` is
#'   `FALSE`, The individual tiles are downloaded only.
#'
#' @param lon Longitude in decimal degrees (WGS84).
#' @param lat Latitude in decimal degrees (WGS84).
#' @param radius_m Radius (in meters) around the point within which to search
#'   for tiles.
#' @param dest_dir Directory to save downloaded files. Created if it does not
#'   exist. Default is `"NOAA_CUDEM"`.
#' @param mosaic_and_crop Logical. If `TRUE` (default), mosaics and crops the
#'   downloaded rasters to the buffer. If FALSE, only downloads intersecting
#'   tiles and returns their file paths.
#' @param output_file Optional file path to save the cropped mosaic. If NULL, no
#'   file is written.
#' @param overwrite Logical. If `TRUE`, re-download existing tiles and overwrite
#'   existing output file.
#' @param plot Logical. If `TRUE`, generates an interactive map to display the
#'   relation between the available CUDEM data, the central point defined by
#'   `lon` and `lat` and the buffer created by `radius_m`. This calls
#'   [`map_noaa_cudem()`]
#' @param cleanup_source_files Logical. If `TRUE` and `mosaic_and_crop` is also
#'   `TRUE`, the original downloaded source tiles will be deleted after the
#'   mosaic raster is created. Defaults to `FALSE`.
#'
#' @section Geographic Coverage: The CUDEM dataset accessed by this function
#'   covers select areas of the United States, including:
#' - The East Coast
#' - Gulf Coast
#' - San Francisco Bay
#' - Part of the Oregon Coast
#' - Puget Sound and the Washington Coast
#' - Portions of Cook Inlet, Alaska
#'
#'   Other CUDEM products available through NOAA cover additional U.S. states
#'   and territories, including:
#' - Hawaii
#' - Guam
#' - American Samoa
#' - Puerto Rico and the U.S. Virgin Islands
#' - Northern Mariana Islands
#'
#'   Additional CUDEM products, with varying spatial resolutions and geographic
#'   extents, are available at:
#'   \url{https://www.ncei.noaa.gov/products/coastal-elevation-models}
#'
#' @section Data Source and Citation: This function accesses data from NOAA's
#'   Continuously Updated Digital Elevation Model (CUDEM), specifically the
#'   Ninth Arc-Second Topobathymetric dataset:
#'   \url{https://chs.coast.noaa.gov/htdata/raster2/elevation/NCEI_ninth_Topobathy_2014_8483/}
#'
#'   Cite as: Cooperative Institute for Research in Environmental Sciences
#'   (CIRES) at the University of Colorado, Boulder. 2014: Continuously Updated
#'   Digital Elevation Model (CUDEM) - 1/9 Arc-Second Resolution
#'   Bathymetric-Topographic Tiles. `[Indicate subset used]`. NOAA National
#'   Centers for Environmental Information.
#'   \url{https://doi.org/10.25921/ds9v-ky35}. Accessed `[date]`.
#'
#' @return A `terra::SpatRaster` if `mosaic_and_crop = TRUE`; otherwise
#'   Invisibly returns `TRUE` if downloads completed, or `NULL` if no tiles were
#'   within the radius.
#'
#' @export
#'
get_noaa_cudem <- function(lon, lat, radius_m,
                           dest_dir = "NOAA_CUDEM",
                           mosaic_and_crop = TRUE,
                           output_file = "Cropped_Mosaic_Raster.tif",
                           overwrite = FALSE,
                           plot = TRUE,
                           cleanup_source_files = FALSE
                           ) {
  # Check input types
  stopifnot("lon, lat, and radius_m must be numeric" = is.numeric(lon), is.numeric(lat), is.numeric(radius_m))

  # map the avaiable tiles if the user wants it
  if(plot == TRUE){
    map_noaa_cudem(lon = lon, lat = lat, radius_m = radius_m)
  }

  # Load tile index from package extdata
  tile_path <- system.file("extdata", "NOAA_CUDEM_tiles.shp", package = "WEMo")
  if (tile_path == "") stop("Tile index file not found in package data.")

  tile_index <- sf::st_read(tile_path, quiet = TRUE)

  # Create sf point in WGS84
  pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)

  # Transform to match tile CRS
  pt <- sf::st_transform(pt, sf::st_crs(tile_index))

  buffer <- sf::st_buffer(pt, dist = radius_m)

  # Subset tiles that intersect buffer
  intersecting_tiles <- tile_index[sf::st_intersects(tile_index, buffer, sparse = FALSE), ]

  if (nrow(intersecting_tiles) == 0) {
    message("No tiles found within the specified radius.")
    return(invisible(NULL))
  }

  # Prepare download directory
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  # Download relevant tiles
  file_paths <- character(nrow(intersecting_tiles))
  for (i in seq_len(nrow(intersecting_tiles))) {
    url <- intersecting_tiles$url[i]
    filename <- intersecting_tiles$location[i]
    dest_file <- file.path(dest_dir, filename)
    file_paths[i] <- dest_file

    if (!overwrite && file.exists(dest_file)) {
      message(sprintf("Skipping existing file: %s", filename))
    } else {
      message(sprintf("Downloading: %s", url))
      tryCatch(
        utils::download.file(url, destfile = dest_file, mode = "wb", quiet = TRUE),
        error = function(e) {
          warning(sprintf("Failed to download %s: %s", url, e$message))
        }
      )
    }
  }
  message("Download complete")
  # Stop here if user only wants downloaded tiles
  if (!mosaic_and_crop) {
    return(invisible(file_paths[file.exists(file_paths)]))
  }

  # Load and mosaic the downloaded rasters
  rasters <- lapply(file_paths[file.exists(file_paths)], terra::rast)
  if (length(rasters) == 0) stop("No valid raster files were loaded. Erorr Downloading and Storing.")

  if(length(rasters) > 1){
    mosaic_bathy <- do.call(terra::mosaic, rasters)
  } else{
    mosaic_bathy <- rasters[[1]]
  }

  buffer_vect <- terra::vect(buffer)

  # Crop to buffer
  if (!is.null(output_file)) {
    # Write output if requested
    cropped <- terra::crop(mosaic_bathy, buffer_vect, filename = file.path(dest_dir, output_file), overwrite = TRUE)
    message("Cropped mosaic saved to: ", file.path(dest_dir, output_file))
  } else{
    cropped <- terra::crop(mosaic_bathy, buffer_vect)
  }

  # Cleanup downloaded source tiles if requested
  if (cleanup_source_files) {
    message("Cleaning up source files...")
    # Get a list of files that actually exist before trying to remove
    files_to_remove <- file_paths[file.exists(file_paths)]
    if (length(files_to_remove) > 0) {
      file.remove(files_to_remove)
      message("Source files have been removed.")
    } else {
      message("No source files to remove.")
    }
  }

  return(cropped)
}
