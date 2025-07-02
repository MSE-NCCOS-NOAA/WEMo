#' Example Site Points for WEMo
#'
#' Point data to be used in examples from wemo package
#'
#' @format ## `PI_points`
#' A Simple feature collection with 3 features and 2 fields
#' \describe{
#'   \item{FID}{feature ID number}
#'   \item{site}{site number}
#'   ...
#' }
"PI_points"

#' Example Shoreline Polygon for WEMo
#'
#' A polygon enclosing land areas around Pivers Island NC. The boundary was
#' drawn at 0.552 m NAVD88 which is MHHW for the area
#'
#' @format ## `PI_shoreline`
#' A Simple feature collection with 3 features and 2 fields
#' \describe{
#'   \item{FID}{feature ID number}
#'   \item{site}{site number}
#'   ...
#' }
"PI_shoreline"

#' Example Wind Data for WEMo examples
#'
#' Wind history data from
#'
#' @format ## `PI_wind_data`
#' A tibble with 184,947 rows and 73 columns
#' \describe{
#'   \item{code}{the ISD station code}
#'   \item{time}{datetime when the observation was made}
#'   \item{year}{year when the observation was made}
#'   \item{month}{month when the observation was made}
#'   \item{day}{day when the observation was made}
#'   \item{wind_direction}{direction from which the wind blows. degrees off north}
#'   \item{speed}{speed of the wind. meters per second}
#'   ...
#' }
#'
#' @source created by:
#' get_wind_data(
#'  site_point = NULL,
#'  years = 2023:2024,
#'  which_station = "723090-13754"
#' )
"PI_wind_data"

#' Example Wind Data for WEMo examples
#'
#' Wind history data from
#'
#' @format ## `wemo_points`
#' A tibble with 184,947 rows and 73 columns
#' \describe{
#'   \item{code}{the ISD station code}
#'   \item{time}{datetime when the observation was made}
#'   \item{year}{year when the observation was made}
#'   \item{month}{month when the observation was made}
#'   \item{day}{day when the observation was made}
#'   \item{wind_direction}{direction from which the wind blows. degrees off north}
#'   \item{speed}{speed of the wind. meters per second}
#'   ...
#' }
#'
#' @source continuously updated topobathy from NOAA - need more info here
"PI_bathy"
