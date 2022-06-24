#' Shapefile in the rough shape of Israel
#'
#' A shapefile used to mask the vulture data to exclude points outside of Israel.
#'
#' @format An sf object with 1 feature and 2 fields, type POLYGON
#' \describe{
#'   \item{Regional_polygon}{approximate extent of the Israel area}
#'   ...
#' }
#' @source \url{}
"mask"

#' CRS for Israel shapefile
#'
#' The CRS for the Israel shapefile, saved separately because it doesn't seem to load properly...
#'
#' @format A crs object
#' \describe{
#'   \item{}{}
#'   ...
#' }
#' @source \url{}
"israelCRS"

