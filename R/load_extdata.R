#' load_raster_extdata
#'
#' A raster of the (example).
#'
#' @param path_name Path of the raster (.tiff) file to download.
#'
#' @format An object `SpatRaster` (package terra).
#' @source Internal data package.
#' @keywords datasets
#' @export
load_raster_extdata <- function(path_name) {
  raster_path <- system.file("extdata", path_name, package = "spacemodR")
  terra::rast(raster_path)
}
