#' Create a raster mask from a habitat object
#'
#' @param spacemodel spacemodel.
#' @param layer layer of the \code{spacemodel}.
#' @param method method use for dispersal. Default is "convolution".
#' @param method_option parameters to be used in `method`.
#'
#' @return spacemod with modification of the layer
#'
#' @export
dispersal <- function(spacemodel,
                      layer = 1,
                      method = "convolution",
                      method_option = list()) {
  if (method == "convolution") {
    out <- dispersal_convolution(
      raster <- spacemodel[[layer]],
      kernel <- method_option$kernel
    )
  }
  spacemodel[[layer]][] = out
  return(spacemodel)
}

# ===============================
# Dispersal Method
# ===============================

#' Compute presence map from habitat and nogo mask
#'
#' @param raster SpatRaster with habitat (weight and NA)
#' @param kernel Matrix defining the kernel. Can be build with `compute_kernel`.
#'
#' @return dispersal_raster (SpatRaster)
#' @export
dispersal_convolution <- function(
    raster,
    kernel
) {
  tmp_raster=raster
  raster[is.na(raster)] = 0
  dispersal_raster <- terra::focal(
    raster,
    w = kernel,
    fun = "sum",
    na.policy = "omit",
    pad = TRUE
  )
  dispersal_raster[is.na(tmp_raster)] = NA
  return(dispersal_raster)
}


