#' Create a spacemodel object
#'
#' @param raster_stack SpatRaster (multi-layer stack)
#' @param trophic_tbl Object of class \code{trophic_tbl}
#' @export
spacemodel <- function(raster_stack, trophic_tbl) {
  if (!inherits(raster_stack, "SpatRaster")) {
    stop("raster_stack must be a SpatRaster object")
  }
  if (!inherits(trophic_tbl, "trophic_tbl")) {
    stop("trophic_tbl must be an trophic_tbl object")
  }
  levels <- attr(trophic_tbl, "level")
  if(dim(raster_stack)[[3]] != length(levels)){
    stop("raster_stack and trophic_tbl must have the same length")
  }
  if(!all(names(raster_stack) %in% names(levels)) ||
    !all(names(trophic_tbl$level) %in% names(raster_stack))
  ) {
    stop("elements in raster_stack and trophic_tbl must have the same names")
  }

  # add attribute
  attr(raster_stack, "trophic_tbl") <- trophic_tbl
  # add class as attribute
  attr(raster_stack, "spacemodel") <- TRUE

  return(raster_stack)
}

# ===============================
# Raster Constructor
# ===============================
#' Create a RasterStack from a list of rasters and names
#'
#' @description
#' This function creates a `SpatRaster` stack from a list of rasters and assigns unique names to each layer.
#'
#' @param raster_list A list of `SpatRaster` objects or file paths to raster files.
#' @param names A character vector of unique names for each raster layer in the stack.
#'
#' @return A `SpatRaster` stack with named layers.
#'
#' @details
#' The function checks that the length of `raster_list` matches the length of `names`,
#' and that all names are unique. If not, it stops with an error.
#'
#' @examples
#' # Example with terra rasters
#' library(terra)
#' r1 <- rast(nrows=10, ncols=10, vals=1:100)
#' r2 <- rast(nrows=10, ncols=10, vals=101:200)
#' raster_stack(list(r1, r2), c("layer1", "layer2"))
#'
#' @export
#' @importFrom terra rast
raster_stack <- function(raster_list, names=NULL) {
  if(is.null(names)){
    names <- names(raster_list)
  }
  if (length(raster_list) != length(names)) {
    stop("'raster_list' and 'names' should have the same length")
  }
  if (length(unique(names)) != length(names)) {
    stop("'names' have to be unique")
  }
  stack <- terra::rast(raster_list)
  names(stack) <- names
  return(stack)
}

# ===============================
# Adidtional Function for Exploring
# ===============================

#' @export
print.spacemodel <- function(x, ...) {
  cat("Spacemodel object\n")
  cat("Number of raster layers:", terra::nlyr(x), "\n")
  cat("Raster extent:", terra::ext(x), "\n")
  trophic_tbl <- attr(spcmdl_habitat, "trophic_tbl")
  cat("Trophic data.frame:", trophic_tbl, "\n")
}


#' @export
plot.spacemodel <- function(x, layer = 1, graph=FALSE, ...) {
  if(graph){
    trophic_tbl <- attr(spcmdl_habitat, "trophic_tbl")
    return(plot(trophic_tbl))
  }
  # plot raster
  terra::plot(x$raster_stack[[layer]], main = paste("Layer", layer))
}

