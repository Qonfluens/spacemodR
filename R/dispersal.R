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

#' Create a raster mask from a habitat object
#'
#' Applies dispersal logic to a specific layer of the spacemodel.
#'
#' @param spacemodel spacemodel object.
#' @param layer character or integer. Name or index of the layer to disperse.
#' @param method method use for dispersal ("convolution", etc.).
#' @param method_option parameters list (must contain `kernel` for convolution).
#'
#' @return spacemod with modification of the layer
#'
#' @export
dispersal <- function(spacemodel,
                      layer = 1,
                      method = "convolution",
                      method_option = list()) {
  # secure raster for computing
  input_rast <- spacemodel[[layer]]
  # Calcul using generique computer
  out_rast <- compute_dispersal(
    x = input_rast,
    method = method,
    options = method_option,
    mask = input_rast # mask by itself => keep same NA
  )
  # update
  spacemodel[[layer]] <- out_rast

  return(spacemodel)
}

# ===============================
# Dispersal Method
# ===============================

#' Compute dispersal or spread map
#'
#' Generic engine to apply spatial processing (Convolution or External algorithms).
#' Handles NA masking and method dispatch.
#'
#' @param x SpatRaster. Source layer to disperse.
#' @param method Character. "convolution" or "omniscape" (placeholder).
#' @param options List. Parameters (e.g., `kernel` for convolution).
#' @param mask SpatRaster (optional). A mask to apply after dispersion (e.g., maintain original NA structure).
#'
#' @return SpatRaster
#' @noRd
compute_dispersal <- function(x, method = "convolution", options = list(), mask = NULL) {

  # 1. Dispatch Method
  if (method == "convolution") {
    # Validation du kernel
    if (is.null(options$kernel) || !is.matrix(options$kernel)) {
      stop("Method 'convolution' requires a 'kernel' matrix in 'method_option'.")
    }

    # Terra focal is very fast.
    # na.policy = "omit" consider NA as 0 in the weighted sum,
    out <- terra::focal(
      x,
      w = options$kernel,
      fun = "sum",
      na.policy = "omit", # neighbor NA do not propagate NA, they are set to 0
      fillvalue = 0,      # frontiers fill of 0
      expand = FALSE      # same extent
    )

  } else if (method == "omniscape") {
    # --- Placeholder for future integration as Julia ---
    # out <- run_julia_omniscape(x, options)
    stop("Method 'omniscape' is not yet implemented.")

  } else {
    stop(sprintf("Dispersal method '%s' is not recognized.", method))
  }

  # 2. Mask (Smart handling)
  # If their is a mask, (predator habitat), result is filtered.
  # Keep intake in the realistic area for transfer function.
  if (!is.null(mask)) {
    out <- terra::mask(out, mask)
  }

  return(out)
}

