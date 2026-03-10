# ===============================
# Constructor: habitat
# ===============================

#' Create a Habitat object
#'
#' Create a spatial Habitat object based on an optional \code{sf} data.frame.
#' If no geometry is provided, creates an empty Habitat object.
#' The object has columns:
#' \itemize{
#'   \item \code{habitat}: logical, TRUE/FALSE
#'   \item \code{weight}: numeric
#'   \item \code{geometry}: sfc geometry
#' }
#'
#' @param geometry An object of class \code{sf} or \code{sfc}. Optional, default is empty.
#' @param habitat Logical vector indicating habitat presence. Default is logical(0).
#' @param weight Numeric vector of weights. Default is numeric(0).
#'
#' @return An object of class \code{habitat}, inheriting from \code{sf} and \code{data.frame}.
#' @export
#'
#' @examples
#' library(sf)
#'
#' # Empty habitat
#' h <- habitat()
#' h
#'
#' # Habitat with geometries
#' geom <- st_sfc(
#'   st_point(c(0, 0)),
#'   st_point(c(1, 1)),
#'   crs = 4326
#' )
#'
#' hab <- habitat(
#'   geometry = geom,
#'   habitat = c(TRUE, FALSE),
#'   weight = c(0.8, 0)
#' )
#' hab
habitat <- function(geometry = NULL,
                    habitat = NULL,
                    weight = NULL) {

  # ---- empty object if no geometry ----
  if (is.null(geometry)) {
    sf_obj <- sf::st_sf(
      habitat = logical(0),
      weight = numeric(0),
      geometry = sf::st_sfc()
    )
    class(sf_obj) <- c("habitat", class(sf_obj))
    return(sf_obj)
  }

  # ---- handle sf or sfc ----
  if (inherits(geometry, "sf")) {
    geom <- sf::st_geometry(geometry)
    n <- nrow(geometry)
  } else if (inherits(geometry, "sfc")) {
    geom <- geometry
    n <- length(geometry)
  } else {
    stop("`geometry` must be an object of class 'sf' or 'sfc'")
  }

  # ---- checks ----
  if (is.null(habitat)) habitat <- rep(TRUE, n)
  if (is.null(weight)) weight <- rep(1, n)

  if(length(habitat)==1) habitat <- rep(habitat, n)
  if(length(weight)==1) weight <- rep(weight, n)

  if (!length(habitat) == n) stop("Length of habitat must match geometry")
  if (!length(weight) == n) stop("Length of weight must match geometry")
  if (!is.logical(habitat)) stop("`habitat` must be logical (TRUE/FALSE)")
  if (!is.numeric(weight)) stop("`weight` must be numeric")

  # ---- build sf object ----
  sf_obj <- sf::st_sf(
    habitat = habitat,
    weight = weight,
    geometry = sf::st_sfc(geom)
  )
  class(sf_obj) <- c("habitat", class(sf_obj))
  sf_obj
}

#' @export
print.habitat <- function(x, ...) {
  cat("Habitat object\n")
  cat("Number of features:", nrow(x), "\n")
  cat("CRS:", sf::st_crs(x)$input, "\n")
  NextMethod()
}

#' Plot a Habitat object
#'
#' @description
#' Visually inspect a \code{habitat} object. Favorable habitat zones are colored
#' in green, and absolute barriers (non-habitat) are colored in red.
#'
#' @param x A \code{habitat} object.
#' @param ... Additional arguments passed to \code{plot} (e.g., \code{main}, \code{border}).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hab <- habitat() |>
#'   add_habitat(good_polygons, weight = 1) |>
#'   add_nohabitat(roads)
#' plot(hab)
#' }
plot.habitat <- function(x, ...) {
  # Handle empty objects gracefully
  if (nrow(x) == 0) {
    message("Empty habitat object. Nothing to plot.")
    return(invisible(x))
  }
  # Ensure sf's plotting method is available
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for plotting.")
  }
  # Define colors: Green for TRUE (habitat), Red for FALSE (no-habitat)
  fill_colors <- ifelse(x$habitat, "#81C784", "#E57373") # Muted green and red
  # Set up the title if not provided by the user
  args <- list(...)
  if (!"main" %in% names(args)) {
    args$main <- "Habitat Landscape\n(Green: Habitat, Red: Barrier)"
  }
  # Plot geometries
  do.call(plot, c(list(sf::st_geometry(x), col = fill_colors), args))
  # Return the object invisibly to allow piping if needed
  invisible(x)
}

# ===============================
# Generic functions
# ===============================

#' Add habitat zones to a Habitat object
#'
#' @param hab A Habitat object
#' @param sf_data An sf object containing geometries to add
#' @param weight A weight to apply on the habitat feature. Can then be used as
#'  resistance for dispersal functions for instance.
#' @param ... Additional arguments
#' @return A Habitat object with new geometries added as habitat
#' @export
add_habitat <- function(hab, sf_data, weight = 1, ...) {
  UseMethod("add_habitat")
}

#' Add non-habitat zones to a Habitat object
#'
#' @param hab A Habitat object
#' @param sf_data An sf object containing geometries to add
#' @param ... Additional arguments
#' @return A Habitat object with new geometries added as non-habitat
#' @export
add_nohabitat <- function(hab, sf_data, ...) {
  UseMethod("add_nohabitat")
}


# ===============================
# S3 methods for Habitat
# ===============================

#' @export
add_habitat.habitat <- function(hab, sf_data, weight = NULL, ...) {
  if (!inherits(sf_data, "sf")) stop("sf_data must be an sf object")
  new_block <- habitat(
    geometry = sf_data,
    habitat = TRUE,
    weight = weight
  )
  if(nrow(hab)==0){
    out <- new_block
  } else{
    # rbind.sf check crs.
    class(hab) <- setdiff(class(hab), "habitat")
    class(new_block) <- setdiff(class(new_block), "habitat")
    out <- rbind(hab, new_block)
    class(out) <- c("habitat", class(out))
  }
  return(out)
}

#' @export
add_nohabitat.habitat <- function(hab, sf_data, ...) {
  if (!inherits(sf_data, "sf")) stop("sf_data must be an sf object")
  new_block <- habitat(
    geometry = sf_data,
    habitat = FALSE,
    weight = 0
  )
  if(nrow(hab)==0){
    out <- new_block
  } else{
    # rbind.sf check crs.
    class(hab) <- setdiff(class(hab), "habitat")
    class(new_block) <- setdiff(class(new_block), "habitat")
    out <- rbind(hab, new_block)
    class(out) <- c("habitat", class(out))
  }
  return(out)
}


# ===============================
# Rasterize Habitat
# ===============================

#' Rasterize a Habitat Object
#'
#' @description
#' Converts a spatial \code{habitat} object into a \code{terra::SpatRaster}. The resulting
#' raster stores the \code{weight} values of the habitat geometries. Areas explicitly
#' defined as non-habitat (\code{habitat = FALSE}) are assigned \code{NA}.
#'
#' @param ground_raster A \code{terra::SpatRaster} object used as a reference template.
#'   It provides the extent, resolution, and coordinate reference system (CRS).
#' @param habitat A \code{habitat} object (inheriting from \code{sf}).
#'
#' @details
#' The function maps the continuous or categorical weights of the habitat geometries onto a raster grid.
#'
#' \strong{Background handling:} By default, pixels not covered by any geometry are assigned
#' a weight of \code{0} and are considered passable (habitat = \code{TRUE}).
#'
#' \strong{Exclusion zones:} Geometries added via \code{add_nohabitat()} act as absolute
#' barriers. Any pixel intersecting these geometries will be assigned \code{NA}, overriding
#' any underlying weight. If overlapping polygons exist, the order of features in the
#' \code{habitat} object determines the final value (last on top).
#'
#' @return A \code{terra::SpatRaster} containing habitat weights, with \code{NA} values
#'   in explicit non-habitat zones.
#'
#' @importFrom terra rast vect rasterize mask
#' @export
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' # Create a dummy reference raster
#' ref_grid <- rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10)
#'
#' # Create habitat zones
#' good_hab <- st_sf(geometry = st_sfc(st_polygon(list(cbind(c(1, 9, 9, 1, 1), c(1, 1, 9, 9, 1))))))
#' bad_hab  <- st_sf(geometry = st_sfc(st_polygon(list(cbind(c(4, 6, 6, 4, 4), c(4, 4, 6, 6, 4))))))
#'
#' # Build habitat object
#' my_habitat <- habitat() |>
#'   add_habitat(good_hab, weight = 0.8) |>
#'   add_nohabitat(bad_hab)
#'
#' # Rasterize
#' hab_rast <- habitat_raster(ref_grid, my_habitat)
#' plot(hab_rast)
#' }
habitat_raster <- function(ground_raster, habitat){
  if (!inherits(habitat, "habitat")) {
    stop("`habitat` must be a 'habitat' object")
  }
  # drop habitat to keep sf class
  class(habitat) <- setdiff(class(habitat), "habitat")

  template <- terra::rast(ground_raster)

  if (nrow(habitat) == 0) {
    return(terra::init(out, v = 0))
  }

  v <- terra::vect(habitat)
  out_hab <- terra::rasterize(v, template, field = "weight", background = 0)
  out_go <- terra::rasterize(v, template, field = "habitat", background = 1)
  out <- terra::mask(out_hab, out_go, maskvalues = 0, updatevalue = NA)

  return(out)
}


