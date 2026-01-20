#' Transfer concentration across trophic levels
#'
#' Computes the transfer of concentration through a trophic network from
#' lower to higher trophic levels using spatial spreading and intake functions.
#'
#' @param spacemodel A named list of spatial layers (e.g. \code{SpatRaster} objects).
#'        Must contain an attribute \code{trophic_tbl} of class \code{trophic_tbl}.
#' @param kernels A list of kernel parameters for each layer.
#' @param intakes A \code{trophic_tbl} object (or compatible table) containing
#'        normalized weights and flux functions for each trophic link.
#' @param verbose Logical. If \code{TRUE}, prints progress information.
#'
#' @details
#' The function processes layers in ascending trophic order, as defined
#' by the \code{level} attribute of the trophic table.
#'
#' For each layer:
#' \enumerate{
#'   \item Resources (lower neighbors) are identified.
#'   \item Concentration from each resource is spatially spread using \code{spread()}.
#'   \item Intake is computed using \code{intake()}.
#'   \item Contributions from all resources are summed.
#' }
#'
#' The function assumes that intake weights are already normalized so that,
#' for each consumer, the sum of contributions from all resources equals 1.
#'
#' @return A named list of spatial layers representing concentrations
#'         after trophic transfer.
#'
#' @examples
#' # Assuming spacemodel, kernels and intakes are defined:
#' # result <- transfer(spacemodel, kernels, intakes)
#'
#' @export
transfer <- function(spacemodel, kernels, intakes, verbose = FALSE) {

  stopifnot(attr(spacemodel, "spacemodel"))
  stopifnot(!is.null(attr(spacemodel, "trophic_tbl")))

  trophic_tbl <- attr(spacemodel, "trophic_tbl")
  levels <- attr(trophic_tbl, "level")

  stopifnot(!is.null(levels))

  concentration_stack <- list()

  # "level" attribute is ordered from lower to upper layer.
  for (layer in names(levels)) {

    if (verbose) message("Processing layer: ", layer)

    ressources <- lower_neighbors(trophic_tbl, layer)
    concentration_stack[[layer]] <- spacemodel[[layer]]

    if(length(ressources)>0) {
      out <- concentration_stack[[layer]][] * 0
      for (r_name in ressources) {

        if (verbose) {
          message("  Resource: ", r_name)
        }
        exposure <- spread(
          concentration_stack[[r_name]],
          spacemodel[[layer]],
          kernels[[layer]]
        )
        bodyburden <- intake(exposure, intakes, r_name, layer)
        out <- out + bodyburden[]
      }
      concentration_stack[[layer]][] <- out
    }
  }
  # CONVERT IN SPACEMODEL
  stack_transfer <- raster_stack(
    raster_list = concentration_stack,
    names = names(concentration_stack)
  )
  spacemodel_transfer <- spacemodel(stack_transfer, trophic_tbl)
  return(spacemodel_transfer)
}



#' Spread concentration using a kernel
#'
#' Applies a spatial spreading (convolution) of a raster layer using a kernel.
#'
#' @param l_raster A \code{SpatRaster} representing the lower-level layer to spread.
#' @param u_raster A \code{SpatRaster} of the upper layer used to mask invalid cells.
#' @param kernel A list with elements \code{radius}, \code{GSD}, and \code{kernel_size_std}.
#'
#' @return A \code{SpatRaster} representing the spread concentration, masked by \code{u_raster}.
#'
#' @export
spread <- function(l_raster, u_raster, kernel=NA){
  if(is.matrix(kernel)){
    tmp_raster=l_raster
    l_raster[is.na(l_raster)] = 0
    l_raster <- terra::focal(
      l_raster,
      w = kernel,
      fun = "sum",
      na.policy = "omit",
      pad = TRUE
    )
  }
  l_raster[is.na(u_raster)] = NA
  return(l_raster)
}

#' Apply trophic intake from a resource layer
#'
#' Computes the contribution of a resource raster to a consumer layer using
#' the normalized weight and a flux function stored in the trophic table.
#'
#' @param raster A \code{SpatRaster} (or similar) representing exposure from the resource.
#' @param intakes A \code{trophic_tbl} object that must contain a column
#'        \code{normalized_weight} and a column \code{flux}.
#' @param from Character string, name of the source node.
#' @param to Character string, name of the target node.
#'
#' @details
#' The function extracts the link corresponding to \code{from -> to} from the
#' trophic table and applies:
#' \enumerate{
#'   \item the associated flux function to the raster values
#'   \item the normalized weight of the link
#' }
#'
#' An error is thrown if no such link exists.
#'
#' @return A raster object with transformed values.
#'
#' @export
intake <- function(raster, intakes, from, to) {

  stopifnot(inherits(intakes, "trophic_tbl"))

  # Extract from/to vectors from the link column
  edges <- data.frame(
    from = sapply(intakes$link, `[[`, "from"),
    to   = sapply(intakes$link, `[[`, "to"),
    stringsAsFactors = FALSE
  )

  # Identify the matching link
  idx <- which(edges$from == from & edges$to == to)

  if (length(idx) == 0) {
    stop(sprintf("No link between %s and %s", from, to))
  }

  # Apply flux function and normalized weight
  w <- intakes$normalized_weight[idx]
  flux_fun <- intakes$flux[[idx]]

  raster[] <- w * flux_fun(raster[])

  raster
}
