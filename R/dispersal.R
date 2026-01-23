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
#'
#' @export
compute_dispersal <- function(x, method = "convolution", options = list(), mask = NULL) {

  # 1. Dispatch Method
  if (method == "convolution") {
    # --- CONVOLUTION (Terra) ---
    if (is.null(options$kernel) || !is.matrix(options$kernel)) {
      stop("Method 'convolution' requires a 'kernel' matrix in 'method_option'.")
    }

    out <- terra::focal(
      x,
      w = options$kernel,
      fun = "sum",
      na.policy = "omit",
      fillvalue = 0,
      expand = FALSE
    )

  } else if (method == "omniscape") {
    # --- OMNISCAPE (Julia) ---
    # Cette méthode nécessite Julia et le package Omniscape.jl
    out <- run_julia_omniscape(x, options)

  } else {
    stop(sprintf("Dispersal method '%s' is not recognized.", method))
  }

  # 2. Mask (Smart handling)
  if (!is.null(mask)) {
    out <- terra::mask(out, mask)
  }

  return(out)
}

#' @section Using Omniscape (Julia):
#' To use `method = "omniscape"`, you must have Julia installed on your machine
#' and the `JuliaCall` R package.
#'
#' \strong{First-time setup:}
#' \enumerate{
#'   \item Install Julia from \url{https://julialang.org/downloads/}
#'   \item In R, install JuliaCall: \code{install.packages("JuliaCall")}
#'   \item Setup Julia in R: \code{JuliaCall::julia_setup(installJulia = TRUE)}
#'   \item Install the Omniscape package in Julia (via R):
#'     \code{JuliaCall::julia_install_package("Omniscape")}
#' }
#'
#' \strong{Usage example:}
#' \preformatted{
#' # 1. Initialize Julia (Required at the start of the session)
#' library(JuliaCall)
#' julia_setup()
#'
#' # 2. Define inputs
#' my_habitat <- ... # SpatRaster
#' my_resistance <- ... # SpatRaster (1 = low resistance, 100 = high)
#'
#' # 3. Run dispersal
#' # This will compute connectivity using Circuit Theory
#' dispersed_map <- dispersal(
#'   spacemodel = my_model,
#'   layer = "Fox",
#'   method = "omniscape",
#'   method_option = list(
#'     resistance = my_resistance,
#'     radius = 15 # Moving window radius in pixels
#'   )
#' )
#' }
#'
#'

#' Run Omniscape via JuliaCall (Robust Version)
#'
#' @noRd
#' Run Omniscape via JuliaCall (Fixed Syntax)
#'
#' @noRd
run_julia_omniscape <- function(x, options) {

  # --- A. Vérifications ---
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Package 'JuliaCall' is required. Please install it.")
  }

  tryCatch({
    JuliaCall::julia_command("1")
  }, error = function(e) {
    stop("Julia not connected. Run 'JuliaCall::julia_setup()' first.")
  })

  if (is.null(options$resistance) || is.null(options$radius)) {
    stop("Options 'resistance' and 'radius' are mandatory for omniscape.")
  }

  # --- B. Préparation (R -> Julia) ---
  source_mat <- terra::as.matrix(x, wide = TRUE)
  resist_mat <- terra::as.matrix(options$resistance, wide = TRUE)

  # Gestion simple des NA pour le transfert
  source_mat[is.na(source_mat)] <- 0
  # On laisse les NA de résistance tels quels pour l'instant (transfert en NaN ou NA selon R)

  JuliaCall::julia_assign("source_mat_R", source_mat)
  JuliaCall::julia_assign("resist_mat_R", resist_mat)

  # --- C. Exécution (Julia) ---
  # CORRECTION : On utilise 'begin ... end' pour que Julia accepte le bloc entier.
  # On retire les accents dans les commentaires Julia pour éviter les erreurs d'encodage via sprintf.

  cmd_julia <- sprintf('
    begin
        using Omniscape

        # 1. Configuration
        config = Dict{String, String}(
          "radius" => "%d",
          "block_size" => "1",
          "source_from_resistance" => "false",
          "solver" => "cg+amg"
        )

        # 2. Conversion safe (coalesce remplace les missing par 0.0)
        # On s assure que tout est en Float64
        src = coalesce.(Float64.(source_mat_R), 0.0)
        rst = Float64.(resist_mat_R)

        # 3. Run Omniscape
        # Le resultat est stocke dans res
        res = run_omniscape(config, src, resistance=rst)

        # 4. Nettoyage Sortie
        # On recupere la matrice brute et on nettoie les Missing
        out_raw = res.cum_currmap
        out_clean = map(x -> ismissing(x) ? NaN : Float64(x), out_raw)
    end
  ', as.integer(options$radius))

  # Exécution du bloc
  JuliaCall::julia_command(cmd_julia)

  # --- D. Récupération (Julia -> R) ---
  # La variable "out_clean" a été définie dans le scope global (Main) par le bloc begin/end
  result_mat <- JuliaCall::julia_eval("out_clean")

  # --- E. Reconstruction du Raster ---
  out_rast <- terra::rast(x)
  terra::values(out_rast) <- result_mat

  # Remettre les NA originaux si besoin
  # out_rast[is.na(x)] <- NA

  return(out_rast)
}
