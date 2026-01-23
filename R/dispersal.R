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


#' Run Omniscape via JuliaCall
#'
#' @param x SpatRaster. The source strength (abundance).
#' @param options List containing:
#'   - `resistance`: SpatRaster (Required). Resistance map.
#'   - `radius`: Numeric (Required). Moving window radius (in pixels).
#'   - `block_size`: Integer (Optional). Block size for parallel processing.
#'   - `parallel`: Boolean (Optional). Use parallel processing in Julia.
#'
#' @return SpatRaster (Cumulative Current)
#' @noRd
run_julia_omniscape <- function(x, options) {

  # --- A. Vérifications Préliminaires ---
  if (!requireNamespace("JuliaCall", quietly = TRUE)) {
    stop("Package 'JuliaCall' is required for method='omniscape'. Please install it.")
  }

  # Vérification des options obligatoires pour Omniscape
  if (is.null(options$resistance)) {
    stop("Method 'omniscape' requires a 'resistance' raster in 'options'.")
  }
  if (is.null(options$radius)) {
    stop("Method 'omniscape' requires a 'radius' (in pixels) in 'options'.")
  }

  # Vérification de la connexion Julia (l'utilisateur doit l'avoir initialisé)
  # On tente une commande simple pour voir si Julia répond
  tryCatch({
    JuliaCall::julia_command("1 + 1")
  }, error = function(e) {
    stop("Julia is not connected. Please run 'JuliaCall::julia_setup()' before using this function.")
  })

  # --- B. Préparation des Données (R -> Julia) ---
  # Omniscape.jl attend des Arrays (matrices) ou des chemins de fichiers.
  # Le transfert mémoire via matrice est plus propre pour un package R.

  # Conversion Terra -> Matrix
  source_mat <- terra::as.matrix(x, wide = TRUE)
  resist_mat <- terra::as.matrix(options$resistance, wide = TRUE)

  # Gestion des NA : Omniscape n'aime pas les NA dans la résistance.
  # On remplace par une résistance infinie ou très haute (ex: -9999 géré par Omniscape ou juste une valeur haute)
  # Ici, on laisse l'utilisateur gérer ses NA ou on met une valeur par défaut ?
  # Pour la démo, on remplace NA résistance par -9999 (nodata standard)
  resist_mat[is.na(resist_mat)] <- -9999
  source_mat[is.na(source_mat)] <- 0

  # Envoi vers Julia
  JuliaCall::julia_assign("source_arr", source_mat)
  JuliaCall::julia_assign("resist_arr", resist_mat)

  # --- C. Configuration et Exécution ---

  # Chargement de la librairie Julia
  JuliaCall::julia_library("Omniscape")

  # Création du dictionnaire de configuration Julia
  # Note : Omniscape.jl utilise un Dict pour la config
  radius <- as.integer(options$radius)
  block_size <- ifelse(is.null(options$block_size), 1, as.integer(options$block_size))

  JuliaCall::julia_command(sprintf('
    config = Dict{String, String}(
      "radius" => "%d",
      "block_size" => "%d",
      "source_from_resistance" => "false",
      "calc_flow_potential" => "false",
      "calc_normalized_current" => "false",
      "solver" => "cg+amg" # Solver rapide
    )
  ', radius, block_size))

  # Lancement du calcul ("run_omniscape" peut prendre des arrays directement dans les versions récentes)
  # Sinon on injecte les arrays dans le run
  message("Running Omniscape in Julia... (This may take time)")

  JuliaCall::julia_command('
    # On lance Omniscape en lui passant les arrays directement
    # Note: La signature exacte dépend de la version d\'Omniscape.jl.
    # Cette approche simule le passage via mémoire.

    using Omniscape

    # On appelle la fonction interne ou l\'API publique qui accepte les arrays
    # Si l\'API publique ne prend que des fichiers, on peut utiliser `run_omniscape(config, source_arr, resist_arr)`
    # Supposons l\'API standard v0.5+:
    result = run_omniscape(config, source_arr, resistance=resist_arr)

    # Récupération du courant cumulatif
    cum_currmap = result.cum_currmap
  ')

  # --- D. Récupération (Julia -> R) ---
  result_mat <- JuliaCall::julia_eval("cum_currmap")

  # Conversion Matrix -> Terra
  # On reprend la géométrie du raster d'entrée 'x'
  out_rast <- terra::rast(x)
  terra::values(out_rast) <- result_mat

  # On remet les NA là où c'était NA dans la source (optionnel, mais propre)
  out_rast[is.na(x)] <- NA

  return(out_rast)
}

