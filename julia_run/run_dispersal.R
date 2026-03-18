# julia_run/run_dispersal.R

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Usage: Rscript run_dispersal.R <path_to_habitat> <path_to_resistance> <radius>")
}

path_habitat <- args[1]
path_resistance <- args[2]
radius <- as.numeric(args[3])

# 1. Nettoyer l'environnement R
Sys.setenv(LD_LIBRARY_PATH = "")

# 2.  charger les packages spatiaux en toute sécurité
library(spacemodR)
library(terra)

# --- 1. Load Data ---
cat("Loading rasters...\n")
rast_habitat <- rast(path_habitat)
rast_resistance <- rast(path_resistance)

# --- 2. Dispersal Computation ---
cat(sprintf("Starting Omniscape computation with radius = %s...\n", radius))
dispersed_map <- compute_dispersal(
  x = rast_habitat,
  method = "omniscape",
  options = list(
    resistance = rast_resistance,
    radius = radius
  )
)

# --- 3. Save the result ---
output_file <- dirname(path_habitat)
output_path <- file.path(output_file, "dispersed_map.rds")

saveRDS(dispersed_map, output_path)
cat("Computation finished. File saved to", output_path, "\n")
