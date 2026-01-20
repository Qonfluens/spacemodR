library(terra)
# library(sp)
# library(gstat)

#' Krigeage object
#'
#' @param dataset sf object contenant les points et les colonnes de concentration
#' @export
krigeage <- function(dataset) {
  if (!inherits(dataset, "sf")) stop("dataset must be an sf object")
  if (sf::st_crs(dataset)$units_gdal != "metre") {
    warning("dataset CRS is not in meters")
  }

  structure(
    list(
      dataset = dataset
    ),
    class = "krigeage"
  )
}

# Internal function to cextract x, y coordinates and values
get_data <- function(obj, column) {
  if (!inherits(obj, "krigeage")) stop("obj must be a krigeage object")
  if (!column %in% colnames(obj$dataset)) {
    stop(paste("no column named", column))
  }

  coords <- sf::st_coordinates(obj$dataset)
  values <- obj$dataset[[column]]

  list(
    X = coords[,1],
    Y = coords[,2],
    values = values
  )
}


# regular grid
create_grid <- function(obj, GSD, margin = 0) {
  bb <- sf::st_bbox(obj$dataset)

  x_seq <- seq(bb["xmin"] - margin, bb["xmax"] + margin, by = GSD)
  y_seq <- seq(bb["ymin"] - margin, bb["ymax"] + margin, by = GSD)

  expand.grid(x = x_seq, y = y_seq)
}

## fit variogram
fit_variogram <- function(obj, column, model_type = "Exp") {
  dat <- get_data(obj, column)

  df <- data.frame(
    x = dat$X,
    y = dat$Y,
    value = dat$values
  )
  coordinates(df) <- ~x+y
  vgm_model <- variogram(value~1, df)
  fit <- fit.variogram(vgm_model, model = vgm(1, model_type, 1, 1))

  fit
}

## ordinary kriging
ordinary_krige <- function(obj, column, GSD = 10, margin = 0) {
  dat <- get_data(obj, column)
  df <- data.frame(
    x = dat$X,
    y = dat$Y,
    value = dat$values
  )
  coordinates(df) <- ~x+y

  # Fit variogram
  vgm_fit <- fit_variogram(obj, column)

  # Create grid
  grd_df <- create_grid(obj, GSD, margin)
  coordinates(grd_df) <- ~x+y
  gridded(grd_df) <- TRUE

  # Ordinary kriging
  ok <- krige(value~1, df, grd_df, model = vgm_fit)

  # Convert to SpatRaster
  r <- rast(ok)

  r
}


# ===============================
# Create raster ground data
# ===============================

library(sf)
library(gstat)
library(sp)

create_grid <- function(obj, GSD, margin = 0) {
  bb <- sf::st_bbox(obj)
  x_seq <- seq(bb["xmin"] - margin, bb["xmax"] + margin, by = GSD)
  y_seq <- seq(bb["ymin"] - margin, bb["ymax"] + margin, by = GSD)
  expand.grid(x = x_seq, y = y_seq)
}
# Charger les données
ground_concentration <- st_read("data/concentration.geojson")

sp_ground = sf::as_Spatial(ground_concentration)
v <- variogram(cd_log10~1, sp_ground)
m0 = vgm(psill = 1, model = "Lin", range = 100, nugget = 0.5)
m1 = vgm(psill = 1, model = "Sph", range = 100, nugget = 0.5)
m2 = vgm(psill = 1, model = "Exp", range = 100, nugget = 1) # best
m3 = vgm(psill = 1, model = "Gau", range = 100, nugget = 2)
m4 = vgm(psill = 1, model = "Mat", range = 100, nugget = 0.5, kappa = 1.5)
m <- fit.variogram(v, model=m2)
plot(v, model = m)


m_sph <- fit.variogram(v, model = vgm(psill = 10, "Sph", 2, 0.1))
m_gau <- fit.variogram(v, model = vgm(psill = 10, "Gau", 2, 0.1))
m_exp <- fit.variogram(v, model = vgm(psill = 1,  "Exp", 90, 2))
cv_sph <- krige.cv(cd_log10 ~ 1, sp_ground, model = m_sph)
cv_gau <- krige.cv(cd_log10 ~ 1, sp_ground, model = m_gau)
cv_exp <- krige.cv(cd_log10 ~ 1, sp_ground, model = m_exp)


grd_df <- create_grid(ground_concentration, 25, 1000)
coordinates(grd_df) <- ~x+y
gridded(grd_df) <- TRUE
proj4string(grd_df) <- proj4string(sp_ground)

# plot(sp_ground, col = "blue", pch = 19, main = "overlap check")
# plot(grd_df, add = TRUE, col = "red", pch = 3, cex = 0.5)

sp_ground_gstat <- gstat(formula = cd_log10 ~ 1, data = sp_ground,
                     nmax = 7, set = list(idp = .5))
sp_ground_gstat

z <- predict(sp_ground_gstat, grd_df)
spplot(z)

z_raster <- rast(
  nrows = nrow(grd_df),  # Nombre de lignes de la grille
  ncols = ncol(grd_df),  # Nombre de colonnes de la grille
  xmin = min(coordinates(grd_df)[, 1]),  # Coordonnée x minimale
  xmax = max(coordinates(grd_df)[, 1]),  # Coordonnée x maximale
  ymin = min(coordinates(grd_df)[, 2]),  # Coordonnée y minimale
  ymax = max(coordinates(grd_df)[, 2]),  # Coordonnée y maximale
  crs = crs(grd_df)  # Système de coordonnées
)

# Remplir le raster avec les valeurs prédites
values(z_raster) <- z$var1.pred


