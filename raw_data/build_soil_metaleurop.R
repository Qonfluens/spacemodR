# ===============================
# Build `soil_metaleurop` (soil Cd, Pb, Zn sampling points around Metaleurop)
# ===============================
# Source: raw_data/concentration.geojson (595 topsoil samples, EPSG:2154),
# originally from "startt_sol_3_bdd_Gold_sspedo_090728.xls".

library(sf)

raw <- st_read("raw_data/concentration.geojson", quiet = TRUE)

soil_metaleurop <- raw[, c("name_ISA", "usage", "dist", "angle_N", "vent",
                           "cd", "pb", "zn")]
names(soil_metaleurop)[names(soil_metaleurop) == "name_ISA"] <- "id"
names(soil_metaleurop)[names(soil_metaleurop) == "angle_N"] <- "angle"
names(soil_metaleurop)[names(soil_metaleurop) == "vent"] <- "wind"
st_geometry(soil_metaleurop) <- "geometry"

usethis::use_data(soil_metaleurop, overwrite = TRUE, compress = "xz")
