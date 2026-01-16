---
title: "Tutorial"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Tutorial}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: console
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

```{r setup}
library(spacemodR)
```

## Load OCS-GE layers in the Region Of Interest (ROI)

```{r}
data("roi_metaleurop")
## Or load a ROI via geojson
# roi_metaleurop <- sf::st_read("data/metaleurop_roi.geojson")
dpts <- get_departements_for_roi(roi_metaleurop)
```

The next function can be quite long to compute because must load OCS-GE files.

```{r}
## This function is long to compute so we load the data set
# sf_site <- get_ocsge_data(roi_metaleurop)
data(ocsge_metaleurop)
```

You can then plot the object using for instance the `ggplot2` library which is not
included in `spacemodR`.

```{r}
library(ggplot2)
ggplot() +
  theme_minimal() +
  geom_sf(data=ocsge_metaleurop, aes(fill=code_cs)) +
    geom_sf(data=roi_metaleurop, fill=NA, color="red", size=2)
```

# Habitat

```{r}
layer_CS2111 = ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS2.1.1.1", ]
layer_CS221 = ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS2.2.1", ]
layer_CS1121 = ocsge_metaleurop[ocsge_metaleurop$code_cs == "CS1.1.2.1", ]
```


```{r}
habitat_10 = habitat(layer_CS2111)
plot(habitat_10)
```
```{r}
habitat_11 = habitat(layer_CS2111, weight=0.2)
head(habitat_11)
```


```{r}
habitat_12 = habitat(
  layer_CS2111,
  habitat=sample(c(TRUE,FALSE), nrow(layer_CS2111), replace=TRUE),
  weight=runif(nrow(layer_CS2111), 0, 10)
)
plot(habitat_12)
```
Another option more convient for more complex case:


```{r}
habitat_20 = habitat() |>
  add_habitat(layer_CS2111)
class(habitat_20)
```


```{r}
habitat_21 = habitat() |>
  add_habitat(layer_CS2111, weight=0.2)
head(habitat_21)
```


```{r}
habitat_22 = habitat() |>
  add_habitat(layer_CS2111, weight=0.2) |>
  add_habitat(layer_CS1121, weight=0.4) |>
  add_nohabitat(layer_CS221)
```

```{r}
plot(habitat_22)
```

# Raster habitat

```{r}
ground_cd <- terra::rast("../data/ground_concentration_cd.tif")
# data(ground_cd)
terra::plot(ground_cd)
```

```{r}
layer_soil_natural = ocsge_metaleurop[ocsge_metaleurop$code_cs %in% OCSGE_codes$soil, ]
layer_soil_artificial = ocsge_metaleurop[ocsge_metaleurop$code_cs %in% OCSGE_codes$sealed, ]

habitat_sol = habitat() |>
  add_habitat(layer_soil_natural) |>
  add_nohabitat(layer_soil_artificial)
plot(habitat_sol)
```

```{r}
rast_sol <- habitat_raster(ground_cd, habitat_sol)
terra::plot(rast_sol)

rast_10 <- habitat_raster(ground_cd, habitat_10)
terra::plot(rast_10)

rast_12 <- habitat_raster(ground_cd, habitat_12)
terra::plot(rast_12)

rast_22 <- habitat_raster(ground_cd, habitat_22)
terra::plot(rast_22)
```


## stack habitat 

```{r}
stack_habitat <- raster_stack(
  raster_list = list(rast_sol, rast_10, rast_12, rast_22),
  names = c("sol", "sp10", "sp12", "sp22")
)

terra::plot(stack_habitat)
```

# Trophic web

```{r}
trophic_df <- trophic() |>
  add_link("sol", "sp10", 2) |>
  add_link("sol", "sp12", 3) |>
  add_link("sp10", "sp12", 2) |>
  add_link("sp12", "sp22")

attr(trophic_df, "level")
```

The trophic web can then be computed:

```{r}
plot(trophic_df, shift=FALSE)
```

```{r}
plot(trophic_df, shift=TRUE)
```

# Introduction to a `spacemodel`

A `spacemodel` is a list of two object a raster_stack and a trophic_graph:
- the `raster_stack` is a stack of rasters using the `terra` package and the object `raster_stack`
- the `trophic_graph` is a `graph` object defining a directed acyclic graph (DAG) 
to which we can add weight "weigth, 

```{r}
spcmdl_habitat <- spacemodel(stack_habitat, trophic_df)
```

## Dispersal

Dispersal is independent of trophic link for now since we did not yet implemented 
a dispersal dependent of the potential trophic links.


```{r}
k_sp10 <- compute_kernel(radius=10, GSD=25, size_std=1.5)
k_sp12 <- compute_kernel(radius=100, GSD=25, size_std=1.5)
k_sp22 <- compute_kernel(radius=200, GSD=25, size_std=1.5)

spcmdl_dispersal <- spcmdl_habitat |>
  dispersal("sp10",  method="convolution",  method_option=list(kernel=k_sp10)) |>
  dispersal("sp12",  method="convolution",  method_option=list(kernel=k_sp12)) |>
  dispersal("sp22",  method="convolution",  method_option=list(kernel=k_sp22))
```


```{r}
terra::plot(spcmdl_dispersal)
```


# Spacemodel Exposure

```{r}
spcmdl_dispersal[["sol"]][] <- ground_cd
terra::plot(spcmdl_dispersal[["sol"]])
```


```{r}
kernels <- list(
  sol  = NA,
  sp10 = k_sp10,
  sp12 = k_sp12,
  sp22 = k_sp22
)

intakes <- attr(spcmdl_dispersal, "trophic_tbl")
intakes <- normalize_weights(intakes)
intakes$flux <- list(
  function(x) x*2 + 0,
  function(x) x*3 + 0,
  function(x) x*4 + 0,
  function(x) x*5 + 0
)

spcmdl_transfer <- transfer(spcmdl_dispersal, kernels, intakes)
```

```{r}
terra::plot(spcmdl_transfer)
```

