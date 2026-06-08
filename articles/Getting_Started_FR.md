# Getting Started: Vue d'ensemble de spacemodR

Bienvenue dans `spacemodR` ! Ce guide de démarrage (“Getting Started”)
vous offre une vue d’ensemble des capacités du package.

L’objectif de `spacemodR` est de réaliser des **Évaluations des Risques
Écologiques (ERE) spatialement explicites**. Plutôt que de calculer un
risque global sur un site, nous intégrons la géographie réelle du
paysage pour cartographier les flux.

Pour cela, nous suivons un pipeline logique et modulaire : 1. **Le
Paysage et les Habitats** : Définir la zone d’étude et la capacité des
espèces à y vivre. 2. **Le Réseau Trophique** : Définir “qui mange qui”
(les proies et les prédateurs). 3. **Le Spacemodel** : L’objet central
qui fusionne l’espace et les relations écologiques. 4. **La Dispersion**
: Modéliser le mouvement des animaux dans le paysage. 5. **Le Transfert
et l’Exposition** : Suivre la propagation d’un contaminant à travers le
réseau trophique. 6. **La Cartographie du Risque** : Générer des indices
de risque spatiaux (ex: Eco-SSL).

Commençons par charger les packages nécessaires :

``` r

library(spacemodR)
library(ggplot2)
library(terra)
```

------------------------------------------------------------------------

## 1. Le Paysage et les Habitats

Tout commence par la géographie. `spacemodR` facilite l’extraction et la
manipulation de données d’occupation du sol (comme la base OCS-GE en
France) pour une zone d’intérêt (Region of Interest - ROI).

``` r

# Charger une zone d'étude (ex: site de Metaleurop) et les données d'occupation du sol
data("roi_metaleurop")
data("ocsge_metaleurop")

ggplot() +
  theme_minimal() +
  geom_sf(data=ocsge_metaleurop, aes(fill=code_cs), color=NA) +
  geom_sf(data=roi_metaleurop, fill=NA, color="red", size=1) +
  theme(legend.position = "none") +
  labs(title="Occupation du sol sur la zone d'étude")
```

![](Getting_Started_FR_files/figure-html/load_roi-1.png)

À partir de ces polygones, nous définissons des **habitats** pour nos
espèces. Un habitat est une combinaison de zones favorables,
défavorables ou neutres. Ces géométries sont ensuite transformées en
grilles régulières (rasters) prêtes pour la modélisation.

``` r

# Exemple: Chargement d'un raster de la concentration de fond d'un contaminant (Cadmium)
ground_cd <- load_raster_extdata("ground_concentration_cd_compressed.tif")

# Pour la démonstration, nous simulons un écosystème simplifié avec 4 maillons
names_hab = c("soil", "plant", "herbivore", "carnivore")

# On initialise des grilles d'habitats (raster) pour chaque maillon
# (Dans un vrai projet, chaque raster dérive de pondérations d'habitat spécifiques)
list_habitat <- lapply(names_hab, function(i) ground_cd) 
stack_habitat <- raster_stack(list_habitat, names_hab)

# Visualisation des couches
terra::plot(stack_habitat)
```

![](Getting_Started_FR_files/figure-html/build_habitat-1.png)

------------------------------------------------------------------------

## 2. Le Réseau Trophique

Une fois nos cartes créées, nous devons relier ces couches par des
interactions écologiques. `spacemodR` utilise un **Graphe Orienté
Acyclique (DAG)** pour définir les proies, les prédateurs, et le flux de
l’énergie (ou des contaminants).

``` r

# Construction du réseau trophique
trophic_df <- trophic() |>
  add_link(from = "soil", to = "plant") |>
  add_link(from = "plant", to = "herbivore") |>
  add_link(from = "herbivore", to = "carnivore")

# Visualisation des liens trophiques
plot(trophic_df)
```

![](Getting_Started_FR_files/figure-html/trophic_web-1.png)

------------------------------------------------------------------------

## 3. Le Spacemodel : l’objet central

Le `spacemodel` est le cœur du package. Il lie intrinsèquement la
dimension spatiale (le `raster_stack`) et la dimension écologique (le
graphe `trophic_df`).

Toute opération ultérieure (dispersion, exposition) utilisera cet objet
pour garantir la cohérence des calculs.

``` r

spcmdl <- spacemodel(stack_habitat, trophic_df)
print(spcmdl)
#> class       : SpatRaster
#> size        : 415, 401, 4  (nrow, ncol, nlyr)
#> resolution  : 24.93766, 24.93976  (x, y)
#> extent      : 697602.7, 707602.7, 7032171, 7042521  (xmin, xmax, ymin, ymax)
#> coord. ref. : RGF93 v1 / Lambert-93 (EPSG:2154)
#> sources     : ground_concentration_cd_compressed.tif
#> names       :      soil,     plant, herbivore, carnivore
#> min values  : -0.229399, -0.229399, -0.229399, -0.229399
#> max values  :  1.769119,  1.769119,  1.769119,  1.769119
```

------------------------------------------------------------------------

## 4. La Dispersion et le Mouvement

Les animaux ne sont pas statiques. L’exposition d’un individu dépend de
ses déplacements (son rayon de recherche de nourriture, sa dispersion).
`spacemodR` permet de simuler ces mouvements, par exemple via des noyaux
de convolution (kernels) ou la théorie des circuits (Omniscape).

``` r

# Calcul des noyaux de dispersion en fonction d'un rayon de mobilité (ex: en pixels)
k_herb <- compute_kernel(radius=50, GSD=25, size_std=1.5)
k_carn <- compute_kernel(radius=150, GSD=25, size_std=1.5)

# Application de la dispersion sur le spacemodel
spcmdl_dispersal <- spcmdl |>
  dispersal("herbivore", method="convolution", method_option=list(kernel=k_herb)) |>
  dispersal("carnivore", method="convolution", method_option=list(kernel=k_carn))

# Note : le sol et les plantes sont évidemment statiques et ne dispersent pas.
```

------------------------------------------------------------------------

## 5. Exposition et Transfert des Contaminants

Maintenant que le système est en place, nous pouvons “injecter” la
concentration de notre polluant dans le sol, et modéliser sa
bioconcentration et sa bioaccumulation jusqu’au sommet de la chaîne
alimentaire grâce à la fonction
[`transfer()`](https://qonfluens.github.io/spacemodR/reference/transfer.md).

``` r

# 1. On assigne la grille de concentration réelle dans la couche "soil"
spcmdl_dispersal[["soil"]][] <- ground_cd

# 2. On définit les équations de transfert (Facteurs de Bioaccumulation / Bioconcentration)
intakes <- intake(spcmdl_dispersal,
  "soil -> plant" = ~ 10^x / 32,      # Equation spécifique pour les plantes
  "plant -> herbivore" = 0.5,         # Facteur simple pour l'herbivore
  "herbivore -> carnivore" = 0.8,     # Facteur simple pour le carnivore
  default = 1
)

# 3. On rassemble nos noyaux de dispersion pour le calcul de l'exposition
kernels <- list(soil = NA, plant = NA, herbivore = k_herb, carnivore = k_carn)

# 4. Calcul du transfert global dans l'écosystème
spcmdl_transfer <- transfer(spcmdl_dispersal, kernels, intakes)

# Visualisation de la contamination absorbée par le carnivore
color_transfer <- colorRampPalette(c("white", "#A33D0A"))(255)
terra::plot(spcmdl_transfer[["carnivore"]], 
            col=color_transfer, 
            main="Exposition estimée du Carnivore au Cadmium")
```

![](Getting_Started_FR_files/figure-html/transfer-1.png)

------------------------------------------------------------------------

## 6. Cartographie du Risque Écologique

L’étape finale de l’ERA consiste souvent à générer un **Indice de
Risque** (par exemple, le Quotient de Danger ou l’approche Eco-SSL).
Cela permet de cartographier les “Hot-Spots” où les seuils de toxicité
de référence sont dépassés.

``` r

# Définition des classes de risque (de "Sûr" à "Risque Très Élevé")
breaks_risk <- c(0, 0.1, 0.5, 1, 5, 10, Inf)
cols_risk <- c(
  "darkgreen",   # 0 - 0.1 (Pas de risque)
  "green",       # 0.1 - 0.5
  "lightgreen",  # 0.5 - 1 (Limite)
  "yellow",      # 1 - 5 (Risque modéré)
  "saddlebrown", # 5 - 10 (Risque fort)
  "#4A2C2A"      # > 10 (Risque très sévère)
)

# Projection du risque sur la zone d'étude
poly_vect <- terra::project(terra::vect(roi_metaleurop), terra::crs(spcmdl_transfer))
rast_crop <- terra::crop(spcmdl_transfer[["herbivore"]], poly_vect)
rast_final <- terra::mask(rast_crop, poly_vect)

terra::plot(rast_final,
            breaks = breaks_risk,
            col = cols_risk,
            main = "Cartographie de l'Indice de Risque (Herbivore)")
```

![](Getting_Started_FR_files/figure-html/eco_ssl_risk-1.png)

------------------------------------------------------------------------

## Prochaines Étapes

Vous venez de voir l’ensemble du pipeline `spacemodR` en action ! Pour
aller plus loin et configurer finement chaque étape, nous vous invitons
à consulter nos guides spécialisés :

- 🛠️ **Habitat Layer Management** : Construire des cartes de résistance
  et d’habitats complexes depuis des vecteurs.
- 🛠️ **Landscape Connectivity** : Intégrer la théorie des circuits
  (Omniscape) pour le mouvement des grands mammifères.
- 🦁 **The Example Zoo** : Explorer des cas d’études réels (comme le
  modèle Berisp complet).
