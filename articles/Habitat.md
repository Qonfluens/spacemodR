# Habitat

``` r
library(spacemodR)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

## Weighted species habitat with OCS-GE layer

### How it works:

We have build a database linking OCSGE label with species label.

``` r
data("ocsge_species_dict")
dfhab <- ocsge_species_dict
dfhab_Apsy <- dfhab[grepl("Apodemus_sylvaticus", dfhab$nom_espece), ]
dfhab_Apsy
#>          code_cs weight_global weight_movement weight_foraging resistance
#> 12825 CS 1.1.1.1             3               1               2         10
#> 12826 CS 1.1.1.2             3               1               1         10
#> 12827 CS 1.1.2.1             5               7               4          5
#> 12828 CS 1.1.2.2             6               8               5          4
#> 12829   CS 1.2.1             7               9               6          2
#> 12830   CS 1.2.2             0               0               0         10
#> 12831   CS 1.2.3             0               0               0         10
#> 12832 CS 2.1.1.1             9               7               9          3
#> 12833 CS 2.1.1.2             8               6               8          4
#> 12834 CS 2.1.1.3             9               7               9          3
#> 12835   CS 2.1.2             8               7               7          3
#> 12836   CS 2.1.3             7               6               7          4
#> 12837   CS 2.2.1             8               8               7          2
#> 12838   CS 2.2.2             1               3               1          8
#>                nom_espece
#> 12825 Apodemus_sylvaticus
#> 12826 Apodemus_sylvaticus
#> 12827 Apodemus_sylvaticus
#> 12828 Apodemus_sylvaticus
#> 12829 Apodemus_sylvaticus
#> 12830 Apodemus_sylvaticus
#> 12831 Apodemus_sylvaticus
#> 12832 Apodemus_sylvaticus
#> 12833 Apodemus_sylvaticus
#> 12834 Apodemus_sylvaticus
#> 12835 Apodemus_sylvaticus
#> 12836 Apodemus_sylvaticus
#> 12837 Apodemus_sylvaticus
#> 12838 Apodemus_sylvaticus
```

``` r
data("ref_ocsge")
dfhab_Apsy_def <- dplyr::left_join(dfhab_Apsy, ref_ocsge, by = c("code_cs" = "code_cs_"))
```

And therefore we can distinguish: - Non habitat area:

``` r
wg = dfhab_Apsy_def$weight_global
dfhab_Apsy_def[wg == 0,]$nomenclature
#> [1] "Surfaces d'eau"    "Névés et glaciers"
```

- Very Poor habitat:

``` r
dfhab_Apsy_def[wg > 0 & wg <= 3,]$nomenclature
#> [1] "Zones bâties"                    "Zones non bâties"               
#> [3] "Autres formations non ligneuses"
```

- Poor habitat:

``` r
dfhab_Apsy_def[wg > 3 & wg <= 7, ]$nomenclature
#> [1] "Matériaux minéraux"          "Matériaux composites"       
#> [3] "Sols nus"                    "Autres formations ligneuses"
```

- Good habitat:

``` r
dfhab_Apsy_def[wg > 7,]$nomenclature
#> [1] "Feuillus"                               
#> [2] "Conifères"                              
#> [3] "Mixte"                                  
#> [4] "Formations arbustives, sous-arbrisseaux"
#> [5] "Formations herbacées"
```

### A classification of species

``` r
dfhab_full <- dplyr::left_join(dfhab, ref_ocsge, by = c("code_cs" = "code_cs_"))

dfhab_grouped <- dfhab_full %>%
  tidyr::separate(nom_espece, into = c("genus", "epithet"), sep = "_", remove = FALSE) %>%
  dplyr::group_by(genus, nomenclature) %>%
  dplyr::summarise(mean_weighted = mean(weight_global, na.rm = TRUE), .groups = 'drop')
#> Warning: Expected 2 pieces. Additional pieces discarded in 14 rows [10501, 10502, 10503,
#> 10504, 10505, 10506, 10507, 10508, 10509, 10510, 10511, 10512, 10513, 10514].

dfhab_matrix <- dfhab_grouped %>%
  tidyr::pivot_wider(names_from = nomenclature, values_from = mean_weighted, values_fill = 0) %>%
  tibble::column_to_rownames(var = "genus")

# 4. Clustering Hiérarchique (Méthode de Ward pour des groupes compacts)
dist_matrix <- dist(dfhab_matrix, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
```

``` r
plot(hc, main = "Dendrogram", xlab = "Genre", sub = "")
```

![](Habitat_files/figure-html/unnamed-chunk-9-1.png)

``` r
# library(factoextra)
# 
# plt_circ_dend = fviz_dend(hc, k = 4, # On suggère 4 groupes de couleurs
#           cex = 0.5,                 # Taille du texte
#           type = "circular",         # Format circulaire
#           palette = "jco",           # Palette de couleurs
#           rect = TRUE,               # Cadre autour des groupes
#           main = "Genus distance for OCS-GE layer")
# 
# plt_circ_dend
```

``` r
# install.packages("pheatmap")
# library(pheatmap)
# pheatmap(dfhab_matrix, 
#          clustering_distance_rows = "euclidean",
#          clustering_method = "ward.D2",
#          scale = "column", # Normalise par habitat pour voir les préférences
#          color = colorRampPalette(c("white", "orange", "red"))(50),
#          main = "Preferential  OCS-GE layer per Genus (Mean weight)")
```
