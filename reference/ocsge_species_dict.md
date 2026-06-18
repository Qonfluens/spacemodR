# Valued weights and resistance between OCS-GE layers and species

A dataset providing detailed ecological scores linking European
terrestrial vertebrate species to French OCS-GE (Occupation du Sol à
Grande Échelle) land cover classes.

Unlike traditional expert-based crosswalks that assign simple
presence/absence or discrete suitability scores (e.g., 0, 1, 2), this
dataset was generated using a Large Language Model (LLM - Google
Gemini). The model semantically analyzed free-text descriptions of
preferred and avoided habitats (derived from the EEA dataset) against
ecological definitions of OCS-GE classes to infer granular scores (0 to
10).

These scores are specifically designed for spatial ecology modeling,
separating general suitability from movement permeability and foraging
potential.

## Usage

``` r
data(ocsge_species_dict)
```

## Format

A data frame with the following columns:

- nom_espece:

  Character. The scientific name of the species.

- code_cs:

  Character. The OCS-GE layer code (e.g., "CS 1.1.1.1").

- weight_global:

  Numeric (0-10). The overall habitat suitability of the OCS-GE layer
  for the species.

- weight_movement:

  Numeric (0-10). The permeability of the layer, representing how easily
  the species can disperse through it.

- weight_foraging:

  Numeric (0-10). The suitability of the layer specifically for feeding
  and resource acquisition.

- resistance:

  Numeric (0-10). The impedance or barrier effect of the layer, heavily
  influenced by known avoided habitats.

## Details

The methodology differs significantly from global Area of Habitat (AOH)
approaches. While global AOH maps rely on hard-coded rules and
keyword-based penalties to score land use intensity, this dataset
leverages AI to interpret complex ecological nuances (e.g., thermophily,
physical barriers, seasonal cover) described in the OCS-GE standard.

## References

The underlying conceptual framework for linking terrestrial vertebrate
species to European land systems, as well as the baseline habitat
descriptions, draws inspiration from:

O'Connor, L. M. J., Renaud, J., Dou, Y., Karger, D. N., Maiorano, L.,
Verburg, P. H., & Thuiller, W. (2024). Habitat Suitability of European
Land Systems for Terrestrial Vertebrates. *Global Ecology and
Biogeography*, 33(11), e13903.

## Examples

``` r
data(ocsge_species_dict)
head(ocsge_species_dict)
#>      code_cs weight_global weight_movement weight_foraging resistance
#> 1 CS 1.1.1.1             1               1               1         10
#> 2 CS 1.1.1.2             1               1               1         10
#> 3 CS 1.1.2.1             3               6               4          5
#> 4 CS 1.1.2.2             5               7               5          4
#> 5   CS 1.2.1             3               8               3          4
#> 6   CS 1.2.2             9              10               6          1
#>           nom_espece
#> 1 Alytes_cisternasii
#> 2 Alytes_cisternasii
#> 3 Alytes_cisternasii
#> 4 Alytes_cisternasii
#> 5 Alytes_cisternasii
#> 6 Alytes_cisternasii
```
