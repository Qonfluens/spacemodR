# Species habitat dictionary for OCS-GE land cover and land use

Habitat suitability, breeding, foraging and movement scores of 935
European terrestrial vertebrates (amphibians, reptiles, birds, mammals)
for the French OCS-GE nomenclature: the 14 cover classes (`CS`), and 47
(cover, use) pairs for which the land use (`US`) changes what the cover
stands for. It has the same columns as
[`eunis_species_dict`](https://qonfluens.github.io/spacemodR/reference/eunis_species_dict.md),
plus `code_us`.

## Usage

``` r
data(ocsge_species_dict)
```

## Format

A data frame with 57,035 rows (935 species x 61 classes) and 14 columns:

- code_cs:

  Character. OCS-GE cover code, as in `ref_ocsge$code_cs` (e.g.
  `"CS2.2.1"`).

- code_us:

  Character. OCS-GE use code (e.g. `"US1.1"`), or `NA` on the rows
  giving the scores of the cover whatever its use.

- species_name:

  Character. Scientific name, genus and species separated by `"_"`.

- sppID:

  Character. Species identifier from the EEA source table; the first
  letter gives the group (A, R, B, M).

- taxon_group:

  Character. `"amphibian"`, `"reptile"`, `"bird"` or `"mammal"`.

- suitability:

  Integer (0-10). Overall habitat suitability: 0 unused or avoided, 2
  marginal, 5 secondary, 8 main, 10 core habitat.

- weight_breeding:

  Integer (0-10). Value of the class as a breeding, nesting, denning,
  roosting or larval site.

- weight_foraging:

  Integer (0-10). Value of the class for feeding.

- permeability:

  Integer (0-10). Ease of movement through the class: 10 moves freely, 7
  readily, 4 reluctantly, 1 strong barrier, 0 impassable. Accounts for
  flight, swimming or fossorial habits.

- resistance:

  Numeric (1-100). Derived from permeability as
  `10^(2 * (1 - permeability / 10))`: 10 gives 1, 5 gives 10, 0 gives
  100.

- habitat_breadth:

  Integer. Species-level: number of cover classes (out of 14) with
  `suitability >= 5`.

- specialization:

  Numeric (0-1). Species-level: 1 minus the normalised Shannon evenness
  of the suitability profile over the 14 cover classes (0 generalist, 1
  specialist).

- confidence:

  Integer (1-3). Species-level: 1 vague or missing source description, 2
  medium, 3 detailed.

- in_france:

  Integer (0/1). Species-level: 1 if the species occurs natively and
  regularly in metropolitan France (expert knowledge). The dictionary
  covers Europe while OCS-GE covers France: filter on this column first.

## Details

**Cover rows** (`code_us = NA`, 935 x 14). Scores were assigned by a
large language model (Claude Opus 5.5) reading the EEA habitat and
avoided-habitat descriptions of each species against an ecological
description of each cover class, with the anchored rubric given above.

**Cover x use rows** (935 x 47). They are derived, not scored again:
each (cover, use) pair is linked to one or several EUNIS Level 2 classes
by
[`ref_ocsge_eunis`](https://qonfluens.github.io/spacemodR/reference/ref_ocsge_eunis.md),
and takes the mean of their
[`eunis_species_dict`](https://qonfluens.github.io/spacemodR/reference/eunis_species_dict.md)
scores weighted by `1 / rank`. For instance herbaceous cover under
agricultural use (`CS2.2.1`, `US1.1`) is mostly arable land (`I1`,
rank 1) but may be permanent grassland (`E2`, rank 2). Only the pairs
listed in `ref_ocsge_eunis` have a row; for any other pair use the cover
row, which is what
[`join_ocsge_species`](https://qonfluens.github.io/spacemodR/reference/join_ocsge_species.md)
does.

These are expert-judgement scores and have not been validated against
occurrence data. They were checked against the keywords of the EEA
habitat, avoided-habitat and threat descriptions
(`raw_data/check_ocsge_species_dict.R` of the source repository):
species naming a habitat score higher on the matching class than the
others, with a probability of 0.57 to 0.93 depending on the habitat for
cover rows, and 0.68 to 0.93 for cover x use rows. The use helps most
for gardens and parks (0.57 to 0.80), quarries (0.69 to 0.93) and arable
land (0.74 to 0.77).

**Change from spacemodR 0.3.0.** The dictionary used to hold scores from
another model for the cover classes only, in the columns
`weight_global`, `weight_movement`, `weight_foraging` and `resistance`
(1-10), with `nom_espece` and cover codes written with a space
(`"CS 2.2.1"`). `suitability` replaces `weight_global` and
`permeability` replaces `weight_movement`; values differ (Spearman
correlation 0.72 for suitability, 0.22 for movement, mostly because
flying species were scored as if they walked) and are lower on classes
that are not habitat.

## References

The species list and the habitat descriptions come from:

O'Connor, L. M. J., Renaud, J., Dou, Y., Karger, D. N., Maiorano, L.,
Verburg, P. H., & Thuiller, W. (2024). Habitat Suitability of European
Land Systems for Terrestrial Vertebrates. *Global Ecology and
Biogeography*, 33(11), e13903.

## See also

[`join_ocsge_species`](https://qonfluens.github.io/spacemodR/reference/join_ocsge_species.md),
[`ref_ocsge`](https://qonfluens.github.io/spacemodR/reference/ref_ocsge.md),
[`ref_ocsge_eunis`](https://qonfluens.github.io/spacemodR/reference/ref_ocsge_eunis.md),
[`eunis_species_dict`](https://qonfluens.github.io/spacemodR/reference/eunis_species_dict.md)

## Examples

``` r
data(ocsge_species_dict)
apsy <- subset(ocsge_species_dict, species_name == "Apodemus_sylvaticus")

# scores of each cover, whatever its use
subset(apsy, is.na(code_us), c(code_cs, suitability, permeability, resistance))
#>         code_cs suitability permeability resistance
#> 55877 CS1.1.1.1           5            4       15.8
#> 55879 CS1.1.1.2           2            1       63.1
#> 55880 CS1.1.2.1           2            2       39.8
#> 55887 CS1.1.2.2           0            1       63.1
#> 55888   CS1.2.1           3            3       25.1
#> 55891   CS1.2.2           0            0      100.0
#> 55898   CS1.2.3           0            0      100.0
#> 55899 CS2.1.1.1           8            9        1.6
#> 55906 CS2.1.1.2           8            9        1.6
#> 55912 CS2.1.1.3           8            9        1.6
#> 55918   CS2.1.2           9           10        1.0
#> 55921   CS2.1.3           8            9        1.6
#> 55922   CS2.2.1           8            9        1.6
#> 55937   CS2.2.2           2            4       15.8

# herbaceous cover, by use
subset(apsy, code_cs == "CS2.2.1", c(code_us, suitability, weight_foraging))
#>       code_us suitability weight_foraging
#> 55922    <NA>           8               8
#> 55923   US1.1           8               8
#> 55924   US1.2           9               9
#> 55925   US1.3           5               5
#> 55926     US2           9               9
#> 55927   US235           8               8
#> 55928     US3           8               8
#> 55929 US4.1.1           6               6
#> 55930 US4.1.2           6               6
#> 55931 US4.1.3           6               6
#> 55932   US4.2           9               9
#> 55933   US4.3           9               9
#> 55934     US5           8               8
#> 55935   US6.2           9               9
#> 55936   US6.3           8               8
```
