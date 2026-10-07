# Species habitat dictionary for EUNIS Level 2 land cover classes

Habitat suitability, breeding, foraging and movement scores of 935
European terrestrial vertebrates (amphibians, reptiles, birds, mammals)
for each of the 45 EUNIS Level 2 classes of the EEA Ecosystem Type Map
v3.1 (terrestrial part), as returned by
[`get_eunis_data`](https://qonfluens.github.io/spacemodR/reference/get_eunis_data.md)
with `level = "L2"`. It is the EUNIS counterpart of
[`ocsge_species_dict`](https://qonfluens.github.io/spacemodR/reference/ocsge_species_dict.md),
with an improved schema: scores follow an anchored rubric, breeding is
separated from foraging, and resistance is derived from permeability on
the 1-100 log scale expected by connectivity models such as Omniscape.

## Usage

``` r
data(eunis_species_dict)
```

## Format

A data frame with 42,075 rows (935 species x 45 EUNIS classes) and 13
columns:

- code_eunis:

  Character. EUNIS Level 2 code (e.g. `"G1"`); matches `ref_eunis$code`
  for `level == "L2"`.

- nom_espece:

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

  Integer. Species-level: number of EUNIS classes with
  `suitability >= 5`.

- specialization:

  Numeric (0-1). Species-level: 1 minus the normalised Shannon evenness
  of the suitability profile (0 generalist, 1 specialist).

- confidence:

  Integer (1-3). Species-level: 1 vague or missing source description, 2
  medium, 3 detailed.

- in_france:

  Integer (0/1). Species-level: 1 if the species occurs natively and
  regularly in metropolitan France (expert knowledge).

## Details

Scores were assigned by a large language model (Claude Opus 5.5) reading
the EEA habitat and avoided-habitat descriptions of each species
(`raw_data/spp_EEA_habitats_cleaned.csv`) against short ecological
descriptions of each EUNIS class. They are expert-judgement scores and
have not been validated against occurrence data. Four species had an
empty description and were scored from general knowledge
(`confidence = 1`). The scoring files, the comparison with
`ocsge_species_dict` and the build scripts are in
`raw_data/opus_scoring/` of the source repository.

## See also

[`get_eunis_data`](https://qonfluens.github.io/spacemodR/reference/get_eunis_data.md),
[`ref_eunis`](https://qonfluens.github.io/spacemodR/reference/ref_eunis.md),
[`ocsge_species_dict`](https://qonfluens.github.io/spacemodR/reference/ocsge_species_dict.md)

## Examples

``` r
data(eunis_species_dict)
subset(eunis_species_dict, nom_espece == "Apodemus_sylvaticus")
#>       code_eunis          nom_espece sppID taxon_group suitability
#> 41221         B1 Apodemus_sylvaticus   M19      mammal           8
#> 41222         B2 Apodemus_sylvaticus   M19      mammal           2
#> 41223         B3 Apodemus_sylvaticus   M19      mammal           1
#> 41224         C1 Apodemus_sylvaticus   M19      mammal           1
#> 41225         C2 Apodemus_sylvaticus   M19      mammal           1
#> 41226         C3 Apodemus_sylvaticus   M19      mammal           3
#> 41227         D1 Apodemus_sylvaticus   M19      mammal           3
#> 41228         D2 Apodemus_sylvaticus   M19      mammal           3
#> 41229         D3 Apodemus_sylvaticus   M19      mammal           4
#> 41230         D4 Apodemus_sylvaticus   M19      mammal           4
#> 41231         D5 Apodemus_sylvaticus   M19      mammal           5
#> 41232         D6 Apodemus_sylvaticus   M19      mammal           5
#> 41233         E1 Apodemus_sylvaticus   M19      mammal           7
#> 41234         E2 Apodemus_sylvaticus   M19      mammal           9
#> 41235         E3 Apodemus_sylvaticus   M19      mammal          10
#> 41236         E4 Apodemus_sylvaticus   M19      mammal           6
#> 41237         E6 Apodemus_sylvaticus   M19      mammal           0
#> 41238         E7 Apodemus_sylvaticus   M19      mammal           8
#> 41239         F1 Apodemus_sylvaticus   M19      mammal           6
#> 41240         F2 Apodemus_sylvaticus   M19      mammal           7
#> 41241         F3 Apodemus_sylvaticus   M19      mammal           9
#> 41242         F4 Apodemus_sylvaticus   M19      mammal           9
#> 41243         F5 Apodemus_sylvaticus   M19      mammal           8
#> 41244         F6 Apodemus_sylvaticus   M19      mammal           7
#> 41245         F7 Apodemus_sylvaticus   M19      mammal           4
#> 41246         F8 Apodemus_sylvaticus   M19      mammal           0
#> 41247         F9 Apodemus_sylvaticus   M19      mammal           8
#> 41248         FB Apodemus_sylvaticus   M19      mammal           7
#> 41249         G1 Apodemus_sylvaticus   M19      mammal           8
#> 41250         G2 Apodemus_sylvaticus   M19      mammal           8
#> 41251         G3 Apodemus_sylvaticus   M19      mammal           8
#> 41252         G4 Apodemus_sylvaticus   M19      mammal           9
#> 41253         G5 Apodemus_sylvaticus   M19      mammal           9
#> 41254         H2 Apodemus_sylvaticus   M19      mammal           4
#> 41255         H3 Apodemus_sylvaticus   M19      mammal           4
#> 41256         H4 Apodemus_sylvaticus   M19      mammal           0
#> 41257         H5 Apodemus_sylvaticus   M19      mammal           3
#> 41258         I1 Apodemus_sylvaticus   M19      mammal           8
#> 41259         I2 Apodemus_sylvaticus   M19      mammal           8
#> 41260         J1 Apodemus_sylvaticus   M19      mammal           5
#> 41261         J2 Apodemus_sylvaticus   M19      mammal           5
#> 41262         J3 Apodemus_sylvaticus   M19      mammal           3
#> 41263         J4 Apodemus_sylvaticus   M19      mammal           1
#> 41264         J5 Apodemus_sylvaticus   M19      mammal           2
#> 41265         J6 Apodemus_sylvaticus   M19      mammal           1
#>       weight_breeding weight_foraging permeability resistance habitat_breadth
#> 41221               8               8            9        1.6              25
#> 41222               2               2            3       25.1              25
#> 41223               1               1            2       39.8              25
#> 41224               1               1            1       63.1              25
#> 41225               1               1            1       63.1              25
#> 41226               3               3            4       15.8              25
#> 41227               3               3            4       15.8              25
#> 41228               3               3            4       15.8              25
#> 41229               4               4            5       10.0              25
#> 41230               4               4            5       10.0              25
#> 41231               5               5            6        6.3              25
#> 41232               5               5            6        6.3              25
#> 41233               7               7            8        2.5              25
#> 41234               9               9           10        1.0              25
#> 41235              10              10           10        1.0              25
#> 41236               6               6            7        4.0              25
#> 41237               0               0            1       63.1              25
#> 41238               8               8            9        1.6              25
#> 41239               6               6            7        4.0              25
#> 41240               7               7           10        1.0              25
#> 41241               9               9           10        1.0              25
#> 41242               9               9           10        1.0              25
#> 41243               8               8            9        1.6              25
#> 41244               7               7            8        2.5              25
#> 41245               4               4            5       10.0              25
#> 41246               0               0            1       63.1              25
#> 41247               8               8            9        1.6              25
#> 41248               7               7            8        2.5              25
#> 41249               8               8            9        1.6              25
#> 41250               8               8            9        1.6              25
#> 41251               8               8            9        1.6              25
#> 41252               9               9            9        1.6              25
#> 41253               9               9            9        1.6              25
#> 41254               4               4            5       10.0              25
#> 41255               4               4            5       10.0              25
#> 41256               0               0            1       63.1              25
#> 41257               3               3            4       15.8              25
#> 41258               8               8            9        1.6              25
#> 41259               8               8            9        1.6              25
#> 41260               5               5            6        6.3              25
#> 41261               5               5            6        6.3              25
#> 41262               3               3            4       15.8              25
#> 41263               1               1            1       63.1              25
#> 41264               2               2            3       25.1              25
#> 41265               1               1            1       63.1              25
#>       specialization confidence in_france
#> 41221           0.06          3         1
#> 41222           0.06          3         1
#> 41223           0.06          3         1
#> 41224           0.06          3         1
#> 41225           0.06          3         1
#> 41226           0.06          3         1
#> 41227           0.06          3         1
#> 41228           0.06          3         1
#> 41229           0.06          3         1
#> 41230           0.06          3         1
#> 41231           0.06          3         1
#> 41232           0.06          3         1
#> 41233           0.06          3         1
#> 41234           0.06          3         1
#> 41235           0.06          3         1
#> 41236           0.06          3         1
#> 41237           0.06          3         1
#> 41238           0.06          3         1
#> 41239           0.06          3         1
#> 41240           0.06          3         1
#> 41241           0.06          3         1
#> 41242           0.06          3         1
#> 41243           0.06          3         1
#> 41244           0.06          3         1
#> 41245           0.06          3         1
#> 41246           0.06          3         1
#> 41247           0.06          3         1
#> 41248           0.06          3         1
#> 41249           0.06          3         1
#> 41250           0.06          3         1
#> 41251           0.06          3         1
#> 41252           0.06          3         1
#> 41253           0.06          3         1
#> 41254           0.06          3         1
#> 41255           0.06          3         1
#> 41256           0.06          3         1
#> 41257           0.06          3         1
#> 41258           0.06          3         1
#> 41259           0.06          3         1
#> 41260           0.06          3         1
#> 41261           0.06          3         1
#> 41262           0.06          3         1
#> 41263           0.06          3         1
#> 41264           0.06          3         1
#> 41265           0.06          3         1
```
