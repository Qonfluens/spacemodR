# Crosswalk between OCS-GE (cover and use) and EUNIS Level 2

Correspondence table between the French OCS-GE nomenclature and the 45
EUNIS Level 2 classes of
[`ref_eunis`](https://qonfluens.github.io/spacemodR/reference/ref_eunis.md).
The two nomenclatures do not nest: OCS-GE cover (`CS`) describes what
physically covers the ground in 14 classes, while EUNIS describes
habitats. The relation is therefore many-to-many, and each OCS-GE class
is given a ranked list of EUNIS classes. Rows with a land use code
(`US`) refine the match for a given cover: for instance herbaceous cover
(`CS2.2.1`) is arable land (`I1`) under agricultural use, but gardens
and parks (`I2`) under residential use.

## Usage

``` r
data(ref_ocsge_eunis)
```

## Format

A data frame with 152 rows and 7 columns:

- code_cs:

  Character. OCS-GE cover code, as in `ref_ocsge$code_cs` (e.g.
  `"CS2.2.1"`).

- code_us:

  Character. OCS-GE use code (e.g. `"US1.1"`), or `NA` for the
  cover-only match, to be used when the use is unknown or has no
  dedicated row.

- code_eunis:

  Character. EUNIS Level 2 code, as in `ref_eunis$code` and
  `eunis_species_dict$code_eunis`.

- rank:

  Integer. Order of the EUNIS classes within a (`code_cs`, `code_us`)
  group; `1` is the default match.

- main_cs:

  Logical. `TRUE` on the single row giving the main OCS-GE cover of each
  EUNIS class (EUNIS to OCS-GE direction, 45 rows).

- pct_metaleurop:

  Numeric. Measured share (%) of the OCS-GE group area falling in this
  EUNIS class on the Metaleurop site; `NA` when the group covers less
  than 10 ha there. See Details.

- note:

  Character. What the match stands for, and what extra data would be
  needed to resolve it.

## Details

`rank` is an expert reading of the two nomenclatures (CNIG OCS-GE
specification and EUNIS habitat definitions). `pct_metaleurop` is a
measurement: OCS-GE polygons of
[`roi_metaleurop`](https://qonfluens.github.io/spacemodR/reference/roi_metaleurop.md)
were overlaid on the EUNIS raster returned by
[`get_eunis_data`](https://qonfluens.github.io/spacemodR/reference/get_eunis_data.md).
The two columns are not expected to agree closely, because the EUNIS
raster has a 100 m pixel carrying one dominant class, while OCS-GE maps
objects a few meters wide: a road or a garden usually falls in a pixel
classified as buildings (`J1`, `J2`). The measurement also only covers
the classes present on this site.

Some EUNIS distinctions cannot be recovered from OCS-GE, with or without
the use: wetlands (`D`), grassland types (`E1` to `E4`), standing versus
running water (`C1`, `C2`), deciduous versus evergreen broadleaved
woodland (`G1`, `G2`), and crops versus permanent grassland, which share
the same cover and the same agricultural use.

The source table and the build script are
`raw_data/ocsge_eunis_crosswalk.csv` and
`raw_data/build_ref_ocsge_eunis.R` of the source repository.

## See also

[`ref_ocsge`](https://qonfluens.github.io/spacemodR/reference/ref_ocsge.md),
[`ref_eunis`](https://qonfluens.github.io/spacemodR/reference/ref_eunis.md),
[`eunis_species_dict`](https://qonfluens.github.io/spacemodR/reference/eunis_species_dict.md),
[`get_ocsge_data_fgb`](https://qonfluens.github.io/spacemodR/reference/get_ocsge_data_fgb.md)

## Examples

``` r
data(ref_ocsge_eunis)

# default EUNIS class of each OCS-GE cover
subset(ref_ocsge_eunis, is.na(code_us) & rank == 1)
#>       code_cs code_us code_eunis rank main_cs pct_metaleurop
#> 1   CS1.1.1.1    <NA>         J1    1    TRUE             52
#> 5   CS1.1.1.2    <NA>         J4    1    TRUE             10
#> 8   CS1.1.2.1    <NA>         J4    1   FALSE              3
#> 23  CS1.1.2.2    <NA>         J6    1    TRUE             NA
#> 24    CS1.2.1    <NA>         H5    1    TRUE              0
#> 36    CS1.2.2    <NA>         C1    1    TRUE              2
#> 52    CS1.2.3    <NA>         H4    1    TRUE             NA
#> 53  CS2.1.1.1    <NA>         G1    1    TRUE             53
#> 66  CS2.1.1.2    <NA>         G3    1    TRUE             NA
#> 76  CS2.1.1.3    <NA>         G4    1    TRUE             NA
#> 86    CS2.1.2    <NA>         F3    1    TRUE              0
#> 100   CS2.1.3    <NA>         FB    1    TRUE             NA
#> 101   CS2.2.1    <NA>         I1    1    TRUE             54
#> 148   CS2.2.2    <NA>         D1    1    TRUE             NA
#>                                                                                   note
#> 1                                                        Buildings of the urban fabric
#> 5                                             Sealed ground: roads, car parks, squares
#> 8                          Railway ballast, gravel tracks, unsealed transport surfaces
#> 23                                              Landfills, dumps, slag and spoil heaps
#> 24                      Bare ground, sand and gravel, burnt or recently disturbed land
#> 36  Lakes, ponds, pools. Standing and running water cannot be separated by cover alone
#> 52                                                         Glaciers and permanent snow
#> 53                                                      Broadleaved deciduous woodland
#> 66                                                                 Coniferous woodland
#> 76                                             Mixed deciduous and coniferous woodland
#> 86                                          Temperate thickets, hedges, bramble, broom
#> 100                  Vineyards and other woody crops that are neither trees nor shrubs
#> 101           Arable crops. In OCS-GE crops are herbaceous cover with agricultural use
#> 148                                                 Raised and blanket bogs (Sphagnum)

# main OCS-GE cover of each EUNIS class
subset(ref_ocsge_eunis, main_cs, c(code_eunis, code_cs))
#>     code_eunis   code_cs
#> 1           J1 CS1.1.1.1
#> 2           J2 CS1.1.1.1
#> 5           J4 CS1.1.1.2
#> 9           J3 CS1.1.2.1
#> 23          J6 CS1.1.2.2
#> 24          H5   CS1.2.1
#> 25          H3   CS1.2.1
#> 26          H2   CS1.2.1
#> 27          B1   CS1.2.1
#> 28          B2   CS1.2.1
#> 29          B3   CS1.2.1
#> 36          C1   CS1.2.2
#> 37          C2   CS1.2.2
#> 38          J5   CS1.2.2
#> 52          H4   CS1.2.3
#> 53          G1 CS2.1.1.1
#> 54          G2 CS2.1.1.1
#> 55          G5 CS2.1.1.1
#> 66          G3 CS2.1.1.2
#> 76          G4 CS2.1.1.3
#> 86          F3   CS2.1.2
#> 87          F4   CS2.1.2
#> 88          F5   CS2.1.2
#> 89          F6   CS2.1.2
#> 90          F9   CS2.1.2
#> 91          F2   CS2.1.2
#> 92          F7   CS2.1.2
#> 95          F8   CS2.1.2
#> 100         FB   CS2.1.3
#> 101         I1   CS2.2.1
#> 102         E2   CS2.2.1
#> 103         I2   CS2.2.1
#> 104         E1   CS2.2.1
#> 105         E3   CS2.2.1
#> 106         E4   CS2.2.1
#> 107         E7   CS2.2.1
#> 108         D5   CS2.2.1
#> 109         C3   CS2.2.1
#> 110         D2   CS2.2.1
#> 111         D4   CS2.2.1
#> 112         D6   CS2.2.1
#> 113         E6   CS2.2.1
#> 148         D1   CS2.2.2
#> 151         F1   CS2.2.2
#> 152         D3   CS2.2.2

# EUNIS class of OCS-GE polygons from cover and use, falling back on cover alone
ocs <- data.frame(code_cs = c("CS2.2.1", "CS2.2.1", "CS2.1.1.1"),
                  code_us = c("US1.1", "US5", "US1.2"))
best <- subset(ref_ocsge_eunis, rank == 1)
with_us <- best$code_eunis[match(paste(ocs$code_cs, ocs$code_us),
                                 paste(best$code_cs, best$code_us))]
cs_only <- best$code_eunis[match(paste(ocs$code_cs, NA),
                                 paste(best$code_cs, best$code_us))]
ocs$code_eunis <- ifelse(is.na(with_us), cs_only, with_us)
ocs
#>     code_cs code_us code_eunis
#> 1   CS2.2.1   US1.1         I1
#> 2   CS2.2.1     US5         I2
#> 3 CS2.1.1.1   US1.2         G1
```
