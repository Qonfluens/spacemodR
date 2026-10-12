# OCS-GE land cover and land use of the Metaleurop site

OCS-GE polygons of
[`roi_metaleurop`](https://qonfluens.github.io/spacemodR/reference/roi_metaleurop.md)
with both the cover (`CS`) and the use (`US`), dissolved into one
multipolygon per (cover, use) pair.
[`ocsge_metaleurop`](https://qonfluens.github.io/spacemodR/reference/ocsge_metaleurop.md)
is dissolved by cover only and cannot be used to study the land use.

## Usage

``` r
data(ocsge_us_metaleurop)
```

## Format

An `sf` object (EPSG:2154) with 68 multipolygons and 3 columns:

- code_cs:

  Character. OCS-GE cover code (e.g. `"CS2.2.1"`).

- code_us:

  Character. OCS-GE use code (e.g. `"US1.1"`).

- area_ha:

  Numeric. Area of the (cover, use) pair on the site, in hectares.

## Details

Built from the 2021 OCS-GE of the Nord and Pas-de-Calais departments, as
returned by
[`get_ocsge_data_fgb`](https://qonfluens.github.io/spacemodR/reference/get_ocsge_data_fgb.md).
Outlines are simplified with a 3 m tolerance to keep the dataset small.

## See also

[`join_ocsge_species`](https://qonfluens.github.io/spacemodR/reference/join_ocsge_species.md),
[`ocsge_species_dict`](https://qonfluens.github.io/spacemodR/reference/ocsge_species_dict.md)

## Examples

``` r
data(ocsge_us_metaleurop)
head(ocsge_us_metaleurop)
#> Simple feature collection with 6 features and 3 fields
#> Attribute-geometry relationships: identity (2), NAs (1)
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 698346.3 ymin: 7032687 xmax: 707087.5 ymax: 7041903
#> Projected CRS: RGF93 v1 / Lambert-93
#>     code_cs code_us area_ha                       geometry
#> 1 CS1.1.1.1   US1.1    6.17 MULTIPOLYGON (((699461.3 70...
#> 2 CS1.1.1.1   US1.3    0.02 MULTIPOLYGON (((701605.2 70...
#> 3 CS1.1.1.1     US2   24.80 MULTIPOLYGON (((700428.8 70...
#> 4 CS1.1.1.1     US3   53.23 MULTIPOLYGON (((699179.5 70...
#> 5 CS1.1.1.1 US4.1.2    0.11 MULTIPOLYGON (((699057.6 70...
#> 6 CS1.1.1.1   US4.2   39.58 MULTIPOLYGON (((702061.4 70...
```
