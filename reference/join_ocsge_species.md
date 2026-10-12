# Join OCSGE Spatial Data with Species Traits

This function merges a spatial \`sf\` object containing OCSGE
(Occupation du Sol à Grande Échelle) polygons with a dictionary of
species-specific traits (e.g., habitat suitability, resistance).
Polygons are matched on their cover code (`CS`) and, when the object
also carries the land use (`US`), on the (cover, use) pair.

## Usage

``` r
join_ocsge_species(
  sf_obj,
  species_pattern,
  code_col = "code_cs",
  species_dict = spacemodR::ocsge_species_dict,
  ref_dict = spacemodR::ref_ocsge,
  us_col = "code_us"
)
```

## Arguments

- sf_obj:

  A spatial `sf` object containing the landscape polygons.

- species_pattern:

  Character. A regex pattern or exact name to search for in the species
  dictionary.

- code_col:

  Character. The name of the column in `sf_obj` that contains the OCSGE
  cover codes. Default is `"code_cs"`.

- species_dict:

  A data.frame containing the species traits. Defaults to
  [`spacemodR::ocsge_species_dict`](https://qonfluens.github.io/spacemodR/reference/ocsge_species_dict.md).

- ref_dict:

  An optional reference data.frame giving the label and color of each
  cover code, added to the result. Defaults to
  [`spacemodR::ref_ocsge`](https://qonfluens.github.io/spacemodR/reference/ref_ocsge.md).

- us_col:

  Character. The name of the column in `sf_obj` that contains the OCSGE
  use codes. Default is `"code_us"`. Ignored when the column is absent;
  set to `NULL` to score polygons on their cover only.

## Value

An `sf` object enriched with the species traits. Returns `NULL` if the
species is not found.

## Details

Cover codes are compared without spaces, so `"CS2.2.1"` and `"CS 2.2.1"`
are the same class.

In
[`ocsge_species_dict`](https://qonfluens.github.io/spacemodR/reference/ocsge_species_dict.md),
rows with `code_us = NA` hold the scores of a cover whatever its use,
and rows with a `code_us` hold the scores of a (cover, use) pair. A
polygon takes the scores of its (cover, use) pair when the dictionary
has one, and the scores of its cover otherwise. The logical column
`use_matched` of the result tells which of the two was used.
