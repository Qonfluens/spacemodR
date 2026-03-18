# Join OCSGE Spatial Data with Species Traits

This function merges a spatial \`sf\` object containing OCSGE
(Occupation du Sol à Grande Échelle) polygons with a dictionary of
species-specific traits (e.g., habitat suitability weights, resistance).
It handles potential code bridging via a reference dictionary.

## Usage

``` r
join_ocsge_species(
  sf_obj,
  species_pattern,
  code_col = "code_cs",
  species_dict = spacemodR::ocsge_species_dict,
  ref_dict = spacemodR::ref_ocsge
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
  codes. Default is `"code_cs"`.

- species_dict:

  A data.frame containing the species traits. Defaults to
  [`spacemodR::ocsge_species_dict`](https://qonfluens.github.io/spacemodR/reference/ocsge_species_dict.md).

- ref_dict:

  An optional reference data.frame to bridge codes between the spatial
  object and the species dictionary. Defaults to
  [`spacemodR::ref_ocsge`](https://qonfluens.github.io/spacemodR/reference/ref_ocsge.md).

## Value

An `sf` object enriched with the species traits. Returns `NULL` if the
species is not found.
