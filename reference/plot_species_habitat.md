# Plot Habitat Suitability Maps from OCSGE Data

Generates a composite plot of three spatial maps (suitability, foraging
weight and resistance) from an \`sf\` object previously enriched with
species traits.

## Usage

``` r
plot_species_habitat(sf_merged, title_species = NULL)
```

## Arguments

- sf_merged:

  An \`sf\` object enriched with species traits (e.g., output of
  [`join_ocsge_species`](https://qonfluens.github.io/spacemodR/reference/join_ocsge_species.md)).

- title_species:

  Character (optional). The name of the species to display in the title.
  If NULL, it will try to retrieve the name from the object attributes.

## Value

A `patchwork` object containing the combined `ggplot2` maps.

## Details

Suitability and foraging weight are drawn on their full 0-10 scale, and
resistance on its 1-100 logarithmic scale, so that maps of different
species can be compared.
