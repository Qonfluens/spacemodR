# Add habitat zones to a Habitat object

Add habitat zones to a Habitat object

## Usage

``` r
add_habitat(hab, sf_data, weight = 1, ...)
```

## Arguments

- hab:

  A Habitat object

- sf_data:

  An sf object containing geometries to add

- weight:

  A weight to apply on the habitat feature. Can then be used as
  resistance for dispersal functions for instance.

- ...:

  Additional arguments

## Value

A Habitat object with new geometries added as habitat
