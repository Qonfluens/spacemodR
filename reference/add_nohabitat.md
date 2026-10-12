# Add non-habitat zones to a Habitat object

Add non-habitat zones to a Habitat object

## Usage

``` r
add_nohabitat(hab, sf_data, priority = NULL, touches = TRUE, ...)
```

## Arguments

- hab:

  A Habitat object

- sf_data:

  An sf object containing geometries to add

- priority:

  Stacking order of the added layer when the habitat is rasterized with
  [`habitat_raster`](https://qonfluens.github.io/spacemodR/reference/habitat_raster.md):
  the non-habitat zone only excludes the pixels where no layer of higher
  priority lies on top of it. Same default as in
  [`add_habitat`](https://qonfluens.github.io/spacemodR/reference/add_habitat.md):
  on top of everything already in `hab`.

- touches:

  Logical, a single value for the whole layer or one value per feature.
  If `TRUE` (default), every pixel touched by the features is excluded,
  so that the barrier is watertight: it cannot vanish between pixel
  centres nor be crossed diagonally between two pixels. If `FALSE`, a
  polygon only excludes the pixels whose centre it covers and a line
  only the pixels of a thin line. See
  [`add_habitat`](https://qonfluens.github.io/spacemodR/reference/add_habitat.md)
  for details.

- ...:

  Additional arguments

## Value

A Habitat object with new geometries added as non-habitat
