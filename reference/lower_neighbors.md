# Get resource layers for a given trophic layer

Returns the upstream neighbors (prey/resources) of a given layer in a
trophic graph.

## Usage

``` r
lower_neighbors(trophic_tbl, layer)
```

## Arguments

- trophic_tbl:

  A `trophic_tbl` object

- layer:

  Name of the layer (string) in the spacemodel.

## Value

A character vector of names of layers that are resources for `layer`.
