# Apply trophic flux from a resource layer

Computes the contribution of a resource raster to a consumer layer using
the normalized weight and a flux function stored in the trophic table.

## Usage

``` r
flux(raster, intakes, from, to)
```

## Arguments

- raster:

  A `SpatRaster` (or similar) representing exposure from the resource.

- intakes:

  A `trophic_tbl` object that must contain a column `normalized_weight`
  and a column `flux`.

- from:

  Character string, name of the source node.

- to:

  Character string, name of the target node.

## Value

A raster object with transformed values.

## Details

The function extracts the link corresponding to `from -> to` from the
trophic table and applies:

1.  the associated flux function to the raster values

2.  the normalized weight of the link

An error is thrown if no such link exists.
