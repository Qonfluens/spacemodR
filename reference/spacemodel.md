# Create a spacemodel object

Constructor for the \`spacemodel\` class. This function combines spatial
data (a raster stack) and ecological data (a trophic table) into a
single object used for modelling.

## Usage

``` r
spacemodel(raster_stack, trophic_tbl)
```

## Arguments

- raster_stack:

  A
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object (multi-layer stack) representing the spatial distribution of
  the species or groups.

- trophic_tbl:

  An object of class `trophic_tbl` containing the ecological parameters
  and properties of the species.

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object with the following additional attributes:

- `trophic_tbl`: The `trophic_tbl` object passed as input.

- `spacemodel`: A logical flag set to `TRUE`, indicating this raster is
  part of a spacemodel.

## Details

The function performs several checks to ensure data consistency:

- Verifies that `raster_stack` is a `SpatRaster`.

- Verifies that `trophic_tbl` is a `trophic_tbl` object.

- Ensures the number of raster layers matches the number of levels in
  the trophic table.

- Ensures that the names of the raster layers match the names in the
  trophic table.

## See also

[`rast`](https://rspatial.github.io/terra/reference/rast.html),
`trophic_tbl`
