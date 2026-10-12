# Add habitat zones to a Habitat object

Add habitat zones to a Habitat object

## Usage

``` r
add_habitat(hab, sf_data, weight = 1, priority = NULL, touches = TRUE, ...)
```

## Arguments

- hab:

  A Habitat object

- sf_data:

  An sf object containing geometries to add

- weight:

  A weight to apply on the habitat feature. Can then be used as
  resistance for dispersal functions for instance.

- priority:

  Stacking order of the added layer when the habitat is rasterized with
  [`habitat_raster`](https://qonfluens.github.io/spacemodR/reference/habitat_raster.md):
  where layers overlap, the one with the highest priority gives its
  value to the pixel. A single number for the whole layer, or one value
  per feature. By default (`NULL`) the layer is placed on top of
  everything already in `hab` (highest priority so far + 1), so that
  layers stack in the order they are added.

- touches:

  Logical, a single value for the whole layer or one value per feature.
  If `TRUE` (default), the features fill every pixel they touch when
  rasterized with
  [`habitat_raster`](https://qonfluens.github.io/spacemodR/reference/habitat_raster.md);
  if `FALSE`, a polygon only fills the pixels whose centre it covers,
  and a line only the pixels of a thin line. See Details.

- ...:

  Additional arguments

## Value

A Habitat object with new geometries added as habitat

## Details

`touches` controls which pixels a feature occupies, not how hard it is
to cross: the permeability of a structure is carried by its `weight` (or
by
[`add_nohabitat`](https://qonfluens.github.io/spacemodR/reference/add_nohabitat.md)
for an absolute barrier).

- `touches = TRUE` keeps thin structures (hedgerows, rivers, roads,
  whether drawn as lines or as narrow polygons) continuous: every pixel
  they cross is filled, so they cannot vanish between pixel centres nor
  be crossed diagonally between two pixels. The price is a footprint
  larger than the feature: it grows by up to one pixel in width.

- `touches = FALSE` keeps areas unbiased, which is preferable for a
  layer made of contiguous polygons covering the whole landscape (a land
  cover map): with `touches = TRUE`, each polygon would spread on the
  pixels of its neighbours and the border pixels would go to the feature
  on top of the stack.

A common set-up is therefore a land cover layer added with
`touches = FALSE` and a low `priority`, with the thin structures that
must stay continuous added on top with `touches = TRUE`.

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
square <- function(a, b) {
  st_sf(geometry = st_sfc(st_polygon(list(
    cbind(c(a, b, b, a, a), c(a, a, b, b, a))))))
}
hedge <- st_sf(geometry = st_sfc(st_linestring(cbind(c(0, 10), c(0, 10)))))

# the hedge is declared first but kept on top of the field with `priority`
hab <- habitat() |>
  add_habitat(hedge, weight = 1, priority = 10) |>
  add_habitat(square(2, 8), weight = 0.2, priority = 1)
hab
#> Habitat object
#> Number of features: 2 
#> CRS: NA 
#> Simple feature collection with 2 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 10 ymax: 10
#> CRS:           NA
#>   habitat weight priority touches                       geometry
#> 1    TRUE    1.0       10    TRUE        LINESTRING (0 0, 10 10)
#> 2    TRUE    0.2        1    TRUE POLYGON ((2 2, 8 2, 8 8, 2 ...
```
