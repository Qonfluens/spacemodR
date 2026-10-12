# Rasterize a Habitat Object

Converts a spatial `habitat` object into a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).
The resulting raster stores the `weight` values of the habitat
geometries. Areas explicitly defined as non-habitat (`habitat = FALSE`)
are assigned `NA`.

## Usage

``` r
habitat_raster(ground_raster, habitat)
```

## Arguments

- ground_raster:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object used as a reference template. It provides the extent,
  resolution, and coordinate reference system (CRS).

- habitat:

  A `habitat` object (inheriting from `sf`).

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
containing habitat weights, with `NA` values in explicit non-habitat
zones.

## Details

The function maps the continuous or categorical weights of the habitat
geometries onto a raster grid.

**Background handling:** By default, pixels not covered by any geometry
are assigned a weight of `0` and are considered passable (habitat =
`TRUE`).

**Exclusion zones:** Geometries added via
[`add_nohabitat()`](https://qonfluens.github.io/spacemodR/reference/add_nohabitat.md)
act as absolute barriers. Any pixel intersecting these geometries will
be assigned `NA`, overriding any underlying weight.

**Stacking order:** Where several features overlap, the pixel takes the
value of the feature with the highest `priority` (see
[`add_habitat`](https://qonfluens.github.io/spacemodR/reference/add_habitat.md));
features are stacked from the lowest to the highest priority and the top
one is read. With equal priorities, the order of the features in the
`habitat` object decides (last on top). As
[`add_habitat()`](https://qonfluens.github.io/spacemodR/reference/add_habitat.md)
and
[`add_nohabitat()`](https://qonfluens.github.io/spacemodR/reference/add_nohabitat.md)
give by default an increasing priority to each new layer, layers stack
in the order they were added unless `priority` is set explicitly. The
same rule applies to exclusion zones: a non-habitat feature excludes a
pixel only if no feature of higher priority covers it.

**Pixels filled by a feature:** The `touches` value of each feature (see
[`add_habitat`](https://qonfluens.github.io/spacemodR/reference/add_habitat.md))
sets which pixels it occupies. With `touches = TRUE` (the default of
[`add_habitat()`](https://qonfluens.github.io/spacemodR/reference/add_habitat.md)
and
[`add_nohabitat()`](https://qonfluens.github.io/spacemodR/reference/add_nohabitat.md))
a line or a polygon fills every pixel it touches. A thin structure such
as a hedgerow, a river or a road is then a chain of pixels sharing an
edge: a corridor stays connected for 4-neighbour movements and a barrier
cannot be crossed diagonally. With `touches = FALSE`, a polygon fills
only the pixels whose centre it covers and a line only the pixels of a
thin line, which keeps areas unbiased but can break thin structures.
Combine a high `priority` with `touches = TRUE` to keep a thin structure
continuous above the layers it runs through. Points, lines and polygons
can be mixed in the same `habitat` object.

Note that a raster holds one value per pixel: a structure narrower than
a pixel is either widened to a full pixel or dropped, and two structures
sharing a pixel cannot both be kept. When this matters, the only real
solution is a finer `ground_raster`.

## Examples

``` r
library(terra)
#> terra 1.9.50
library(sf)

# Create a dummy reference raster
ref_grid <- rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10)

# Create habitat zones
good_hab <- st_sf(geometry = st_sfc(st_polygon(list(cbind(c(1, 9, 9, 1, 1), c(1, 1, 9, 9, 1))))))
bad_hab  <- st_sf(geometry = st_sfc(st_polygon(list(cbind(c(4, 6, 6, 4, 4), c(4, 4, 6, 6, 4))))))

# Build habitat object
my_habitat <- habitat() |>
  add_habitat(good_hab, weight = 0.8) |>
  add_nohabitat(bad_hab)

# Rasterize
hab_rast <- habitat_raster(ref_grid, my_habitat)
plot(hab_rast)


# A hedgerow kept continuous across the exclusion zone, whatever the order
# in which the layers are added
hedge <- st_sf(geometry = st_sfc(st_linestring(cbind(c(0, 10), c(0.2, 9.7)))))
my_habitat <- habitat() |>
  add_habitat(hedge, weight = 1, priority = 10) |>
  add_habitat(good_hab, weight = 0.8, priority = 1, touches = FALSE) |>
  add_nohabitat(bad_hab, priority = 2)
plot(habitat_raster(ref_grid, my_habitat))
```
