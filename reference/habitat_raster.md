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
be assigned `NA`, overriding any underlying weight. If overlapping
polygons exist, the order of features in the `habitat` object determines
the final value (last on top).

## Examples

``` r
if (FALSE) { # \dontrun{
library(terra)
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
} # }
```
