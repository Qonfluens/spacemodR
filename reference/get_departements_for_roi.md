# Identify codes of departments intersecting a region of interest

This function takes a region of interest (ROI) as an \`sf\` or \`sfc\`
object and returns the codes of departments intersecting this region.

## Usage

``` r
get_departements_for_roi(roi)
```

## Arguments

- roi:

  A spatial object of type \`sf\` or \`sfc\` representing the region of
  interest. Use only the first geometry (polygon).

## Value

A character vector containing the codes of departments intersecting the
region of interest.

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
roi <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = 600000, ymin = 6600000, xmax = 650000, ymax = 6650000),
  crs = 2154)
)
departments <- get_departements_for_roi(roi)
```
