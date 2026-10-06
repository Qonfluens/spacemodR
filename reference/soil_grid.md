# Build a regular raster grid around sampling points

Creates an empty \`SpatRaster\` covering the bounding box of \`points\`,
enlarged by \`margin\` on each side, with square cells of size \`res\`.
The extent is aligned on multiples of \`res\`.

## Usage

``` r
soil_grid(points, res = 25, margin = 1000)
```

## Arguments

- points:

  An \`sf\` / \`sfc\` object (or any object accepted by
  \[sf::st_bbox()\]) in a projected CRS (metres).

- res:

  Numeric. Cell size in metres (ground sampling distance).

- margin:

  Numeric. Margin in metres added around the points.

## Value

An empty \`SpatRaster\`.

## Examples

``` r
data(soil_metaleurop)
soil_grid(soil_metaleurop, res = 25, margin = 1000)
#> class       : SpatRaster
#> size        : 415, 401, 1  (nrow, ncol, nlyr)
#> resolution  : 25, 25  (x, y)
#> extent      : 697600, 707625, 7032150, 7042525  (xmin, xmax, ymin, ymax)
#> coord. ref. : RGF93 v1 / Lambert-93 (EPSG:2154)
```
