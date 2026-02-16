# Create a RasterStack from a list of rasters and names

This function creates a \`SpatRaster\` stack from a list of rasters and
assigns unique names to each layer.

## Usage

``` r
raster_stack(raster_list, names = NULL)
```

## Arguments

- raster_list:

  A list of \`SpatRaster\` objects or file paths to raster files.

- names:

  A character vector of unique names for each raster layer in the stack.

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object with named layers.

## Details

The function checks that the length of \`raster_list\` matches the
length of \`names\`, and that all names are unique. If not, it stops
with an error.

## Examples

``` r
# Example with terra rasters
library(terra)
#> terra 1.8.93
r1 <- rast(nrows=10, ncols=10, vals=1:100)
r2 <- rast(nrows=10, ncols=10, vals=101:200)
raster_stack(list(r1, r2), c("layer1", "layer2"))
#> class       : SpatRaster 
#> size        : 10, 10, 2  (nrow, ncol, nlyr)
#> resolution  : 36, 18  (x, y)
#> extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (CRS84) (OGC:CRS84) 
#> source(s)   : memory
#> names       : layer1, layer2 
#> min values  :      1,    101 
#> max values  :    100,    200 
```
