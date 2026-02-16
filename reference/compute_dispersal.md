# Compute dispersal or spread map (Generic Engine)

Low-level function to apply spatial processing (Convolution or External
algorithms) to a raster. It handles NA masking and method dispatch to
Julia if necessary.

## Usage

``` r
compute_dispersal(x, method = "convolution", options = list(), mask = NULL)
```

## Arguments

- x:

  SpatRaster. Source layer to disperse.

- method:

  Character. "convolution" or "omniscape" (placeholder).

- options:

  List. Parameters (e.g., \`kernel\` for convolution).

- mask:

  SpatRaster (optional). A mask to apply after dispersion (e.g.,
  maintain original NA structure).

## Value

A
[`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object containing the dispersed values.
