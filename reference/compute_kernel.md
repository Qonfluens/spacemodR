# Create a 2D Gaussian motion kernel as a SpatRaster

Create a 2D Gaussian motion kernel as a SpatRaster

## Usage

``` r
compute_kernel(radius, GSD, size_std = 1.5)
```

## Arguments

- radius:

  Numeric, std of the distribution in meters

- GSD:

  Numeric, ground sampling distance in meters per pixel

- size_std:

  Numeric, how many std to extend kernel on each side

## Value

A matrix defining the kernel
