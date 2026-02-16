# Create a Habitat object

Create a spatial Habitat object based on an optional `sf` data.frame. If
no geometry is provided, creates an empty Habitat object. The object has
columns:

- `habitat`: logical, TRUE/FALSE

- `weight`: numeric

- `geometry`: sfc geometry

## Usage

``` r
habitat(geometry = NULL, habitat = NULL, weight = NULL)
```

## Arguments

- geometry:

  An object of class `sf` or `sfc`. Optional, default is empty.

- habitat:

  Logical vector indicating habitat presence. Default is logical(0).

- weight:

  Numeric vector of weights. Default is numeric(0).

## Value

An object of class `habitat`, inheriting from `sf` and `data.frame`.

## Examples

``` r
library(sf)

# Empty habitat
h <- habitat()
h
#> Habitat object
#> Number of features: 0 
#> CRS: NA 
#> Simple feature collection with 0 features and 2 fields
#> Bounding box:  xmin: NA ymin: NA xmax: NA ymax: NA
#> CRS:           NA
#> [1] habitat  weight   geometry
#> <0 rows> (or 0-length row.names)

# Habitat with geometries
geom <- st_sfc(
  st_point(c(0, 0)),
  st_point(c(1, 1)),
  crs = 4326
)

hab <- habitat(
  geometry = geom,
  habitat = c(TRUE, FALSE),
  weight = c(0.8, 0)
)
hab
#> Habitat object
#> Number of features: 2 
#> CRS: EPSG:4326 
#> Simple feature collection with 2 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> Geodetic CRS:  WGS 84
#>   habitat weight    geometry
#> 1    TRUE    0.8 POINT (0 0)
#> 2   FALSE    0.0 POINT (1 1)
```
