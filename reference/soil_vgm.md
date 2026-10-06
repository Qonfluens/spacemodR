# Build a variogram model by hand

Creates a \`soil_vgm\` object from known parameters, e.g. to re-use a
variogram fitted elsewhere (gstat, gstools, ...).

## Usage

``` r
soil_vgm(model, psill, range, nugget = 0, kappa = 0.5)
```

## Arguments

- model:

  Character. One of \`"Exp"\`, \`"Sph"\`, \`"Gau"\`, \`"Mat"\`.

- psill:

  Numeric. Partial sill.

- range:

  Numeric. Range parameter (m).

- nugget:

  Numeric. Nugget effect (default 0).

- kappa:

  Numeric. Matérn smoothness (default 0.5).

## Value

A \`soil_vgm\` object.

## Examples

``` r
soil_vgm("Exp", psill = 0.1, range = 500, nugget = 0.02)
#> Variogram model: Exp  
#>   nugget = 0.02 | partial sill = 0.1 | range = 500 m
```
