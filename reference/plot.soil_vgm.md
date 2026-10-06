# Plot an empirical variogram and its fitted model(s)

Plot an empirical variogram and its fitted model(s)

## Usage

``` r
# S3 method for class 'soil_vgm'
plot(x, all_models = TRUE, ...)
```

## Arguments

- x:

  A \`soil_vgm\` object (output of \[fit_soil_variogram()\]).

- all_models:

  Logical. Draw every model of \`x\$comparison\`? (default \`TRUE\`)

- ...:

  Not used.

## Value

A \`ggplot\` object.
