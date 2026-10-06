# Cross-validate a soil interpolation method

Estimates the prediction error of an interpolation method by k-fold
cross-validation: the points are split in \`nfold\` groups; each group
is in turn removed, the model is re-fitted (including the variogram) on
the remaining points and used to predict the removed ones.

With \`block_size\`, folds are made of square spatial blocks instead of
random points (\*spatial cross-validation\*). This is more demanding: it
mimics the prediction of areas located far from any sample, where
interpolators that simply copy neighbours are penalised.

## Usage

``` r
cv_soil(
  points,
  value,
  method = c("kriging", "kernel"),
  covariates = NULL,
  model = "Exp",
  log10 = TRUE,
  nfold = 10,
  block_size = NULL,
  seed = 1,
  radius = 200,
  size_std = 3,
  kappa = 0.5,
  cutoff = NULL,
  width = NULL
)
```

## Arguments

- points:

  An \`sf\` object of POINT geometries in a projected CRS (metres).

- value:

  Character. Name of the concentration column (e.g. \`"cd"\`).

- method:

  Character. \`"kriging"\` (\[krige_soil()\]) or \`"kernel"\`
  (\[kernel_smooth_soil()\]).

- covariates:

  Optional \`SpatRaster\` of drift covariates (must cover the points and
  the output grid, same CRS).

- model:

  Character vector of candidate variogram models (see
  \[fit_soil_variogram()\]); the best fitting one is used. Ignored when
  \`vgm\` is given.

- log10:

  Logical. Krige \`log10(value)\` (default \`TRUE\`).

- nfold:

  Integer. Number of folds (default 10).

- block_size:

  Numeric. Side (m) of the spatial blocks; \`NULL\` (default) for random
  folds.

- seed:

  Integer. Random seed for the fold assignment.

- radius, size_std:

  Kernel standard deviation (m) and window half-size (in number of
  \`radius\`), for \`method = "kernel"\` (see \[kernel_smooth_soil()\]).
  For speed, the cross-validation evaluates the Gaussian kernel exactly
  at the left-out points instead of on a raster.

- kappa:

  Matérn smoothness, passed to \[fit_soil_variogram()\].

- cutoff, width:

  Passed to \[soil_variogram()\].

## Value

A \`data.frame\` of class \`soil_cv\` with the observed value (\`obs\`),
the prediction (\`pred\`) and the \`fold\` of each point (on the log10
scale if \`log10 = TRUE\`). Use \[summary()\] on it to get RMSE, MAE,
bias and R2.

## See also

\[krige_soil()\], \[kernel_smooth_soil()\]

## Examples

``` r
# \donttest{
data(soil_metaleurop)
cv <- cv_soil(soil_metaleurop, "cd", model = "Exp")
summary(cv)
#>        RMSE       MAE         bias       R2   n
#> 1 0.2506923 0.1622391 0.0001610785 0.602517 587
# }
```
