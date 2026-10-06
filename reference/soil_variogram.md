# Empirical semi-variogram of soil concentrations

Computes the experimental (empirical) semi-variogram of a soil
concentration measured at sampling points: for every pair of points
separated by a distance falling in a given distance class (a \*lag\*),
half of the squared difference of their values is averaged.

When \`covariates\` are given, the variogram is computed on the
residuals of the linear regression of the values on the covariates (this
is the variogram needed for kriging with external drift, see
\[krige_soil()\]).

## Usage

``` r
soil_variogram(
  points,
  value,
  log10 = TRUE,
  covariates = NULL,
  cutoff = NULL,
  width = NULL,
  estimator = c("classical", "robust")
)
```

## Arguments

- points:

  An \`sf\` object of POINT geometries in a projected CRS (metres).

- value:

  Character. Name of the column holding the concentration.

- log10:

  Logical. Work on \`log10(value)\`? (default \`TRUE\`, recommended for
  skewed concentration data).

- covariates:

  Optional \`SpatRaster\` whose layers are used as linear drift (trend)
  covariates.

- cutoff:

  Numeric. Maximal distance (m) considered. Default: one third of the
  diagonal of the bounding box of the points.

- width:

  Numeric. Width (m) of the distance classes. Default \`cutoff / 15\`.

- estimator:

  Character. \`"classical"\` (Matheron) or \`"robust"\`
  (Cressie-Hawkins, less sensitive to outliers).

## Value

A \`data.frame\` of class \`soil_variogram\` with columns \`np\` (number
of pairs), \`dist\` (mean distance of the pairs) and \`gamma\`
(semi-variance).

## See also

\[fit_soil_variogram()\], \[krige_soil()\]

## Examples

``` r
data(soil_metaleurop)
v <- soil_variogram(soil_metaleurop, "cd")
v
#>       np      dist      gamma
#> 1   1260  174.5234 0.05594691
#> 2   3429  396.6164 0.06124015
#> 3   5228  648.8699 0.08059952
#> 4   6849  904.2774 0.09498779
#> 5   8100 1159.0879 0.12406657
#> 6   9379 1415.9839 0.13137464
#> 7  10421 1672.4693 0.14068862
#> 8  10974 1929.0429 0.14326919
#> 9  11582 2183.6174 0.14815167
#> 10 11920 2440.2196 0.15349987
#> 11 11603 2696.0282 0.15998917
#> 12 10996 2952.5402 0.16331292
#> 13 10286 3209.1854 0.16625862
#> 14  9234 3464.3529 0.17827067
#> 15  8310 3721.6273 0.17961735
```
