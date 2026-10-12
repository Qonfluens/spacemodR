# Fit a variogram model to an empirical soil variogram

Fits one or several theoretical variogram models to an empirical
semi-variogram (from \[soil_variogram()\]) by weighted least squares,
using the weights \\N_j / h_j^2\\ (number of pairs over squared
distance), which give more importance to the short distances that matter
most for kriging (same default as \`gstat::fit.variogram(fit.method =
7)\`).

Available models (the \`range\` parameter has the same meaning as in
gstat):

- \`"Exp"\`: exponential, \\\gamma(h) = c_0 + c (1 - e^{-h/a})\\;

- \`"Sph"\`: spherical, reaches the sill exactly at \\h = a\\;

- \`"Gau"\`: Gaussian, very smooth at the origin;

- \`"Mat"\`: Matérn with smoothness \`kappa\` (\`kappa = 0.5\` is
  \`"Exp"\`).

## Usage

``` r
fit_soil_variogram(variogram, model = "Exp", kappa = 0.5)
```

## Arguments

- variogram:

  A \`soil_variogram\` object.

- model:

  Character vector of model(s) to fit. When several are given, all are
  fitted and the one with the lowest weighted sum of squared errors is
  returned (the full comparison table is kept in \`\$comparison\`).

- kappa:

  Numeric. Smoothness of the Matérn model (ignored otherwise).

## Value

An object of class \`soil_vgm\`: a list with \`model\`, \`nugget\`,
\`psill\`, \`range\`, \`kappa\`, \`sse\` (weighted sum of squared
errors), \`empirical\` (the input variogram) and \`comparison\` (a table
of all fitted models).

## See also

\[soil_variogram()\], \[krige_soil()\]

## Examples

``` r
data(soil_metaleurop)
v <- soil_variogram(soil_metaleurop, "cd")
m <- fit_soil_variogram(v, model = c("Exp", "Sph", "Gau"))
m
#> Variogram model: Gau  
#>   nugget = 0.05312 | partial sill = 0.1073 | range = 1235 m
#>   weighted SSE = 1.386e-05
#>   fitted on values (constant mean) 
m$comparison
#>   model     nugget     psill    range          sse
#> 1   Gau 0.05311783 0.1072776 1235.369 1.386473e-05
#> 2   Sph 0.04211791 0.1231916 2887.670 2.587145e-05
#> 3   Exp 0.03899008 0.1726382 2113.586 3.157755e-05
```
