# Summarise a cross-validation

Summarise a cross-validation

## Usage

``` r
# S3 method for class 'soil_cv'
summary(object, ...)
```

## Arguments

- object:

  A \`soil_cv\` object from \[cv_soil()\].

- ...:

  Not used.

## Value

A one-row \`data.frame\` with \`RMSE\` (root mean squared error),
\`MAE\` (mean absolute error), \`bias\` (mean error), \`R2\` (proportion
of variance explained, \\1 - SSE/SST\\) and \`n\` (number of predicted
points).
