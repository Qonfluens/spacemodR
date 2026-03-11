# Calibrate direct transfer model

Fits a Bayesian direct transfer model using Stan. The model handles
continuous and categorical explanatory variables to predict a response
variable.

## Usage

``` r
calibrate_direct(data, ...)
```

## Arguments

- data:

  A named list containing the data formatted for the Stan model.
  Required elements are: `N`, `K`, `M`, `y`, `x`, `x_cat`, and `x_sim`.

- ...:

  Additional arguments passed to
  [`sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  (e.g., `iter`, `chains`, `cores`, `control`).

## Value

A `stanfit` object containing the posterior samples.
