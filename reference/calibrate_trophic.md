# Calibrate trophic transfer model

Fits a Bayesian trophic transfer model using Stan. This model calibrates
relationships across multiple trophic compartments.

## Usage

``` r
calibrate_trophic(data, ...)
```

## Arguments

- data:

  A named list containing the data formatted for the Stan model.
  Required elements are: `Np`, `Nw`, `Nh`, `Ni`, `M`, `yp`, `xp`, `yw`,
  `xw`, `yh`, `xh`, `yi`, `xi`, and `x_sim`.

- ...:

  Additional arguments passed to
  [`sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  (e.g., `iter`, `chains`, `cores`, `control`).

## Value

A `stanfit` object containing the posterior samples.
