# Constructor for Flux Parameters

Initializes the trophic flux table for a spacemodel.

## Usage

``` r
flux(x, default = 1, normalize = TRUE)
```

## Arguments

- x:

  A \`spacemodel\` object or a \`trophic_tbl\`.

- default:

  The default function or coefficient for unspecified links (default is
  1, meaning full transfer: f(x) = x).

- normalize:

  Logical. Whether to normalize diet weights (default TRUE).

## Value

A \`trophic_tbl\` with an initialized \`flux\` column.
