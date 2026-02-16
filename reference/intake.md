# Constructor for Intake Parameters

Creates and configures the trophic flux table with a simplified syntax.

## Usage

``` r
intake(x, ..., default = NULL, normalize = TRUE)
```

## Arguments

- x:

  A \`spacemodel\` object or a \`trophic_tbl\`.

- ...:

  Flux definitions. - Key \`"Target"\` (e.g., \`"Fox" = 0.5\`): applies
  to all links pointing to Fox. - Key \`"Source -\> Target"\` (e.g.,
  \`"Soil -\> Worm" = 0.8\`): targets a specific link. Values can be
  numeric (linear coefficient), formulas, or functions.

- default:

  The default function for unspecified links (default is identity).

- normalize:

  Logical. Whether to normalize diet weights (default TRUE).

## Value

A \`trophic_tbl\` with a configured \`flux\` column.
