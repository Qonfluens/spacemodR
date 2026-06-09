# Add a Flux Rule

Adds a specific flux rule to a trophic link or target in a trophic
table.

## Usage

``` r
add_flux(tbl, from = NULL, to, value)
```

## Arguments

- tbl:

  A \`trophic_tbl\` object with an initialized \`flux\` column (output
  of \`flux()\`).

- from:

  Character. The source species. If \`NULL\` (default), the rule applies
  to all links pointing to \`to\`.

- to:

  Character. The target species.

- value:

  Numeric, formula, or function. The flux definition.

## Value

The updated \`trophic_tbl\`.
