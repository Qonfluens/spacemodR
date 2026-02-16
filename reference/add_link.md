# Add links to a trophic table

Adds one or several directed links to a trophic_tbl object.

## Usage

``` r
add_link(tbl, from, to, weight = 1)
```

## Arguments

- tbl:

  A `trophic_tbl` object.

- from:

  A single character string indicating the source node.

- to:

  A character vector indicating target nodes.

- weight:

  A numeric vector of weights associated with each link. If a single
  value is provided, it is recycled to match the length of `to`.

## Value

A validated `trophic_tbl` object with the new links added.

## Details

The function performs several checks:

- `from` must be a scalar character string

- `to` must be a character vector

- Links must be unique

- Self-loops (from == to) are forbidden

- The resulting graph must remain acyclic

## Examples

``` r
net <- trophic() |>
  add_link("a", "b", weight = 1)
```
