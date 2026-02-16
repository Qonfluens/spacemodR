# Normalize weights of a trophic table

Adds a new column `normalized_weight` to a `trophic_tbl` object so that,
for each target node (`to`), the sum of incoming weights equals 1.

## Usage

``` r
normalize_weights(tbl)
```

## Arguments

- tbl:

  A `trophic_tbl` object.

## Value

A `trophic_tbl` object with an additional column `normalized_weight`.

## Details

For every unique value in the `to` elements of the `link` column, the
function divides each corresponding weight by the total weight of all
links pointing to that same node.

Nodes with no incoming links are left unchanged.

## Examples

``` r
net <- trophic() |>
  add_link("a", "b", weight = 2) |>
  add_link("c", "b", weight = 3)

net_norm <- normalize_weights(net)
```
