# Compute trophic levels of nodes

Determines the trophic level of each node in an acyclic directed graph.

## Usage

``` r
compute_levels(edges)
```

## Arguments

- edges:

  A data.frame with columns `from` and `to`.

## Value

A named numeric vector giving the trophic level of each node.
