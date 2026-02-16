# Test if a directed graph is cyclic

Implements Kahn's algorithm to detect cycles in a directed graph.

## Usage

``` r
is_cyclic(df)
```

## Arguments

- df:

  A data.frame with columns `from` and `to`.

## Value

Logical. TRUE if the graph contains at least one cycle.

## Examples

``` r
df <- data.frame(from=c("A","B"), to=c("B","A"))
is_cyclic(df)
#> [1] TRUE
```
