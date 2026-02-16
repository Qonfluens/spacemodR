# Plot a trophic table

Creates a simple graphical representation of a trophic network using
ggplot2.

## Usage

``` r
# S3 method for class 'trophic_tbl'
plot(x, shift = TRUE, ...)
```

## Arguments

- x:

  A `trophic_tbl` object.

- shift:

  To shift x_axis between trophic level and avoid the potential
  overlapping of arrows.

- ...:

  Additional arguments (not used, for S3 consistency).

## Value

A ggplot object.

## Details

Nodes are positioned according to their trophic level:

- The y-axis represents trophic levels

- Nodes of the same level are placed on the same horizontal line

- The x-axis positions are assigned sequentially (0, 1, 2, ...)

Directed links are drawn from lower to higher trophic levels using
arrows.

## Examples

``` r
net <- trophic() |>
  add_link("a", "b") |>
  add_link("b", "c")

plot(net)

```
