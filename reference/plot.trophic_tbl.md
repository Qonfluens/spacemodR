# Plot a trophic table

Creates a simple graphical representation of a trophic network using
ggplot2.

## Usage

``` r
# S3 method for class 'trophic_tbl'
plot(x, shift = TRUE, colors = NULL, use_weight = FALSE, ...)
```

## Arguments

- x:

  A `trophic_tbl` object.

- shift:

  To shift x_axis between trophic level and avoid the potential
  overlapping of arrows.

- colors:

  A named character vector of colors (hexadecimal or standard R color
  names) where the names match the node names in the trophic table. If
  NULL, grey colors are used.

- use_weight:

  Logical. If TRUE, width of arrow is proportional to link weight
  (normalized to 1 per target node). Default is FALSE.

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

# Plot with default colors
plot(net)


# Plot with custom colors
my_pal <- c("a" = "brown", "b" = "green", "c" = "blue")
plot(net, colors = my_pal)

```
