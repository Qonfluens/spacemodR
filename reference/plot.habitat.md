# Plot a Habitat object

Visually inspect a `habitat` object. Favorable habitat zones are colored
in green, and absolute barriers (non-habitat) are colored in red.

## Usage

``` r
# S3 method for class 'habitat'
plot(x, ...)
```

## Arguments

- x:

  A `habitat` object.

- ...:

  Additional arguments passed to `plot` (e.g., `main`, `border`).

## Examples

``` r
if (FALSE) { # \dontrun{
hab <- habitat() |>
  add_habitat(good_polygons, weight = 1) |>
  add_nohabitat(roads)
plot(hab)
} # }
```
