# Disperse a species or variable over the landscape

Applies a dispersal mechanism to a specific layer of the \`spacemodel\`
object. This function acts as a wrapper around
[`compute_dispersal`](https://qonfluens.github.io/spacemodr/reference/compute_dispersal.md)
to handle the \`spacemodel\` class structure.

## Usage

``` r
dispersal(
  spacemodel,
  layer = 1,
  method = "convolution",
  method_option = list()
)
```

## Arguments

- spacemodel:

  A `spacemodel` object.

- layer:

  Character or Integer. The name or index of the layer to disperse
  (e.g., "Fox", 1).

- method:

  Character. The dispersal method to use. Options are:

  - `"convolution"` (default): Uses a moving window (kernel).

  - `"omniscape"`: Uses Circuit Theory (via Julia and Omniscape.jl).

- method_option:

  A list of parameters specific to the chosen method:

  - For `"convolution"`: must contain `kernel` (a matrix).

  - For `"omniscape"`: must contain `resistance` (SpatRaster) and
    `radius` (numeric).

## Value

The `spacemodel` object with the specified layer updated with dispersed
values.

## See also

[`compute_dispersal`](https://qonfluens.github.io/spacemodr/reference/compute_dispersal.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Convolution example
my_kernel <- matrix(1, nrow=3, ncol=3)
sm_updated <- dispersal(sm, layer = "Predator", method = "convolution",
                        method_option = list(kernel = my_kernel))

# 2. Omniscape example (requires Julia)
sm_updated <- dispersal(sm, layer = "Predator", method = "omniscape",
                        method_option = list(resistance = res_map, radius = 10))
} # }
```
