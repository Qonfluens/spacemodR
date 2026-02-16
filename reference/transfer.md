# Transfer (food, contaminant) across trophic levels

Computes the transfer through a trophic network from lower to higher
trophic levels using spatial spreading and intake functions.

## Usage

``` r
transfer(
  spacemodel,
  kernels,
  intakes = NULL,
  exposure_weighting = "local",
  verbose = FALSE
)
```

## Arguments

- spacemodel:

  A named list of spatial layers (e.g. `SpatRaster` objects). Must
  contain an attribute `trophic_tbl` of class `trophic_tbl`.

- kernels:

  A list of kernel parameters for each layer.

- intakes:

  A `trophic_tbl` object (or compatible table) containing normalized
  weights and flux functions for each trophic link.

- exposure_weighting:

  Character. Defines how the realized exposure is calculated based on
  the predator's presence. Options are:

  - `"local"` (Default): Weight exposure by the local habitat value
    (\`predator_habitat\`). Assumes the predator's intake is strictly
    proportional to the habitat quality/density of the pixel where it
    resides.

  - `"diffuse"`: Weight exposure by a dispersed habitat kernel.
    Represents a "neighborhood" effect where the predator's presence is
    smoothed over its home range. Useful to avoid edge effects where a
    predator on a poor pixel surrounded by good habitat would otherwise
    have 0 exposure.

  - `"potential"`: No weighting. Returns the pure environmental offer
    (potential exposure) regardless of the predator's density. Useful
    for identifying risk hotspots.

- verbose:

  Logical. If `TRUE`, prints progress information.

## Value

A named \`spacemodel\` object as a list of spatial layers representing
values after transfer.

## Details

The function processes layers in ascending trophic order, as defined by
the `level` attribute of the trophic table.

For each layer:

1.  Resources (lower neighbors) are identified.

2.  Concentration from each resource is spatially spread using
    `spread()`.

3.  Intake is computed using
    [`intake()`](https://qonfluens.github.io/spacemodr/reference/intake.md).

4.  Contributions from all resources are summed.

The function assumes that intake weights are already normalized so that,
for each consumer, the sum of contributions from all resources equals 1.
