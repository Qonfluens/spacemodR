# Changelog

## spacemodR 0.2.0

### Breaking Changes

- **Refactored trophic fluxes interface:** The `intake()` function has
  been completely redesigned and renamed to
  [`flux()`](https://qonfluens.github.io/spacemodR/reference/flux.md) to
  better reflect the mathematical and ecological concept of transfer
  rates.
- **New pipe-friendly syntax:** The definition of transfer parameters no
  longer uses multiple named arguments. Users must now initialize the
  table with
  [`flux()`](https://qonfluens.github.io/spacemodR/reference/flux.md)
  and chain specific rules using the new
  [`add_flux()`](https://qonfluens.github.io/spacemodR/reference/add_flux.md)
  function.
- **Namespace conflict resolution:** The internal utility function
  previously named
  [`flux()`](https://qonfluens.github.io/spacemodR/reference/flux.md)
  (used within the
  [`transfer()`](https://qonfluens.github.io/spacemodR/reference/transfer.md)
  function) has been renamed to avoid conflicts with the new main API.

### New Features & Improvements

- **New
  [`add_flux()`](https://qonfluens.github.io/spacemodR/reference/add_flux.md)
  function:** Allows for adding transfer rules in a readable, sequential
  manner (e.g.,
  `add_flux(from = "soil", to = "plant", value = ~ 10^x / 32)`).
- **Global target rules:** In
  [`add_flux()`](https://qonfluens.github.io/spacemodR/reference/add_flux.md),
  if the source compartment (`from`) is not specified, the rule
  (`value`) is automatically applied to all links pointing to the target
  compartment (`to`).
- **Safe default values:** The
  [`flux()`](https://qonfluens.github.io/spacemodR/reference/flux.md)
  function now features an explicit `default` parameter. It is highly
  recommended to use `1` (full transfer) or `0` (no transfer) instead of
  `NA` to prevent the propagation of missing values during subsequent
  model calculations.

## spacemodR 0.1.3

CRAN release: 2026-02-20

- **Bug Fixes:**
  - Fixed an `rlang_error` (replacement has length zero) in `spacemodel`
    raster layer sub-assignment within the Tutorial vignette.
  - Removed non-ASCII characters (smart quotes) from the
    `DBFunc_BirdFuncDat` dataset to pass CRAN checks.
  - Resolved R CMD Check notes regarding
    `no visible binding for global variable` (`weight_global`,
    `weight_foraging`, `resistance`).
- **Documentation & Data:**
  - Added complete `roxygen2` documentation for internal datasets:
    `FmrBT`, `DBFunc_MamFuncDat`, and `DBFunc_BirdFuncDat`.
  - Translated and enhanced the `Food_Intake` vignette into professional
    scientific English.
  - Updated the `pkgdown` website structure to use a clean 3-tier
    navigation system (Quick Start, Core Guides, Example Zoo).

## spacemodR 0.1.2

- **New Features:**
  - Added the `Food_Intake` module and dataset integration (FmrBT,
    EltonTraits) to compute Daily Food Intake (DFI).
  - Included the Eco-SSL (Ecological Soil Screening Levels) risk index
    assessment example.
- **Enhancements:**
  - Improved the `Omniscape_Connectivity` tutorial with real-world
    spatial data for the Wood Mouse (*Apodemus sylvaticus*).

## spacemodR 0.1.0

- **Initial Release:**
  - Introduced the core `spacemodel` class for merging raster stacks and
    trophic networks.
  - Added `transfer` and `dispersal` modeling functions.
  - Integrated external Julia Omniscape connectivity algorithms.
