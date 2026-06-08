# spacemodR 0.1.3

* **Bug Fixes:**
  * Fixed an `rlang_error` (replacement has length zero) in `spacemodel` raster layer sub-assignment within the Tutorial vignette.
  * Removed non-ASCII characters (smart quotes) from the `DBFunc_BirdFuncDat` dataset to pass CRAN checks.
  * Resolved R CMD Check notes regarding `no visible binding for global variable` (`weight_global`, `weight_foraging`, `resistance`).
* **Documentation & Data:**
  * Added complete `roxygen2` documentation for internal datasets: `FmrBT`, `DBFunc_MamFuncDat`, and `DBFunc_BirdFuncDat`.
  * Translated and enhanced the `Food_Intake` vignette into professional scientific English.
  * Updated the `pkgdown` website structure to use a clean 3-tier navigation system (Quick Start, Core Guides, Example Zoo).

# spacemodR 0.1.2

* **New Features:**
  * Added the `Food_Intake` module and dataset integration (FmrBT, EltonTraits) to compute Daily Food Intake (DFI).
  * Included the Eco-SSL (Ecological Soil Screening Levels) risk index assessment example.
* **Enhancements:**
  * Improved the `Omniscape_Connectivity` tutorial with real-world spatial data for the Wood Mouse (*Apodemus sylvaticus*).

# spacemodR 0.1.0

* **Initial Release:**
  * Introduced the core `spacemodel` class for merging raster stacks and trophic networks.
  * Added `transfer` and `dispersal` modeling functions.
  * Integrated external Julia Omniscape connectivity algorithms.
