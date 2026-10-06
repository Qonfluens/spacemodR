# spacemodR

[![Documentation](https://img.shields.io/badge/documentation-online-blue.svg)](https://qonfluens.github.io/spacemodR/)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Spatially Explicit Modeling of Habitats, Trophic Webs, Dispersal,
Exposure, and Ecological Risk**

`spacemodR` is an R package designed for spatial ecological risk
assessment (ERA). It integrates habitat mapping, trophic networks (food
webs), animal dispersal, and contaminant exposure to build complex
“spacemodels.” By linking geographic data (rasters, vectors) with
biological and ecological interactions, it allows researchers and risk
assessors to map the flow of elements (such as food, energy, or
contaminants) across ecosystems.

------------------------------------------------------------------------

## 🌱 The SPACEMOD Project

This package is the core computational engine of the **SPACEMOD**
project, funded by the French ecological transition agency (**ADEME**)
under the *IMPACTS 2023* call for projects.

With the discontinuation of historical ERA software (like TerraSys or
BERISP), there was a critical need for a modern, generic tool dedicated
to Ecological Risk Assessment for contaminated sites and soils (SSP).
The SPACEMOD project aims to fill this gap by developing a spatially
explicit tool to model multi-exposures and chemical mixture impacts in
trophic webs.

**Project Partners:** \* **UMR Chrono-environnement (CNRS / Université
de Franche-Comté)** - *Project Coordinator (Pr. Renaud Scheifler)* \*
**Qonfluens SAS** \* **UK Centre for Ecology & Hydrology (UKCEH)**

You can find more information about the project on the [ADEME research
portal](https://recherche.ademe.fr/spacemod) or the
[Chrono-environnement
website](https://chrono-environnement.univ-fcomte.fr/project/spacemod/).

------------------------------------------------------------------------

## 🚀 What can you do with `spacemodR`?

The package provides a complete workflow to move from raw landscape data
to risk indices: 1. **Habitat Construction:** Convert vector shapefiles
(e.g., OCS-GE) into high-resolution habitat raster grids based on
species-specific preferences and weights. 2. **Trophic Web Modeling:**
Define Directed Acyclic Graphs (DAGs) representing predation and
herbivory flows across the ecosystem. 3. **Dispersal & Connectivity:**
Simulate animal movement and flow using advanced dispersal kernels or
circuit theory (via integration with Julia’s **Omniscape** algorithm).
4. **Exposure & Transfer:** Calculate Daily Food Intake (DFI) and model
the bioaccumulation and transfer of contaminants (e.g., Cadmium, Lead,
Zinc) through the food web. 5. **Risk Mapping:** Generate spatially
explicit risk maps (e.g., Eco-SSL indices) to identify critical
hot-spots of ecotoxicological concern.

------------------------------------------------------------------------

## 📚 Integrated Databases

To facilitate parameterization, `spacemodR` workflows rely on
standardized ecological and physiological databases: \* **FmrBT:**
Database of Field Metabolic Rates (FMR), body mass, and ambient
temperature for over 700 species (De Castro et al., 2025). \*
**EltonTraits 1.0 / MammalBase:** Functional traits and precise dietary
partitioning (percentages of invertebrates, seeds, vertebrates, etc.)
for birds and mammals. \* **GARD Initiative:** Global Assessment of
Reptile Distributions and traits. \* **TRY Plant Trait Database:** Lower
Calorific Value (LCV) data for plant energy density. \* **Add-my-pet:**
Dynamic energy budget models and parameters (e.g., $`\xi_{WE}`$).

------------------------------------------------------------------------

## 📦 Installation

`spacemodR` is not on CRAN yet. Install it from GitHub using one of the
methods below.

**System requirements:** `sf` and `terra` need GDAL, PROJ and GEOS
(already bundled in the CRAN binaries for Windows and macOS). The
package includes compiled Stan models, so building from source needs a
C++ toolchain ([Rtools](https://cran.r-project.org/bin/windows/Rtools/)
on Windows, Xcode command line tools on macOS) and takes a few minutes.

### 1. From GitHub with `remotes` (recommended)

``` r

# install.packages("remotes")
remotes::install_github("Qonfluens/spacemodR")

# or a specific release
remotes::install_github("Qonfluens/spacemodR@v0.3.0")
```

### 2. From a GitHub release (pre-built packages)

Each [release](https://github.com/Qonfluens/spacemodR/releases)
provides:

- `spacemodR_<version>.tar.gz`: source package (all platforms, needs a
  compiler),
- `spacemodR_<version>.zip`: Windows binary (no compiler needed),
- `spacemodR_<version>.tgz`: macOS binary (no compiler needed).

Binaries are built with the current R release; use the source package
with older R versions. Download the file for your platform, then:

``` r

# dependencies first (binaries do not install them)
install.packages(c("dplyr", "ggplot2", "httr", "patchwork", "Rcpp", "rlang",
                   "rstan", "rstantools", "sf", "terra"))

# source package
install.packages("path/to/spacemodR_0.3.0.tar.gz", repos = NULL, type = "source")
# Windows binary
install.packages("path/to/spacemodR_0.3.0.zip", repos = NULL, type = "win.binary")
# macOS binary
install.packages("path/to/spacemodR_0.3.0.tgz", repos = NULL, type = "binary")
```

### 3. Via Docker

A Dockerfile is provided to ensure a fully reproducible environment with
all system dependencies (including Julia for Omniscape) pre-configured.

``` bash
# Build the image locally
docker build -t spacemodr .

# Run the container (with RStudio Server accessible on port 8787)
docker run --rm -ti \
  -p 8787:8787 \
  -e PASSWORD=yourpassword \
  spacemodr
```

### Releasing a new version (For Maintainers)

1.  Bump `Version:` in `DESCRIPTION` and add a section to `NEWS.md`.

2.  Commit and push to `main`, then tag the commit and push the tag:

    ``` bash
    git tag -a v0.3.0 -m "Release v0.3.0"
    git push origin v0.3.0
    ```

3.  The `.github/workflows/release.yaml` workflow builds the source
    package and the Windows/macOS binaries and attaches them to a new
    GitHub release. The tag must match the `DESCRIPTION` version,
    otherwise the workflow stops.

### Note on Git LFS (For Contributors)

To track large data files (like rasters or fitted models) in the
repository, we use `git lfs`. Before using `git add` on a large file,
ensure it is tracked:

``` bash
git lfs track "raw_data/new_fit.rda"
```

------------------------------------------------------------------------

## 📄 License

`spacemodR` is released under the **MIT License**. This is a permissive
free software license that allows you to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the software,
provided that the original copyright notice and permission notice are
included in all copies or substantial portions of the software.

See the [LICENSE](https://qonfluens.github.io/spacemodR/LICENSE.md) file
for full details.
