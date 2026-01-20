
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{spacemodR}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{spacemodR}` like so:

``` r
pak::pak("Qonfluens/spacemodR")
```

## Run

## DEV

``` r
library(devtools)
devtools::check()
```

### Before a New submission to CRAN:

[] Update DESCRIPTION Version number
[] Update NEWS


``` r
library(devtools)
devtools::submit_cran()
```

## About

You are reading the doc about version : 0.1.0

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-01-09 12:05:18 CET"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.2) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ───────────────────────────── spmdHabitat 0.0.0.9000 ────
#> Duration: 14.9s
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

``` r
covr::package_coverage()
#> spacemodR Coverage: 0.00%
#> R/app_config.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/run_app.R: 0.00%
```
# spacemodR
