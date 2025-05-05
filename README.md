
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{PAS148y151}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{PAS148y151}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
PAS148y151::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-05-05 04:51:46 -04"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.2) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ────────────────────────────── PAS148y151 0.0.0.9000 ────
#> Duration: 29.5s
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

``` r
covr::package_coverage()
#> PAS148y151 Coverage: 0.00%
#> R/app_config.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/run_app.R: 0.00%
```
