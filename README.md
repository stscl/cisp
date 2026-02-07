
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cisp <a href="https://stscl.github.io/cisp/"><img src="man/figures/logo.png" align="right" height="139" alt="cisp website" /></a>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/cisp)](https://CRAN.R-project.org/package=cisp)
[![CRAN
Release](https://www.r-pkg.org/badges/last-release/cisp)](https://CRAN.R-project.org/package=cisp)
[![CRAN
Checks](https://badges.cranchecks.info/worst/cisp.svg)](https://cran.r-project.org/web/checks/check_results_cisp.html)
[![DownloadsAll](https://badgen.net/cran/dt/cisp?color=orange)](https://CRAN.R-project.org/package=cisp)
[![DownloadsMonth](https://cranlogs.r-pkg.org/badges/cisp)](https://CRAN.R-project.org/package=cisp)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-516a54.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-universe](https://stscl.r-universe.dev/badges/cisp?color=cyan)](https://stscl.r-universe.dev/cisp)
[![R-CMD-check](https://github.com/stscl/cisp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stscl/cisp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**A Correlation Indicator Based on Spatial Patterns**

## Installation

- Install from [CRAN](https://CRAN.R-project.org/package=cisp) with:

``` r
install.packages("cisp", dep = TRUE)
```

- Install development binary version from
  [R-universe](https://stscl.r-universe.dev/cisp) with:

``` r
install.packages('cisp',
                 repos = c("https://stscl.r-universe.dev",
                           "https://cloud.r-project.org"),
                 dep = TRUE)
```

- Install development source version from
  [GitHub](https://github.com/stscl/cisp) with:

``` r
if (!requireNamespace("devtools")) {
    install.packages("devtools")
}
devtools::install_github("stscl/cisp",
                         build_vignettes = TRUE,
                         dep = TRUE)
```
