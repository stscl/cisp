
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cisp <a href="https://ausgis.github.io/cisp/"><img src="man/figures/logo.png" align="right" height="139" alt="cisp website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-cyan.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN](https://www.r-pkg.org/badges/version/cisp)](https://CRAN.R-project.org/package=cisp)
[![R-universe](https://ausgis.r-universe.dev/badges/cisp?color=cyan)](https://ausgis.r-universe.dev/cisp)
<!-- badges: end -->

**A Correlation Indicator Based On Spatial Patterns**

## Installation

- Install development binary version from
  [R-universe](https://ausgis.r-universe.dev/cisp) with:

``` r
install.packages('cisp',
                 repos = c("https://ausgis.r-universe.dev",
                           "https://cloud.r-project.org"),
                 dep = TRUE)
```

- Install development source version from
  [GitHub](https://github.com/ausgis/cisp) with:

``` r
if (!requireNamespace("devtools")) {
    install.packages("devtools")
}
devtools::install_github("ausgis/cisp",
                         build_vignettes = TRUE,
                         dep = TRUE)
```
