# cisp

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
