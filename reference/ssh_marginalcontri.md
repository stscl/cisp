# spatial association marginal contributions derived from spatial stratified heterogeneity

spatial association marginal contributions derived from spatial
stratified heterogeneity

## Usage

``` r
ssh_marginalcontri(formula, data, overlay = "and", cores = 1)
```

## Arguments

- formula:

  A formula of ISP model.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- overlay:

  (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
  Default is `and`.

- cores:

  (optional) Positive integer (default is 1). When cores are greater
  than 1, use parallel computing.

## Value

A list.

- `pd`:

  power of determinants

- `spd`:

  shap power of determinants

- `determination`:

  determination of the optimal interaction of variables

## Examples

``` r
NTDs1 = sf::st_as_sf(gdverse::NTDs, coords = c('X','Y'))
g = ssh_marginalcontri(incidence ~ ., data = NTDs1, cores = 1)
g
#> ***   SSH Marginal Contributions    
#> 
#> | variable  |     spd     |
#> |:---------:|:-----------:|
#> | watershed | 0.08591452  |
#> | elevation | 0.04576116  |
#> | soiltype  | -0.01120835 |
plot(g)

```
