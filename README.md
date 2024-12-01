
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The power package

<!-- badges: start -->

![GitHub
version](https://img.shields.io/badge/Version-0.0.0.9000-success.svg?style=flat&logo=github)
[![CRAN
status](http://www.r-pkg.org/badges/version/power)](https://cran.r-project.org/package=power)
[![R-CMD-check](https://github.com/stufield/power/workflows/R-CMD-check/badge.svg)](https://github.com/stufield/power/actions)
[![](https://cranlogs.r-pkg.org/badges/grand-total/power)](https://cran.r-project.org/package=power)
[![Codecov test
coverage](https://codecov.io/gh/stufield/power/branch/main/graph/badge.svg)](https://app.codecov.io/gh/stufield/power?branch=main)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://choosealicense.com/licenses/mit/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `power` package contains some simple functions to empirically
simulate and estimate statistical power under various statistical test
conditions. In general, simulations are performed with *known* effect
sizes or differences, and the proportion of detected significant
*p*-values represents the empirical power, i.e. $1 - \beta$ or
`1 - TypeII` error.

The goal is typically get an idea of the required sample size given an
experimental design, statistical test, effect size, and desired power.

------------------------------------------------------------------------

## Installation

The `power` package is not currently on
[CRAN](https://CRAN.R-project.org) but you can install the latest
version from [github](https://github.com/stufield/power) via:

``` r
remotes::install_github("stufield/power")

# OR

remotes::install_version("stufield/power", "0.0.1")
```

## Loading `power`

Loading the `power` package is as simple as:

``` r
library(power)
```

## Plot power curves

The simplest way to generally plot power curves is via
`plot_power_curves()` which uses `power.t.test()` under the hood:

``` r
plot_power_curves(
  delta_vec = seq(0.5, 2, 0.1),
  power_vec = seq(0.5, 0.9, 0.1)
)
```

<img src="man/figures/README-plot-power-curves-1.png" width="100%" />

## Power curves and KS-distance

There is a loose “rule-of-thumb” relationship between
Sensitivity/Specificity and KS-distance, and of course, KS is related to
effect size. So we can visualize this relationship also via a standard
power curve:

``` r
ks_tables <- ks_power_table()
ks_tables
#> $n
#> # A tibble: 31 × 10
#>    SS       KS `power=0.60` `power=0.65` `power=0.70` `power=0.75` `power=0.80` `power=0.85`
#>    <chr> <dbl>        <dbl>        <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1 60/60  0.2         149.         158.         168.         178.         191.         206. 
#>  2 61/61  0.22        123.         131.         139.         147.         158.         170. 
#>  3 62/62  0.24        104.         110.         117.         124.         133.         143. 
#>  4 63/63  0.26         88.4         93.6         99.3        106.         113.         122. 
#>  5 64/64  0.28         76.4         80.8         85.7         91.2         97.4        105. 
#>  6 65/65  0.3          66.6         70.5         74.7         79.5         84.9         91.4
#>  7 66/66  0.32         58.7         62.1         65.8         69.9         74.6         80.3
#>  8 67/67  0.34         52.1         55.1         58.3         61.9         66.1         71.1
#>  9 68/68  0.36         46.5         49.2         52.1         55.3         58.9         63.4
#> 10 69/69  0.38         41.9         44.2         46.8         49.6         52.9         56.8
#> # ℹ 21 more rows
#> # ℹ 2 more variables: `power=0.90` <dbl>, `power=0.95` <dbl>
#> 
#> $power
#> # A tibble: 31 × 11
#>    SS       KS  `n=20` `n=30` `n=40` `n=50` `n=60` `n=70` `n=80` `n=90` `n=100`
#>    <chr> <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#>  1 60/60  0.2  0.00444 0.0129 0.0283 0.0519 0.0844  0.126  0.175  0.231   0.292
#>  2 61/61  0.22 0.00676 0.0206 0.0459 0.0841 0.135   0.198  0.270  0.347   0.426
#>  3 62/62  0.24 0.0101  0.0320 0.0715 0.130  0.205   0.292  0.385  0.479   0.569
#>  4 63/63  0.26 0.0149  0.0482 0.107  0.191  0.292   0.403  0.513  0.616   0.705
#>  5 64/64  0.28 0.0216  0.0706 0.155  0.268  0.396   0.524  0.641  0.740   0.818
#>  6 65/65  0.3  0.0307  0.101  0.215  0.358  0.508   0.644  0.755  0.840   0.900
#>  7 66/66  0.32 0.0429  0.139  0.288  0.459  0.620   0.752  0.847  0.911   0.951
#>  8 67/67  0.34 0.0591  0.188  0.372  0.563  0.724   0.840  0.914  0.956   0.979
#>  9 68/68  0.36 0.0799  0.246  0.464  0.665  0.813   0.905  0.956  0.981   0.992
#> 10 69/69  0.38 0.106   0.314  0.558  0.756  0.882   0.949  0.980  0.993   0.998
#> # ℹ 21 more rows
#> 
#> attr(,"class")
#> [1] "ks_pwr_table" "list"

# `power` as y-axis
plot(ks_tables)
```

<img src="man/figures/README-ks-curves-1.png" width="100%" />

``` r

# `n` as y-axis
plot(ks_tables, plot_power = FALSE)
```

<img src="man/figures/README-ks-curves-2.png" width="100%" />

------------------------------------------------------------------------

## Two-Groups

### Empirical Power via Simulation

A more robust (?) empirical calculation of power can be generated via
simulation:

``` r
# constant effect size (delta)
size_tbl <- withr::with_seed(1,
  t_power_curve(seq(10, 50, 2), delta = 0.75, nsim = 25L)
)
size_tbl
#> ── t-test Power Curve Simulation ───────────────────────────────────────────────────────────────────
#> • Sim table                 25 x 21
#> • Sims per calculation      25
#> • Repeats per sim (per box) 25
#> • Constant                  delta = 0.75
#> • Varying                   n
#> • Sequence                  10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50
#> ════════════════════════════════════════════════════════════════════════════════════════════════════

plot(size_tbl)
```

<img src="man/figures/README-power-curve-n-1.png" width="100%" />

``` r
# constant sample size (n)
delta_tbl <- withr::with_seed(2,
  t_power_curve(seq(0.5, 2.5, 0.1), n = 10, nsim = 25L)
)
delta_tbl
#> ── t-test Power Curve Simulation ───────────────────────────────────────────────────────────────────
#> • Sim table                 25 x 21
#> • Sims per calculation      25
#> • Repeats per sim (per box) 25
#> • Constant                  n = 10
#> • Varying                   delta
#> • Sequence                  0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5
#> ════════════════════════════════════════════════════════════════════════════════════════════════════

plot(delta_tbl)
```

<img src="man/figures/README-power-curve-d-1.png" width="100%" />

### Solve for Sample Size

To solve for the sample size given a corresponding power value you must
have simulate power keeping “delta” (effect size) constant and varying
`n`. For example, the `size_tbl` object created above:

``` r
solve_n(size_tbl, 0.8)
#>  power      n 
#>  0.800 22.878
```

## Fisher’s Exact for Count Data

For count data, Fisher’s Exact tests assume a 2x2 contingency matrix and
equal proportions across the margins (rows x cols):

``` r
fisher_power(0.85, 0.75, 200, 200, nsim = 200L)
#> [1] 0.61
```

### Fisher’s Power Curve

``` r
f_tbl <- fisher_power_curve(seq(50, 400, 5), p = 0.85, p_diff = -0.1,
                            nsim = 200L)
f_tbl
#> ── Fisher's Exact Power Curve Simulation ───────────────────────────────────────────────────────────
#> • Sim table                 71 x 2
#> • Sims per calculation      200
#> • p                         0.85
#> • delta                     -0.1
#> • Varying                   n
#> • Sequence `n`              50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190, 195, 200, 205, 210, 215, 220, 225, 230, 235, 240, 245, 250, 255, 260, 265, 270, 275, 280, 285, 290, 295, 300, 305, 310, 315, 320, 325, 330, 335, 340, 345, 350, 355, 360, 365, 370, 375, 380, 385, 390, 395, 400
#> ════════════════════════════════════════════════════════════════════════════════════════════════════
```

### Plot the Power Curve

``` r
gg_pwr <- plot(f_tbl)
gg_pwr
```

<img src="man/figures/README-plot-fisher-curve-1.png" width="100%" />

### Solve for `n`

``` r
pwr_n <- solve_n(f_tbl, 0.8)
pwr_n
#>   power       n 
#>   0.800 270.735
```

Visually check the curve and add the solution to the `ggplot`.

``` r
gg_pwr +
  ggplot2::annotate("segment",
    x        = c(pwr_n[["n"]], min(f_tbl$n)),
    xend     = c(pwr_n[["n"]],pwr_n[["n"]]),
    y        = c(min(f_tbl$power), pwr_n[["power"]]),
    yend     = c(pwr_n[["power"]], pwr_n[["power"]]),
    linetype = "dashed", colour = "#00A499")
```

<img src="man/figures/README-plot-fisher-curve2-1.png" width="100%" />
