
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtrace

<!-- badges: start -->

[![R-CMD-check](https://github.com/rnabioco/ggtrace/workflows/R-CMD-check/badge.svg)](https://github.com/rnabioco/ggtrace/actions)
[![Codecov test
coverage](https://codecov.io/gh/rnabioco/ggtrace/branch/master/graph/badge.svg)](https://codecov.io/gh/rnabioco/ggtrace?branch=master)
<!-- badges: end -->

ggtrace provides ggplot2 geoms that allow groups of data points to be
highlighted with an outline for emphasis.

<br>

## Installation

You can install the development version of ggtrace from
[GitHub](https://github.com/rnabioco/ggtrace) with:

``` r
# install.packages("devtools")
devtools::install_github("rnabioco/ggtrace")
```

<br>

## Basic Usage

`geom_point_trace` accepts graphical parameters normally passed to
`geom_point` to control the appearance of data points and outlines. The
`trace_position` argument can be used to select specific sets of points
to highlight. For more examples see the
[vignette](https://rnabioco.github.io/ggtrace/articles/geom-point-trace.html).

``` r
library(ggplot2)
library(ggtrace)

ggplot(clusters, aes(UMAP_1, UMAP_2, color = cluster)) + 
  theme_minimal() +
  geom_point_trace(
    trace_position    = signal < 0,
    fill              = "white",
    background_params = list(color = NA, fill = "grey85")
  )
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

<br>

`geom_line_trace` accepts parameters normally passed to `geom_line` with
the following exceptions: `fill` controls the inner line color, `color`
controls the outline color, and `stroke` controls the outline width.
Like `geom_point_trace()`, the `trace_position` argument can be used to
select specific data points to highlight. For more examples see the
[vignette](https://rnabioco.github.io/ggtrace/articles/geom-line-trace.html).

``` r
ggplot(stocks, aes(day, value, color = name)) +
  theme_minimal() +
  geom_line_trace(
    trace_position    = day < 500 | day > 1500,
    stroke            = 1,
    background_params = list(color = NA, fill = "grey75")
  )
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
