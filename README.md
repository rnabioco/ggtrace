
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtrace

<!-- badges: start -->

[![R-CMD-check](https://github.com/rnabioco/ggtrace/workflows/R-CMD-check/badge.svg)](https://github.com/rnabioco/ggtrace/actions)
[![Codecov test
coverage](https://codecov.io/gh/rnabioco/ggtrace/branch/master/graph/badge.svg)](https://codecov.io/gh/rnabioco/ggtrace?branch=master)
<!-- badges: end -->

## Overview

A scatter plot is a common way to compare two continuous variables.
However, when there are thousands of data points, it can be difficult to
distinguish between groups based on color alone. ggtrace provides
ggplot2 geoms that highlight groups of data points with an outline for
emphasis.

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
`geom_point` to control the appearance of data points and outlines. This
includes `fill`, `color`, `size`, `linetype`, `stroke` and `alpha`. The
arguments `trace_position` and `background_color` can be used to select
specific sets of points to highlight. For more examples see the
[vignette]().

``` r
library(ggplot2)
library(ggtrace)

p <- ggplot(
  clusters,
  aes(UMAP_1, UMAP_2, color = cluster)
) + 
  theme_minimal()

p +
  geom_point_trace(
    size             = 1,
    fill             = "white",
    trace_position   = signal < 0,
    background_color = "grey85"
  )
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
