
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
`geom_point` to control the appearance of data points and outlines. The
fill color for each point can be modified with `fill` and the outline
color can be modified with `color`. Additional parameters including
`size`, `linetype`, `stroke` and `alpha` are also accepted.

``` r
library(ggplot2)
library(ggtrace)

p <- ggplot(
  clusters,
  aes(UMAP_1, UMAP_2, fill = cluster)
) + 
  theme_minimal()

p +
  geom_point_trace(
    color    = "black",
    size     = 1,
    linetype = 1,
    alpha    = 1
  )
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

<br>

## Aesthetics

Like other `ggplot2` geoms, variables can be mapped to aesthetic
attributes to modify the outline appearance.

``` r
ggplot(
  clusters,
  aes(UMAP_1, UMAP_2, color = cluster)
) +
  theme_minimal() +
  geom_point_trace(
    fill   = "black",
    stroke = 2
  )
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

By specifying `group` within `aes()`, outlines can also be added when
coloring with a continuous variable.

``` r
p <- ggplot(
  clusters,
  aes(UMAP_1, UMAP_2, fill = signal, group = cluster)
) +
  theme_minimal()

p +
  scale_fill_gradient(low = "white", high = "red") +
  geom_point_trace(stroke = 0.5)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Aesthetics can be further modified using the ggplot2 `scale_*`
functions.

``` r
p <- ggplot(
  clusters,
  aes(UMAP_1, UMAP_2, color = sample)
) +
  theme_minimal()

p +
  geom_point_trace(fill = "white") +
  scale_fill_manual(values = c("red", "#0072B2"))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

<br>

## Position

The ‘position’ of the outline can be modified with the
`outline_position` parameter. This can be ‘all’, ‘bottom’, or a
predicate selecting the points to outline. By default ‘all’ groups are
outlined.

To only add a single outline around all points plotted, set
`trace_position` to ‘bottom’.

``` r
p <- ggplot(
  clusters,
  aes(UMAP_1, UMAP_2, fill = cluster)
) +
  theme_minimal()

p +
  geom_point_trace(
    trace_position = "bottom"
  )
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

A subset of data points can be highlighted by passing a predicate to
`outline_position`. This must evaluate to `TRUE` or `FALSE` within the
context of the input data.

``` r
p +
  geom_point_trace(
    trace_position = signal < 0
  )
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

The color of background points can be modified using the
`background_color` argument.

``` r
p +
  geom_point_trace(
    trace_position   = signal < 0,
    background_color = "grey75"
  )
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />
