
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtrace

<!-- badges: start -->

[![R-CMD-check](https://github.com/rnabioco/ggtrace/workflows/R-CMD-check/badge.svg)](https://github.com/rnabioco/ggtrace/actions)
[![Codecov test
coverage](https://codecov.io/gh/rnabioco/ggtrace/branch/master/graph/badge.svg)](https://codecov.io/gh/rnabioco/ggtrace?branch=master)
<!-- badges: end -->

## Installation

You can install the development version of ggtrace from
[GitHub](https://github.com/rnabioco/ggtrace) with:

``` r
# install.packages("devtools")
devtools::install_github("rnabioco/ggtrace")
```

<br>

## Rationale

A scatter plot is a common way to compare two continuous variables.
However, when there are thousands of data points, it can be difficult to
distinguish between groups based on color alone. ggtrace provides
ggplot2 geoms that highlight groups of data points with an outline for
emphasis .

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
    trace_color = "black",
    trace_size  = 1
  )
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

To add a single outline around all points plotted, set `trace_position`
to ‘bottom’.

``` r
p +
  geom_point_trace(
    trace_color    = "black",
    trace_size     = 1,
    trace_position = "bottom"
  )
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

A single cluster can be highlighted using standard ggplot2 syntax.

``` r
p +
  geom_point() +
  geom_point_trace(
    data = ~ subset(.x, cluster == "c1"),
    trace_size = 1
  )
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

The trace color can be scaled based on a discrete or continuous variable

``` r
ggplot(
  clusters,
  aes(UMAP_1, UMAP_2, trace_color = cluster)
) +
  theme_minimal() +
  geom_point_trace(trace_size = 2)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

By specifying `group` within `aes()`, outlines can be added when
coloring with a continuous variable

``` r
ggplot(
  clusters,
  aes(UMAP_1, UMAP_2, color = signal, group = cluster)
) +
  theme_minimal() +
  scale_color_gradientn(
    colors = c("white", "red")
  ) +
  geom_point_trace(
    size       = 5,
    trace_size = 1
  )
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
