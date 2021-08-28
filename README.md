
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggoutline

<!-- badges: start -->
[![R build
status](https://github.com/rnabioco/ggoutline/workflows/R-CMD-check/badge.svg)](https://github.com/rnabioco/ggoutline/actions)
[![Codecov test
coverage](https://codecov.io/gh/rnabioco/ggoutline/branch/master/graph/badge.svg)](https://codecov.io/gh/rnabioco/ggoutline?branch=master)
<!-- badges: end -->

## Installation

You can install the development version of ggoutline from
[GitHub](https://github.com/rnabioco/ggoutline) with:

``` r
# install.packages("devtools")
devtools::install_github("rnabioco/ggoutline")
```

<br>

## Rationale

A scatter plot is a common way to compare two continuous variables.
However, when there are thousands of data points, it can be difficult to
distinguish between groups based on color alone. This is a first attempt
at a package that allows groups of data points to be highlighted using
ggplot2. This is under development and likely contains numerous bugs.

geom\_point\_trace will trace each group plotted

``` r
library(ggoutline)
library(tidyverse)
library(cowplot)

th <- theme_cowplot() +
  theme(
    axis.line    = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey85"),
    axis.ticks   = element_line(color = "grey85")
  )

p <- clusters %>%
  ggplot(aes(UMAP_1, UMAP_2, color = cluster)) +
  th

p +
  geom_point_trace(
    trace_color = "black",
    trace_size  = 1
  )
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

<br>

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

<br>

A single cluster can be highlighted using standard ggplot2 syntax.

``` r
p +
  geom_point() +
  geom_point_trace(
    data = ~ filter(.x, cluster == "c1"),
    trace_size = 1
  )
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

<br>

By specifying `group` within `aes()`, outlines can be added when
coloring with a continuous variable

``` r
p <- clusters %>%
  ggplot(aes(UMAP_1, UMAP_2, color = signal, group = cluster)) +
  scale_color_gradientn(colors = c("white", "red")) +
  th

p +
  geom_point_trace(
    size       = 5,
    trace_size = 1
  )
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
