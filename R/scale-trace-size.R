
#' @rdname ggplot2::scale_size
#' @export
#' @usage NULL
scale_trace_size_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                        limits = NULL, range = c(1, 6),
                                        trans = "identity", guide = "legend") {

  continuous_scale("trace_size", "area", area_pal(range), name = name,
    breaks = breaks, labels = labels, limits = limits, trans = trans,
    guide = guide)
}

#' @rdname ggplot2::scale_size
#' @export
scale_trace_size <- scale_trace_size_continuous

#' @rdname ggplot2::scale_size
#' @export
#' @usage NULL
scale_trace_size_discrete <- function(...) {

  rlang::warn("Using size for a discrete variable is not advised.")

  scale_trace_size_ordinal(...)
}

#' @rdname ggplot2::scale_size
#' @export
#' @usage NULL
scale_trace_size_ordinal <- function(..., range = c(2, 6)) {
  force(range)

  discrete_scale(
    "trace_size",
    "size_d",
    function(n) {
      area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
      sqrt(area)
    },
    ...
  )
}
