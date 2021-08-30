#' @rdname scale_trace_size
#' @usage NULL
#' @export
scale_trace_size_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                        limits = NULL, range = c(1, 6),
                                        trans = "identity", guide = "legend") {

  continuous_scale("trace_size", "area", scales::area_pal(range), name = name,
    breaks = breaks, labels = labels, limits = limits, trans = trans,
    guide = guide)
}

#' @inherit ggplot2::scale_size
#' @order 1
#' @export
scale_trace_size <- scale_trace_size_continuous

#' @rdname scale_trace_size
#' @export
scale_trace_size_binned <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                    limits = NULL, range = c(1, 6), n.breaks = NULL,
                                    nice.breaks = TRUE, trans = "identity", guide = "bins") {

  binned_scale("trace_size", "area_b", scales::area_pal(range), name = name,
               breaks = breaks, labels = labels, limits = limits, trans = trans,
               n.breaks = n.breaks, nice.breaks = nice.breaks, guide = guide)
}

#' @rdname scale_trace_size
#' @usage NULL
#' @export
scale_trace_size_discrete <- function(...) {

  rlang::warn("Using size for a discrete variable is not advised.")

  scale_trace_size_ordinal(...)
}

#' @rdname scale_trace_size
#' @usage NULL
#' @export
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

#' @inheritDotParams ggplot2::continuous_scale -aesthetics -scale_name -palette -rescaler
#' @param max_size Size of largest points.
#' @export
#' @rdname scale_trace_size
scale_trace_size_area <- function(..., max_size = 6) {

  continuous_scale("trace_size", "area",
                   palette  = scales::abs_area(max_size),
                   rescaler = scales::rescale_max, ...)
}

#' @rdname scale_trace_size
#' @export
scale_trace_size_binned_area <- function(..., max_size = 6) {

  binned_scale("trace_size", "area_b",
               palette  = scales::abs_area(max_size),
               rescaler = scales::rescale_max, ...)
}


