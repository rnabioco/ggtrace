#' Alpha transparency scales
#'
#' Alpha-transparency scales are not tremendously useful, but can be a
#' convenient way to visually down-weight less important observations.
#' scale_alpha() is an alias for scale_alpha_continuous() since that is the
#' most common use of alpha, and it saves a bit of typing.
#'
#' @inheritParams ggplot2::scale_alpha
#' @export
scale_trace_alpha <- function(..., range = c(0.1, 1)) {

  continuous_scale("trace_alpha", "alpha_c", scales::rescale_pal(range), ...)
}

#' @rdname scale_trace_alpha
#' @export
scale_trace_alpha_continuous <- scale_trace_alpha

#' @rdname scale_trace_alpha
#' @export
scale_trace_alpha_binned <- function(..., range = c(0.1, 1)) {

  binned_scale("trace_alpha", "alpha_b", scales::rescale_pal(range), ...)
}

#' @rdname scale_trace_alpha
#' @export
scale_trace_alpha_discrete <- function(...) {

  rlang::warn("Using alpha for a discrete variable is not advised.")
  scale_trace_alpha_ordinal(...)
}

#' @rdname scale_trace_alpha
#' @export
scale_trace_alpha_ordinal <- function(..., range = c(0.1, 1)) {

  discrete_scale(
    "trace_alpha",
    "alpha_d",
    function(n) seq(range[1], range[2], length.out = n),
    ...
  )
}
