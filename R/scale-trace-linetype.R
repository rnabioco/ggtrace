#' @inherit ggplot2::scale_linetype
#' @export
scale_trace_linetype <- function(..., na.value = "blank") {
  discrete_scale("trace_linetype", "linetype_d", scales::linetype_pal(), na.value = na.value, ...)
}

#' @rdname scale_trace_linetype
#' @export
scale_trace_linetype_binned <- function(..., na.value = "blank") {
  binned_scale("trace_linetype", "linetype_b", binned_pal(scales::linetype_pal()), ...)
}

#' @rdname scale_trace_linetype
#' @export
scale_trace_linetype_continuous <- function(...) {
  rlang::abort("A continuous variable can not be mapped to linetype")
}
#' @rdname scale_trace_linetype
#' @export
scale_trace_linetype_discrete <- scale_trace_linetype
