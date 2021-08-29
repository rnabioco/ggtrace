
#' @rdname ggplot2::scale_manual
#' @export
scale_trace_colour_manual <- function(..., values, aesthetics = "trace_colour", breaks = waiver(), na.value = "grey50") {
  manual_scale(aesthetics, values, breaks, ..., na.value = na.value)
}

#' @rdname scale_trace_colour_manual
#' @export
scale_trace_color_manual <- scale_trace_colour_manual

#' @rdname ggplot2::scale_manual
#' @export
scale_trace_size_manual <- function(..., values, breaks = waiver(), na.value = NA) {
  manual_scale("trace_size", values, breaks, ..., na.value = na.value)
}

#' @rdname ggplot2::scale_manual
#' @export
scale_trace_linetype_manual <- function(..., values, breaks = waiver(), na.value = "blank") {
  manual_scale("trace_linetype", values, breaks, ..., na.value = na.value)
}

#' @rdname ggplot2::scale_manual
#' @export
scale_trace_alpha_manual <- function(..., values, breaks = waiver(), na.value = NA) {
  manual_scale("trace_alpha", values, breaks, ..., na.value = na.value)
}

manual_scale <- function(aesthetic, values = NULL, breaks = waiver(), ..., limits = NULL) {
  # check for missing `values` parameter, in lieu of providing
  # a default to all the different scale_*_manual() functions
  if (rlang::is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }

  if (is.null(limits)) {
    limits <- names(values)
  }

  # order values according to breaks
  if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
      !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    } else {
      names(values) <- breaks[1:length(values)]
    }
  }

  pal <- function(n) {
    if (n > length(values)) {
      rlang::abort(glue::glue("Insufficient values in manual scale. {n} needed but only {length(values)} provided."))
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal, breaks = breaks, limits = limits, ...)
}
