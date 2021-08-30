#' @inherit ggplot2::scale_colour_hue
#' @export
scale_trace_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                             direction = 1, na.value = "grey50", aesthetics = "trace_colour") {

  discrete_scale(aesthetics, "hue", scales::hue_pal(h, c, l, h.start, direction),
    na.value = na.value, ...)
}

#' @rdname scale_trace_colour_hue
#' @usage NULL
#' @export
scale_trace_color_hue <- scale_trace_colour_hue

#' @inherit ggplot2::scale_colour_discrete
#' @export
scale_trace_colour_discrete <- function(..., type = getOption("ggplot2.discrete.colour")) {

  type <- type %||% scale_trace_colour_hue

  if (is.function(type)) {
    check_scale_type(
      type(...),
      "scale_trace_colour_discrete",
      "trace_colour",
      scale_is_discrete = TRUE
    )
  } else {
    scale_trace_colour_qualitative(..., type = type)
  }
}

scale_trace_colour_qualitative <- function(..., type = NULL, h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                           direction = 1, na.value = "grey50", aesthetics = "trace_colour") {
  discrete_scale(
    aesthetics, "qualitative", qualitative_pal(type, h, c, l, h.start, direction),
    na.value = na.value, ...
  )
}

#' @rdname scale_trace_colour_discrete
#' @usage NULL
#' @export
scale_trace_color_discrete <- scale_trace_colour_discrete

#' Given set(s) of colour codes (i.e., type), find the smallest set that can support n levels
#' @param type a character vector or a list of character vectors
#' @noRd
qualitative_pal <- function(type, h, c, l, h.start, direction) {

  function(n) {
    type_list <- if (!is.list(type)) list(type) else type

    if (!all(vapply(type_list, is.character, logical(1)))) {
      rlang::abort("`type` must be a character vector or a list of character vectors", call. = FALSE)
    }

    type_lengths <- vapply(type_list, length, integer(1))

    # If there are more levels than color codes default to hue_pal()
    if (max(type_lengths) < n) {
      return(scales::hue_pal(h, c, l, h.start, direction)(n))
    }

    # Use the minimum length vector that exceeds the number of levels (n)
    type_list <- type_list[order(type_lengths)]

    i <- 1

    while (length(type_list[[i]]) < n) {
      i <- i + 1
    }

    type_list[[i]][seq_len(n)]
  }
}
