#' Discrete trace color scales
#'
#' The default discrete color scale. Defaults to [scale_fill_hue()]/[scale_fill_brewer()]
#' unless `type` (which defaults to the `ggplot2.discrete.fill`/`ggplot2.discrete.color` options)
#' is specified.
#'
#' @param ... Additional parameters passed on to the scale type,
#' @param type One of the following:
#'   * A character vector of color codes. The codes are used for a 'manual' color
#'   scale as long as the number of codes exceeds the number of data levels
#'   (if there are more levels than codes, [scale_color_hue()]/[scale_fill_hue()]
#'   are used to construct the default scale). If this is a named vector, then the color values
#'   will be matched to levels based on the names of the vectors. Data values that
#'   don't match will be set as `na.value`.
#'   * A list of character vectors of color codes. The minimum length vector that exceeds the
#'   number of data levels is chosen for the color scaling. This is useful if you
#'   want to change the color palette based on the number of levels.
#'   * A function that returns a discrete color/fill scale (e.g., [scale_fill_hue()],
#'   [scale_fill_brewer()], etc).
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
    ggplot2::scale_colour_qualitative(..., type = type)
  }
}

scale_trace_color_discrete <- scale_trace_colour_discrete

#' @rdname ggplot2::scale_manual
#' @export
scale_trace_colour_manual <- function(..., values, aesthetics = "trace_colour", breaks = waiver(), na.value = "grey50") {
  manual_scale(aesthetics, values, breaks, ..., na.value = na.value)
}

scale_trace_color_manual <- scale_trace_colour_manual

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
  ggplot2::discrete_scale(aesthetic, "manual", pal, breaks = breaks, limits = limits, ...)
}

#' Evenly spaced colors for discrete data
#'
#' Maps each level to an evenly spaced hue on the color wheel.
#' It does not generate color-blind safe palettes.
#'
#' @param na.value color to use for missing values
#' @inheritDotParams ggplot2::discrete_scale -aesthetics
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply color settings to the `color` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("color", "fill")`.
#' @inheritParams scales::hue_pal
#' @rdname scale_hue
#' @export
scale_trace_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                   direction = 1, na.value = "grey50", aesthetics = "trace_colour") {

  ggplot2::discrete_scale(aesthetics, "hue", scales::hue_pal(h, c, l, h.start, direction),
                          na.value = na.value, ...)
}


scale_colour_qualitative <- function(..., type = NULL, h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                     direction = 1, na.value = "grey50", aesthetics = "trace_colour") {
  ggplot2::discrete_scale(
    aesthetics, "qualitative", qualitative_pal(type, h, c, l, h.start, direction),
    na.value = na.value, ...
  )
}

# helper function to make sure that the provided scale is of the correct
# type (i.e., is continuous and works with the provided aesthetic)
check_scale_type <- function(scale, name, aesthetic, scale_is_discrete = FALSE) {
  if (!ggplot2::is.ggproto(scale) || !inherits(scale, "Scale")) {
    rlang::abort(glue::glue("The `type` argument of `{name}()` must return a continuous scale for the {aesthetic} aesthetic. The provided object is not a scale function."))
  }
  if (!isTRUE(aesthetic %in% scale$aesthetics)) {
    rlang::abort(glue::glue("The `type` argument of `{name}()` must return a continuous scale for the {aesthetic} aesthetic. The provided scale works with the following aesthetics: {glue::glue_collapse(scale$aesthetics, sep = ', ')}"))
  }
  if (isTRUE(scale$is_discrete()) != scale_is_discrete) {
    scale_types <- c("continuous", "discrete")
    if (scale_is_discrete) {
      scale_types <- rev(scale_types)
    }
    rlang::abort(glue::glue("The `type` argument of `{name}()` must return a {scale_types[1]} scale for the {aesthetic} aesthetic, but the provided scale is {scale_types[2]}."))
  }

  scale
}

is.waive <- function(x) inherits(x, "waiver")
