#' @inherit ggplot2::scale_colour_continuous
#' @export
scale_trace_colour_continuous <- function(...,
                                    type = getOption("ggplot2.continuous.colour")) {
  type <- type %||% "gradient"

  if (is.function(type)) {
    check_scale_type(type(...), "scale_trace_colour_continuous", "trace_colour")
  } else if (identical(type, "gradient")) {
    scale_colour_gradient(...)
  } else if (identical(type, "viridis")) {
    scale_colour_viridis_c(...)
  } else {
    rlang::abort("Unknown scale type")
  }
}

#' @rdname scale_trace_colour_continuous
#' @export
scale_trace_colour_binned <- function(...,
                                type = getOption("ggplot2.binned.colour")) {
  if (is.function(type)) {
    check_scale_type(type(...), "scale_trace_colour_binned", "trace_colour")
  } else {
    type_fallback <- getOption("ggplot2.continuous.colour", default = "gradient")
    # don't use fallback from scale_colour_continuous() if it is
    # a function, since that would change the type of the color
    # scale from binned to continuous
    if (is.function(type_fallback)) {
      type_fallback <- "gradient"
    }
    type <- type %||% type_fallback

    if (identical(type, "gradient")) {
      scale_colour_steps(...)
    } else if (identical(type, "viridis")) {
      scale_colour_viridis_b(...)
    } else {
      rlang::abort("Unknown scale type")
    }
  }
}

# helper function to make sure that the provided scale is of the correct
# type (i.e., is continuous and works with the provided aesthetic)
check_scale_type <- function(scale, name, aesthetic, scale_is_discrete = FALSE) {
  if (!is.ggproto(scale) || !inherits(scale, "Scale")) {
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
