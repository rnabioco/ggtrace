#' Trace points to improve clarity of plots with overplotted geoms.
#'
#' @param mapping Set of aesthetic mappings created by [aes()] or
#'    [aes_()]. If specified and `inherit.aes = TRUE` (the
#'    default), it is combined with the default mapping at the top level of the
#'    plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'    a call to a position adjustment function.
#' @param ... Other arguments passed on to [layer()]. These are often
#'     aesthetics, used to set an aesthetic to a fixed value, like
#'     `colour = "red"` or `size = 3`. They may also be parameters to the
#'     paired geom/stat.
#' @param trace_position Specifies how groups of points should be traced.
#'     If `all`, the default, every group plotted will be traced, if `bottom`,
#'     only the bottom most layer of points will be traced.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'     warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'    `NA`, the default, includes if any aesthetics are mapped.
#'    `FALSE` never includes, and `TRUE` always includes.
#'    It can also be a named logical vector to finely select the aesthetics to
#'    display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'    rather than combining with them. This is most useful for helper functions
#'    that define both data and aesthetics and shouldn't inherit behaviour from
#'    the default plot specification, e.g. [borders()].
#' @rdname geom_point_trace
#' @export
# https://stackoverflow.com/questions/67573707/ggplot-extension-function-to-plot-a-superimposed-mean-in-a-scatterplot
geom_point_trace <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                             ..., trace_position = "all", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  if (!trace_position %in% c("all", "bottom")) {
    stop("trace_position must be either 'all' or 'bottom'")
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomPointTrace,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      trace_position = trace_position,
      na.rm = na.rm,
      ...
    )
  )
}

#' GeomPointTrace
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointTrace <- ggplot2::ggproto(
  "GeomPointTrace", ggplot2::Geom,

  required_aes = c("x", "y"),

  non_missing_aes = c("size", "shape", "colour", "trace_size", "trace_colour", "trace_linetype"),

  default_aes = ggplot2::aes(
    shape  = 19,
    colour = "black",
    fill   = NA,
    alpha  = 1,
    size   = 1.5,
    stroke = 0.5,
    trace_size     = 1,
    trace_color    = "black",
    trace_linetype = 1
  ),

  setup_data = function(data, params) {
    if (params$trace_position == "bottom") {
      data$group <- -1
    }

    data
  },

  draw_group = function(data, panel_params, coord, trace_position = "all", na.rm = FALSE) {

    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }

    data$trace_shape <- translate_trace_shape(data$shape)

    data <- translate_trace_size(data)

    coords <- coord$transform(data, panel_params)

    g_trace <- grid::pointsGrob(
      coords$x, coords$y,

      pch = coords$trace_shape,

      gp = grid::gpar(
        col      = alpha(coords$trace_colour, 1),
        lty      = coords$trace_linetype,
        fontsize = coords$trace_fontsize,
        lwd      = coords$trace_lwd

        # Closed shape
        # fontsize = coords$size * .pt + coords$stroke * .stroke / 2 + coords$trace_size * .stroke / 2,
        # lwd      = coords$trace_size * .stroke / 2 + coords$stroke * .stroke / 2

        # Open shape
        # fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
        # lwd      = (coords$trace_size * 2) * .stroke / 2 + coords$stroke * .stroke / 2
      )
    )

    g_points <- grid::pointsGrob(
      coords$x, coords$y,

      pch = coords$shape,

      gp = grid::gpar(
        col      = alpha(coords$colour, coords$alpha),
        fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
        lwd      = coords$stroke * .stroke / 2
        # fill     = alpha(coords$fill, coords$alpha),
      )
    )

    ggname("geom_point_trace", grid::grobTree(g_trace, g_points))
  },

  draw_key = ggplot2::draw_key_point
)


#' Helper to translate shape strings
#' https://github.com/tidyverse/ggplot2/raw/87e9b85dd9f2a294f339d88a353d0c11c851489d/R/geom-point.r
#' @noRd
translate_shape_string <- function(shape_string) {

  # strings of length 0 or 1 are interpreted as symbols by grid
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }

  pch_table <- c(
    "square open"           = 0,
    "circle open"           = 1,
    "triangle open"         = 2,
    "plus"                  = 3,
    "cross"                 = 4,
    "diamond open"          = 5,
    "triangle down open"    = 6,
    "square cross"          = 7,
    "asterisk"              = 8,
    "diamond plus"          = 9,
    "circle plus"           = 10,
    "star"                  = 11,
    "square plus"           = 12,
    "circle cross"          = 13,
    "square triangle"       = 14,
    "triangle square"       = 14,
    "square"                = 15,
    "circle small"          = 16,
    "triangle"              = 17,
    "diamond"               = 18,
    "circle"                = 19,
    "bullet"                = 20,
    "circle filled"         = 21,
    "square filled"         = 22,
    "diamond filled"        = 23,
    "triangle filled"       = 24,
    "triangle down filled"  = 25
  )

  shape_match <- charmatch(shape_string, names(pch_table))

  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0

  if (any(invalid_strings)) {
    bad_string <- unique(shape_string[invalid_strings])
    n_bad <- length(bad_string)

    collapsed_names <- sprintf("\n* '%s'", bad_string[1:min(5, n_bad)])

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }

    abort(glue("Can't find shape name:", collapsed_names, more_problems))
  }

  if (any(nonunique_strings)) {
    bad_string <- unique(shape_string[nonunique_strings])
    n_bad <- length(bad_string)

    n_matches <- vapply(
      bad_string[1:min(5, n_bad)],
      function(shape_string) sum(grepl(paste0("^", shape_string), names(pch_table))),
      integer(1)
    )

    collapsed_names <- sprintf(
      "\n* '%s' partially matches %d shape names",
      bad_string[1:min(5, n_bad)], n_matches
    )

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }

    abort(glue("Shape names must be unambiguous:", collapsed_names, more_problems))
  }

  unname(pch_table[shape_match])
}

#' Helper to adjust trace size
#'
#' To outline both the inside and outside of open shapes, need to adjust
#' fontsize and lwd.
#'
#' @noRd
translate_trace_size <- function(data) {
  pch_open <- 0:14

  pch <- data$shape

  # Calculate fontsize for closed shapes
  fontsize  <- data$size * .pt + data$stroke * .stroke / 2

  fontsize[!pch %in% pch_open] <- fontsize[!pch %in% pch_open] + data$trace_size * .stroke / 2

  # Calculate lwd for open shapes
  lwd <- data$trace_size * .stroke / 2

  lwd[pch %in% pch_open] <- lwd[pch %in% pch_open] * 2 + (data$stroke * .stroke / 2)

  # Add results to data
  data$trace_fontsize <- fontsize
  data$trace_lwd      <- lwd

  data
}

#' Helper to adjust shape specification
#' @noRd
translate_trace_shape <- function(pch) {
  pch_tbl <- c(
    "0" = 0,       # "square open"
    "1" = 1,       # "circle open"
    "2" = 2,       # "triangle open"
    "3" = 3,       # "plus"
    "4" = 4,       # "cross"
    "5" = 5,       # "diamond open"
    "6" = 6,       # "triangle down open"
    "7" = 7,       # "square cross"
    "8" = 8,       # "asterisk"
    "9" = 9,       # "diamond plus"
    "10" = 10,     # "circle plus"
    "11" = 11,     # "star"
    "12" = 12,     # "square plus"
    "13" = 13,     # "circle cross"
    "14" = 14,     # "square triangle"
    "14" = 14,     # "triangle square"
    "15" = 0,      # "square"
    "16" = 1,      # "circle small"
    "17" = 2,      # "triangle"
    "19" = 19,     # "circle"
    "20" = 20      # "bullet"
    # "18" = 18,     # "diamond"
    # "21" = 21,     # "circle filled"
    # "22" = 22,     # "square filled"
    # "23" = 23,     # "diamond filled"
    # "24" = 24,     # "triangle filled"
    # "25" = 25      # "triangle down filled"
  )

  pch_match <- charmatch(pch, names(pch_tbl))

  bad_pch <- is.na(pch_match)

  if (any(bad_pch)) {

    bad_pch <- unique(pch[bad_pch])

    stop("Unsupported shape ", paste0(bad_pch, collapse = ", "))
  }

  res <- unname(pch_tbl[pch_match])

  res
}


