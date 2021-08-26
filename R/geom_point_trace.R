#' Highlight points
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
#' @param geom The geometric object to use display the data
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'    a call to a position adjustment function.
#' @param ... Other arguments passed on to [layer()]. These are often
#'     aesthetics, used to set an aesthetic to a fixed value, like
#'     `colour = "red"` or `size = 3`. They may also be parameters to the
#'     paired geom/stat.
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
#' @export
# https://stackoverflow.com/questions/67573707/ggplot-extension-function-to-plot-a-superimposed-mean-in-a-scatterplot
geom_point_trace <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                             ..., trace_position = "all", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointTrace <- ggproto(
  "GeomPointTrace", Geom,

  required_aes = c("x", "y"),

  non_missing_aes = c("size", "shape", "colour", "trace_size", "trace_colour", "trace_linetype"),

  default_aes = aes(
    shape  = 19,
    colour = "black",
    size   = 1.5,
    fill   = NA,
    alpha  = NA,
    stroke = 0.5,
    trace_size     = 1,
    trace_color    = "black",
    trace_linetype = 1
  ),

  draw_group = function(data, panel_params, coord, trace_position = "all", na.rm = FALSE) {

    pch_table <- c(
      "0" = 0,      # "square open"
      "1" = 1,      # "circle open"
      "2" = 2,      # "triangle open"
      "3" = 3,      # "plus"
      "4" = 4,      # "cross"
      "5" = 5,      # "diamond open"
      "6" = 6,      # "triangle down open"
      "7" = 7,      # "square cross"
      "8" = 8,      # "asterisk"
      "9" = 9,      # "diamond plus"
      "10" = 10,    # "circle plus"
      "11" = 11,    # "star"
      "12" = 12,    # "square plus"
      "13" = 13,    # "circle cross"
      "14" = 14,    # "square triangle"
      "14" = 14,    # "triangle square"
      "15" = 0,     # "square"
      "16" = 1,     # "circle small"
      "17" = 2,     # "triangle"
      "18" = 5,     # "diamond"
      "19" = 1,     # "circle"
      "20" = 20,    # "bullet"
      "21" = 21,    # "circle filled"
      "22" = 22,    # "square filled"
      "23" = 23,    # "diamond filled"
      "24" = 24,    # "triangle filled"
      "25" = 25     # "triangle down filled"
    )

    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }

    coords <- coord$transform(data, panel_params)

    grid::grobTree(
      grid::pointsGrob(
        coords$x, coords$y,
        pch  = pch_table[as.character(data$shape)],

        gp = grid::gpar(
          col      = alpha(coords$trace_colour, coords$alpha),
          fontsize = coords$size * .pt + coords$stroke * .stroke / 2 + coords$trace_size * .stroke / 2,
          lwd      = (coords$trace_size * .stroke / 2),
          lty      = coords$trace_linetype
        )
      ),

      grid::pointsGrob(
        coords$x, coords$y,
        pch  = coords$shape,

        gp = grid::gpar(
          col      = alpha(coords$colour, coords$alpha),
          fill     = alpha(coords$fill, coords$alpha),
          fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
          lwd      = coords$stroke * .stroke / 2
        )
      )
    )
  },

  draw_key = draw_key_point
)

clusters %>%
  ggplot(aes(UMAP_1, UMAP_2, color = cluster)) +
  geom_point(size = 2) +
  geom_point_trace(
    data = ~ filter(.x, cluster == "c1"),
    size        = 2,
    trace_size  = 1,
    trace_color = "black"
  )



