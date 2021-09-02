#' Key glyphs for legends
#'
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where
#' `*` stands for the name of the respective key glyph. The key glyphs can be
#' customized for individual geoms by providing a geom with the `key_glyph`
#' argument (see [`layer()`] or examples below.)
#'
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @examples
#' \dontrun{
#' p <- ggplot(economics, aes(date, psavert, color = "savings rate"))
#' # key glyphs can be specified by their name
#' p + geom_line(key_glyph = "timeseries")
#'
#' # key glyphs can be specified via their drawing function
#' p + geom_line(key_glyph = draw_key_rect)
#' }
#' @return A grid grob.
#' @export
draw_key_point_trace <- function(data, params, size) {

  if (is.null(data$shape)) {
    data$shape <- 19

  } else if (is.character(data$shape)) {
    data$shape <- translate_shape_string(data$shape)
  }

  data$trace_shape <- translate_trace_shape(data$shape)
  data             <- calculate_trace_size(data)

  g_trace <- grid::pointsGrob(
    0.5, 0.5,
    pch = data$trace_shape,
    gp  = grid::gpar(
      col      = alpha(data$trace_colour %||% "black", data$trace_alpha),
      lty      = data$trace_linetype %||% 1,
      fontsize = data$trace_fontsize,
      lwd      = data$trace_lwd
    )
  )

  g_points <- grid::pointsGrob(
    0.5, 0.5,
    pch = data$shape,
    gp  = grid::gpar(
      col      = alpha(data$colour %||% "black", data$alpha),
      fontsize = (data$size %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke / 2,
      lwd      = (data$stroke %||% 0.5) * .stroke / 2
    )
  )

  grid::grobTree(g_trace, g_points)
}
