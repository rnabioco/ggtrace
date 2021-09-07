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
#' @name draw_key
NULL

#' @rdname draw_key
#' @export
draw_key_point_trace <- function(data, params, size) {

  if (is.null(data$shape)) {
    data$shape <- 19

  } else if (is.character(data$shape)) {
    data$shape <- translate_shape_string(data$shape)
  }

  data$trace_shape <- translate_trace_shape(data$shape)
  data             <- calculate_trace_size(data)

  # Replace NULL values in data
  data$colour[is.null(data$colour)]     <- "black"
  data$fill[is.null(data$fill)]         <- "white"
  data$linetype[is.null(data$linetype)] <- 1
  data$size[is.null(data$size)]         <- 1.5

  # Trace grob
  pt_stroke <- 0.5

  trace_grob <- grid::pointsGrob(
    0.5, 0.5,
    pch = data$trace_shape,
    gp  = grid::gpar(
      col      = alpha(data$colour, 1),
      lty      = data$linetype,
      fontsize = data$trace_fontsize,
      lwd      = data$trace_lwd
    )
  )

  # Points grob
  points_grob <- grid::pointsGrob(
    0.5, 0.5,
    pch = data$shape,
    gp  = grid::gpar(
      col      = alpha(data$fill, data$alpha),
      fontsize = data$size * .pt + pt_stroke * .stroke / 2,
      lwd      = pt_stroke * .stroke / 2
    )
  )

  grid::grobTree(trace_grob, points_grob)
}

#' @export
#' @rdname draw_key
draw_key_path_trace <- function(data, params, size) {

  # Replace NULL values in data
  if (is.null(data$linetype)) {
    data$linetype <- 1

  } else {
    data$linetype[is.na(data$linetype)] <- 1
  }

  data$colour[is.null(data$colour)] <- "black"
  data$fill[is.null(data$fill)]     <- "white"
  data$size[is.null(data$size)]     <- 0.5

  # Trace grob
  trace_grob <- grid::segmentsGrob(
    0.1, 0.5, 0.9, 0.5,

    gp = grid::gpar(
      col     = alpha(data$colour, 1),
      lwd     = data$size * .pt + data$stroke * .pt * 2,
      lty     = 1,
      lineend = "butt"
    ),

    arrow = params$arrow
  )

  # Segments grob
  points_grob <- grid::segmentsGrob(
    0.1, 0.5, 0.9, 0.5,

    gp = grid::gpar(
      col     = alpha(data$fill, 1),
      lwd     = data$size * .pt,
      lty     = data$linetype,
      lineend = "butt"
    ),

    arrow = params$arrow
  )

  grid::grobTree(trace_grob, points_grob)
}
