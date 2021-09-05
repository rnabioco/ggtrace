#' Connect observations
#'
#' @export
geom_path_trace <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                      ..., lineend = "butt", linejoin = "round", linemitre = 10, arrow = NULL,
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomPathTrace,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre,
      arrow     = arrow,
      na.rm     = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPathTrace <- ggproto("GeomPathTrace", ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  handle_na = function(data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    complete <- stats::complete.cases(data[c("x", "y", "size", "colour", "linetype")])
    kept     <- stats::ave(complete, data$group, FUN = keep_mid_true)
    data     <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      warn(glue("Removed {sum(!kept)} row(s) containing missing values (geom_path)."))
    }

    data
  },

  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {

    if (!anyDuplicated(data$group)) {
      message_wrap("geom_path: Each group consists of only one observation. ",
        "Do you need to adjust the group aesthetic?")
    }

    # must be sorted on group
    data    <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows    <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]

    if (nrow(munched) < 2) {
      return(zeroGrob())
    }

    # Work out whether we should use lines or segments
    attr <- dapply(munched, "group", function(df) {
      linetype <- unique(df$linetype)

      new_data_frame(
        list(
          solid    = identical(linetype, 1) || identical(linetype, "solid"),
          constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
        ),
        n = 1
      )
    })

    solid_lines <- all(attr$solid)
    constant    <- all(attr$constant)

    if (!solid_lines && !constant) {
      abort("geom_path: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line")
    }

    # Work out grouping variables for grobs
    n          <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start      <- c(TRUE, group_diff)
    end        <-   c(group_diff, TRUE)

    if (!constant) {

      # For trace alpha and linetype are always 1
      trace_grob <- grid::segmentsGrob(
        munched$x[!end],   munched$y[!end],
        munched$x[!start], munched$y[!start],

        default.units = "native",
        arrow         = arrow,

        # lwd <- data$stroke * .stroke / 2 * 2 + (pt_stroke * .stroke / 2)

        gp = grid::gpar(
          col       = alpha("black", 1)[!end],
          # col       = alpha(munched$colour, 1)[!end],
          fill      = alpha(munched$colour, 1)[!end],
          lwd       = munched$size[!end] * .pt * 2,
          lty       = 1,
          lineend   = lineend,
          linejoin  = linejoin,
          linemitre = linemitre
        )
      )

      line_grob <- grid::segmentsGrob(
        munched$x[!end],   munched$y[!end],
        munched$x[!start], munched$y[!start],

        default.units = "native",
        arrow         = arrow,

        gp = grid::gpar(
          col       = alpha(munched$colour, munched$alpha)[!end],
          # col       = alpha(munched$colour, munched$alpha)[!end],
          fill      = alpha(munched$colour, munched$alpha)[!end],
          lwd       = munched$size[!end] * .pt,
          lty       = munched$linetype[!end],
          lineend   = lineend,
          linejoin  = linejoin,
          linemitre = linemitre
        )
      )
    } else {
      id <- match(munched$group, unique(munched$group))

      trace_grob <- grid::polylineGrob(
        munched$x, munched$y,

        id            = id,
        default.units = "native",
        arrow         = arrow,

        # For trace alpha and linetype are always 1
        gp = grid::gpar(
          col       = alpha("black", 1)[start],
          # col       = alpha(munched$colour, 1)[start],
          fill      = alpha(munched$colour, 1)[start],
          lwd       = munched$size[start] * .pt * 2,
          lty       = 1,
          lineend   = lineend,
          linejoin  = linejoin,
          linemitre = linemitre
        )
      )

      line_grob <- grid::polylineGrob(
        munched$x, munched$y,

        id            = id,
        default.units = "native",
        arrow         = arrow,

        gp = grid::gpar(
          col       = alpha(munched$colour, munched$alpha)[start],
          fill      = alpha(munched$colour, munched$alpha)[start],
          lwd       = munched$size[start] * .pt,
          lty       = munched$linetype[start],
          lineend   = lineend,
          linejoin  = linejoin,
          linemitre = linemitre
        )
      )
    }

    ggname("geom_path_trace", grid::grobTree(trace_grob, line_grob))
  },

  draw_key = ggplot2::draw_key_path
)

# Trim false values from left and right: keep all values from
# first TRUE to last TRUE
keep_mid_true <- function(x) {
  first <- match(TRUE, x) - 1
  if (is.na(first)) {
    return(rep(FALSE, length(x)))
  }

  last <- length(x) - match(TRUE, rev(x)) + 1
  c(
    rep(FALSE, first),
    rep(TRUE, last - first),
    rep(FALSE, length(x) - last)
  )
}

