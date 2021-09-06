#' Connect observations
#'
#' @export
geom_path_trace <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                            ..., trace_position = "all", background_color = NULL, stroke = 0.5,
                            lineend = "butt", linejoin = "round", linemitre = 10, arrow = NULL,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  # Store trace_position as expression to pass to fortify
  trace_expr <- substitute(trace_position)

  params <- list(
    stroke    = stroke,
    lineend   = lineend,
    linejoin  = linejoin,
    linemitre = linemitre,
    arrow     = arrow,
    na.rm     = na.rm,
    ...
  )

  # If trace_position includes a logical operator is.call will evaluate TRUE,
  # character, numeric, and symbol will evaluate FALSE
  if (trace_expr != "all") {

    # Store original data input to use for background points
    bkgd_data <- data

    # If data is not NULL, the user has passed a data.frame, function, or
    # formula to the geom. Need to fortify this before applying the predicate
    # passed through trace_position. For a formula fortify will return an
    # anonymous function, a data.frame will be returned if a data.frame is
    # passed
    if (!is.null(data)) {
      data <- ggplot2::fortify(data)
    }

    # If fortify returned a function, need to pass dots to this within a new
    # formula. This is passed back to fortify to generate an anonymous function
    # that encompasses what was passed to both data and trace_position
    if (is.function(data)) {
      d_fn <- data
      data <- ggplot2::fortify(~ subset(d_fn(...), eval(trace_expr)))

    } else if (is.data.frame(data) || is.null(data)) {
      data <- ggplot2::fortify(~ subset(.x, eval(trace_expr)))
    }

    # Adjust parameters for background points
    bkgd_params       <- params
    bkgd_params$color <- NA

    if (!is.null(background_color)) {
      bkgd_params$fill <- background_color
    }

    bkgd_lyr <- layer(
      data        = bkgd_data,
      mapping     = mapping,
      stat        = stat,
      geom        = GeomPathTrace,
      position    = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params      = bkgd_params
    )
  }

  # Create GeomPointTrace layer
  trace_lyr <- layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomPathTrace,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = params
  )

  # Return list with background and trace layers
  if (trace_expr != "all") {
    trace_lyr <- list(bkgd_lyr, trace_lyr)
  }

  trace_lyr
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPathTrace <- ggproto(
  "GeomPathTrace", ggplot2::Geom,

  required_aes = c("x", "y"),

  default_aes = aes(colour = "black", fill = "white", size = 0.5, linetype = 1, alpha = NA),

  handle_na = function(data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    complete <- stats::complete.cases(data[c("x", "y", "size", "fill", "linetype")])
    kept     <- stats::ave(complete, data$group, FUN = keep_mid_true)
    data     <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      warning(glue::glue("Removed {sum(!kept)} row(s) containing missing values (geom_path)."))
    }

    data
  },

  setup_data = function(data, params, mapping) {

    # Adjust groups if color is included in mapping
    if ("colour" %in% colnames(data)) {
      data$orig_group <- data$group

      clr_lst <- list()

      for (i in 1:nrow(data)) {

        dat <- data[i, ]
        clr <- dat$colour

        if (!clr %in% names(clr_lst)) {
          clr_lst[clr] <- grp <- dat$group

        } else {
          grp <- clr_lst[[clr]]
        }

        data[i, "group"] <- grp
      }
    }

    data
  },

  draw_group = function(data, panel_params, coord, stroke = 0.5, arrow = NULL,
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
          constant = nrow(unique(df[, c("alpha", "colour", "fill", "size", "linetype")])) == 1
        ),
        n = 1
      )
    })

    solid_lines <- all(attr$solid)
    constant    <- all(attr$constant)

    if (!solid_lines && !constant) {
      abort("geom_path: If you are using dotted or dashed lines, color, fill, size and linetype must be constant over the line")
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

        gp = grid::gpar(
          col       = alpha(munched$colour, 1)[!end],
          lwd       = munched$size[!end] * .pt + stroke * .pt * 2,
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
          col       = alpha(munched$fill, munched$alpha)[!end],
          lwd       = munched$size[!end] * .pt,
          lty       = munched$linetype[!end],
          lineend   = lineend,
          linejoin  = linejoin,
          linemitre = linemitre
        )
      )

    } else {

      id <- match(munched$group, unique(munched$group))

      if ("orig_group" %in% colnames(munched)) {
        id <- match(munched$orig_group, unique(munched$orig_group))
      }

      trace_grob <- grid::polylineGrob(
        munched$x, munched$y,

        id            = id,
        default.units = "native",
        arrow         = arrow,

        # For trace alpha and linetype are always 1
        gp = grid::gpar(
          col       = alpha(munched$colour, 1)[start],
          lwd       = munched$size[start] * .pt + stroke * .pt * 2,
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
          col       = alpha(munched$fill, munched$alpha)[start],
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

