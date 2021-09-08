#' Trace lines to improve clarity of plots with overplotted geoms.
#'
#' @inheritParams ggplot2::geom_path
#' @param trace_position Specifies which groups of data points should be
#'     outlined. Can be 'all' or a predicate to use for filtering data. If
#'     all', the default, every group plotted will be outlined. A subset of
#'     data points can be outlined by passing a predicate. This must evaluate
#'     to `TRUE` or `FALSE` within the context of the input data.
#' @param background_color Color to use for background lines when a predicate
#'     is passed to `trace_position`. If NULL, the original fill color will be
#'     used.
#' @eval rd_aesthetics("geom", "path_trace")
#' @export
geom_path_trace <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                            ..., trace_position = "all", background_color = NULL, lineend = "butt",
                            linejoin = "round", linemitre = 10, arrow = NULL, na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE) {

  params <- list(
    lineend   = lineend,
    linejoin  = linejoin,
    linemitre = linemitre,
    arrow     = arrow,
    na.rm     = na.rm,
    ...
  )

  create_trace_layers(
    mapping          = mapping,
    data             = data,
    stat             = stat,
    geom             = GeomPathTrace,
    position         = position,
    show.legend      = show.legend,
    inherit.aes      = inherit.aes,
    params           = params,
    trace_position   = substitute(trace_position),
    background_color = background_color,
    allow_bottom     = FALSE
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPathTrace <- ggproto(
  "GeomPathTrace", ggplot2::Geom,

  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(
    colour   = "black",
    fill     = "white",
    size     = 0.5,
    stroke   = 0.5,
    linetype = 1,
    alpha    = NA
  ),

  handle_na = function(data, params) {

    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    complete <- stats::complete.cases(data[c("x", "y", "size", "stroke", "fill", "linetype")])
    kept     <- stats::ave(complete, data$group, FUN = keep_mid_true)
    data     <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      warning("Removed ", sum(!kept), " row(s) containing missing values (geom_path_trace).")
    }

    data
  },

  setup_data = function(data, params, mapping) {

    # Want to adjust groups so lines with the same colour or fill do not have
    # overlapping outlines
    clmn <- c("colour", "fill")
    clmn <- clmn[clmn %in% colnames(data)]

    # When both colour and fill are present, use colour
    if ("colour" %in% clmn) {
      clmn <- "colour"
    }

    # Adjust groups
    if (length(clmn) == 1) {
      data$orig_group <- data$group

      clr_lst <- list()

      for (i in 1:nrow(data)) {

        dat <- data[i, ]
        clr <- dat[[clmn]]

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

  draw_group = function(data, panel_params, coord, arrow = NULL, lineend = "butt",
                        linejoin = "round", linemitre = 10, na.rm = FALSE) {

    if (!anyDuplicated(data$group)) {
      message("geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?")
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
      stop("geom_path: If you are using dotted or dashed lines, color, fill, size and linetype must be constant over the line")
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
          lwd       = munched$size[!end] * .pt + munched$stroke[!end] * .pt * 2,
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
          lwd       = munched$size[start] * .pt + munched$stroke * .pt * 2,
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

  draw_key = draw_key_path_trace
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
    rep(TRUE,  last - first),
    rep(FALSE, length(x) - last)
  )
}

#' @rdname geom_path_trace
#' @export
geom_line_trace <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                            na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE,
                            trace_position = "all", background_color = NULL, ...) {

  params <- list(
    orientation = orientation,
    na.rm       = na.rm,
    ...
  )

  create_trace_layers(
    mapping          = mapping,
    data             = data,
    stat             = stat,
    geom             = GeomLineTrace,
    position         = position,
    show.legend      = show.legend,
    inherit.aes      = inherit.aes,
    params           = params,
    trace_position   = substitute(trace_position),
    background_color = background_color,
    allow_bottom     = FALSE
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLineTrace <- ggproto(
  "GeomLineTrace", GeomPathTrace,

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)

    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {

    data$flipped_aes <- params$flipped_aes
    data             <- flip_data(data, params$flipped_aes)
    data             <- data[order(data$PANEL, data$group, data$x), ]

    data <- flip_data(data, params$flipped_aes)

    # Want to adjust groups so lines with the same colour or fill do not have
    # overlapping outlines
    clmn <- c("colour", "fill")
    clmn <- clmn[clmn %in% colnames(data)]

    # When both colour and fill are present, use colour
    if ("colour" %in% clmn) {
      clmn <- "colour"
    }

    # Adjust groups
    #
    if (length(clmn) == 1) {
      data$orig_group <- data$group

      clr_lst <- list()

      for (i in 1:nrow(data)) {

        dat <- data[i, ]
        clr <- dat[[clmn]]

        if (!clr %in% names(clr_lst)) {
          clr_lst[clr] <- grp <- dat$group

        } else {
          grp <- clr_lst[[clr]]
        }

        data[i, "group"] <- grp
      }
    }

    data
  }
)

#' @rdname geom_path_trace
#' @param direction direction of stairs: 'vh' for vertical then horizontal,
#'   'hv' for horizontal then vertical, or 'mid' for step half-way between
#'   adjacent x-values.
#' @export
geom_step_trace <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                            direction = "hv", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                            trace_position = "all", background_color = NULL, ...) {

  params <- list(
    direction = direction,
    na.rm     = na.rm,
    ...
  )

  create_trace_layers(
    mapping          = mapping,
    data             = data,
    stat             = stat,
    geom             = GeomStepTrace,
    position         = position,
    show.legend      = show.legend,
    inherit.aes      = inherit.aes,
    params           = params,
    trace_position   = substitute(trace_position),
    background_color = background_color,
    allow_bottom     = FALSE
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomStepTrace <- ggproto(
  "GeomStepTrace", GeomPathTrace,

  draw_group = function(data, panel_params, coord, direction = "hv") {
    data <- dapply(data, "group", stairstep, direction = direction)

    GeomPathTrace$draw_group(data, panel_params, coord)
  }
)

#' Calculate stairsteps for `geom_step_trace()`
#' Used by `GeomStepTrace()`
#' @noRd
stairstep <- function(data, direction = "hv") {

  direction <- match.arg(direction, c("hv", "vh", "mid"))
  data      <- as.data.frame(data)[order(data$x), ]
  n         <- nrow(data)

  if (n <= 1) {
    # Need at least one observation
    return(data[0, , drop = FALSE])
  }

  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each = 2))

  } else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2*n]
    xs <- c(1, rep(2:n, each = 2))

  } else if (direction == "mid") {
    xs <- rep(1:(n-1), each = 2)
    ys <- rep(1:n, each = 2)

  } else {
    stop("Parameter `direction` is invalid.")
  }

  if (direction == "mid") {
    gaps      <- data$x[-1] - data$x[-n]
    mid_x     <- data$x[-n] + gaps/2                 # map the mid-point between adjacent x-values
    x         <- c(data$x[1], mid_x[xs], data$x[n])
    y         <- c(data$y[ys])
    data_attr <- data[c(1,xs,n), setdiff(names(data), c("x", "y"))]

  } else {
    x         <- data$x[xs]
    y         <- data$y[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "y"))]
  }

  new_data_frame(c(list(x = x, y = y), data_attr))
}









