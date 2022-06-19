#' Trace lines
#'
#' These geoms are similar to \code{ggplot2::geom_path()},
#' \code{ggplot2::geom_line()}, and \code{ggplot2::geom_step()}, but also
#' include the ability to highlight line segments of interest.
#' These geoms accept normal ggplot2 graphical parameters with
#' some modifications. \code{fill} controls the color of the center line,
#' \code{color} controls the outline color, and \code{stroke} controls
#' outline width, similar to how filled shapes are modified for other ggplot2
#' geoms. Additional parameters including \code{size}, \code{alpha},
#' \code{linetype}, \code{linejoin}, \code{lineend},  and \code{linemitre} are
#' also accepted.
#'
#' @inheritParams ggplot2::geom_path
#'
#' @param trace_position Specifies which data points to outline, can be one of:
#'
#' \itemize{
#'   \item "all" to outline every group plotted
#'   \item A predicate specifying which data points to outline. This must
#'         evaluate to \code{TRUE} or \code{FALSE} within the context of the
#'         input data. e.g. \code{value > 100}
#' }
#'
#' @param background_params Named list specifying aesthetic parameters to use
#'    for background data points when a predicate is passed to
#'    \code{trace_position}, e.g. \code{list(color = "red")}
#'
#' @eval rd_aesthetics("geom", "path_trace")
#'
#' @seealso \link[ggplot2]{geom_path}; \link[ggplot2]{geom_line}; \link[ggplot2]{geom_step}
#' @return ggplot object
#'
#' @examples
#' # Modify line color for each group
#' ggplot2::ggplot(
#'   stocks,
#'   ggplot2::aes(day, value, fill = name)
#' ) +
#'   geom_line_trace() +
#'   ggplot2::theme_minimal()
#'
#' # Modify outline color for each group
#' ggplot2::ggplot(
#'   stocks,
#'   ggplot2::aes(day, value, color = name)
#' ) +
#'   geom_line_trace() +
#'   ggplot2::theme_minimal()
#'
#' # Specify outline color for each group
#' clrs <- c(
#'   CAC  = "#E69F00",
#'   DAX  = "#0072B2",
#'   FTSE = "#009E73",
#'   SMI  = "#56B4E9"
#' )
#'
#' ggplot2::ggplot(
#'   stocks,
#'   ggplot2::aes(day, value, color = name)
#' ) +
#'   geom_line_trace(stroke = 1) +
#'   ggplot2::scale_color_manual(values = clrs) +
#'   ggplot2::theme_minimal()
#'
#' # Outline a subset of data points
#' ggplot2::ggplot(
#'   stocks,
#'   ggplot2::aes(day, value, color = name)
#' ) +
#'   geom_line_trace(trace_position = day > 1500, stroke = 1) +
#'   ggplot2::theme_minimal()
#'
#' # Modify appearance of background data points
#' ggplot2::ggplot(
#'   stocks,
#'   ggplot2::aes(day, value, color = name)
#' ) +
#'   geom_line_trace(
#'     trace_position    = day > 1500,
#'     background_params = list(color = NA, fill = "grey75"),
#'     stroke            = 1
#'   ) +
#'   ggplot2::theme_minimal()
#'
#' # Remove outline
#' ggplot2::ggplot(
#'   stocks,
#'   ggplot2::aes(day, value, fill = name)
#' ) +
#'   geom_line_trace(
#'     trace_position    = day > 1500,
#'     background_params = list(fill = "grey75"),
#'     color             = NA
#'   ) +
#'   ggplot2::theme_minimal()
#'
#' @export
geom_path_trace <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ..., trace_position = "all",
                            background_params = list(color = NA),
                            lineend = "butt", linejoin = "round",
                            linemitre = 10, arrow = NULL, na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE) {

  if (substitute(trace_position) != "all") {
    mapping <- add_dummy_aes(mapping, KEEP_CLMN)
  }

  params <- list(
    lineend   = lineend,
    linejoin  = linejoin,
    linemitre = linemitre,
    arrow     = arrow,
    na.rm     = na.rm,
    ...
  )

  create_trace_layers(
    mapping           = mapping,
    data              = data,
    stat              = stat,
    geom              = GeomPathTrace,
    position          = position,
    show.legend       = show.legend,
    inherit.aes       = inherit.aes,
    params            = params,
    trace_position    = substitute(trace_position),
    background_params = background_params,
    trans_fn          = path_trans_fn,
    allow_bottom      = FALSE
  )
}

# To filter data when user passes a predicate to trace_position, a new column
# is added to mark rows to be highlighted. The column name is specified by
# KEEP_CLMN.
# To keep this column in the data, KEEP_CLMN must also be added to aes, and a
# value must be provided to default_aes().
KEEP_CLMN <- "KEEP_THIS_ROW_PLEASE"

# Helper to add dummy aes
add_dummy_aes <- function(mapping, nm) {
  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }

  mapping[[nm]] <- as.name(nm)

  mapping
}

# Function to use for transforming data when predicate is passed to
# trace_position
path_trans_fn <- function(dat, ex, inv = FALSE) {
  if (inv) {
    dat <- transform(dat, KEEP_THIS_ROW_PLEASE = !eval(ex))

  } else {
    dat <- transform(dat, KEEP_THIS_ROW_PLEASE = eval(ex))
  }

  names(dat)[names(dat) == "KEEP_THIS_ROW_PLEASE"] <- KEEP_CLMN

  dat
}

# Default aes for geom_path_trace geoms
# set this outside of ggproto since need to add KEEP_CLMN so this column
# is included for trace_position predicate
default_path_aes <- ggplot2::aes(
  colour   = "black",
  fill     = "black",
  size     = 0.5,
  stroke   = 0.5,
  linetype = 1,
  alpha    = NA
)

default_path_aes[[KEEP_CLMN]] <- TRUE

# Extra parameters to include for background points
extra_bkgd_params <- paste0("bkgd_", c(
  "colour",   "fill",  "size", "stroke",
  "linetype", "alpha", "layer"
))



#' GeomPathTrace
#'
#' @rdname ggtrace-ggproto
#' @format NULL
#' @usage NULL
#' @return ggproto object
#' @seealso \link[ggplot2]{GeomPath}
#' @export
GeomPathTrace <- ggproto(
  "GeomPathTrace", ggplot2::Geom,

  required_aes = c("x", "y"),

  default_aes = default_path_aes,

  extra_params = c(
    extra_bkgd_params,
    paste0("bkgd_", c("lineend", "linejoin", "linemitre", "arrow"))
  ),

  handle_na = function(data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    # do not include colour here so the user can choose to exclude the outline
    # by setting colour = NA
    drop_na_values <- function(dat, warn = TRUE, clmns = c("x", "y", "size", "fill", "stroke", "linetype")) {
      complete <- stats::complete.cases(dat[clmns])
      kept     <- stats::ave(complete, dat$group, FUN = keep_mid_true)
      dat      <- dat[kept, ]

      if (warn && !all(kept) && !params$na.rm) {
        warning("Removed ", sum(!kept), " row(s) containing missing values (geom_path_trace).")
      }

      dat
    }

    data <- drop_na_values(data)

    # Expand background line so one data point overlaps with highlighted segments
    # this is to eliminate breaks between background and highlighted lines
    # params$bkgd_layer is only set for background layer
    if (!is.null(params$bkgd_layer)) {

      expand_line <- function(grp) {
        dat <- subset(data, group == grp)

        if (!any(dat[[KEEP_CLMN]])) {
          return(dat)
        }

        keep_row <- dat[[KEEP_CLMN]]
        idx      <- seq_along(keep_row)
        idx      <- idx[keep_row]

        n_rows <- length(keep_row)

        seg_strts <- idx[c(TRUE, diff(idx) != 1)]
        seg_ends  <- idx[c(diff(idx) != 1, TRUE)]

        seg_strts[seg_strts > 1]    <- seg_strts[seg_strts > 1] - 1
        seg_ends[seg_ends < n_rows] <- seg_ends[seg_ends < n_rows] + 1

        expanded_idx <- lapply(seq_along(seg_strts), function(i) seg_strts[i]:seg_ends[i])
        expanded_idx <- Reduce(c, expanded_idx)

        dat[expanded_idx, KEEP_CLMN] <- TRUE

        dat
      }

      grps <- unique(data$group)
      data <- lapply(grps, expand_line)
      data <- Reduce(rbind, data)
    }

    # If KEEP_CLMN has been modified by user-provided predicate, add NAs to
    # create line breaks
    if (!is.null(data[[KEEP_CLMN]]) && !all(data[[KEEP_CLMN]])) {
      data[!data[[KEEP_CLMN]], "y"] <- NA

      data <- drop_na_values(data, warn = FALSE)
      data <- data[, colnames(data) != KEEP_CLMN]
    }

    data
  },

  setup_data = function(data, params) {

    if (!is.null(data[[KEEP_CLMN]])) {
      # If trace_position predicate does not select any data points, return
      # empty data for layer so it is not passed to draw_group()
      if (!any(data[[KEEP_CLMN]])) {
        data <- subset(data, eval(as.name(KEEP_CLMN)))

        return(data)
      }

      # Do not want KEEP_CLMN to influence groups since this column is only
      # needed to select data points to highlight. Need to re-adjust groups if
      # KEEP_CLMN has been modified by user-provided predicate
      if (!all(data[[KEEP_CLMN]])) {
        d <- data[, !colnames(data) %in% c(KEEP_CLMN, "group")]
        d <- add_group(d)

        data$group <- d$group
      }
    }

    # Want to adjust groups so lines with the same colour or fill do not have
    # overlapping outlines
    clmn <- c("colour", "fill")
    clmn <- clmn[clmn %in% colnames(data)]

    # Do not adjust groups if both colour and fill are specified
    # Do not adjust groups if groups are already unique for each colour/fill
    # Do not adjust groups if colour/fill is numeric
    if (length(clmn) == 1 && !is.numeric(data[[clmn]])) {
      uniq_grps <- unique(data[, c(clmn, "group")])

      if (anyDuplicated(uniq_grps[[clmn]])) {
        grps <- data$group
        d    <- data[, colnames(data) != "group"]
        d    <- add_group(d)

        data$group      <- d$group
        data$orig_group <- grps
      }
    }

    # Add new background data columns for background_params
    # should not overwrite the original columns since final parameters (colour,
    # fill, etc.) have not been set for groups yet
    bkgd_clmns       <- names(params)[grepl("^bkgd_", names(params))]
    data[bkgd_clmns] <- params[bkgd_clmns]

    # Must be sorted on group
    data <- data[order(data$group), , drop = FALSE]

    data
  },

  draw_group = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {

    if (!anyDuplicated(data$group)) {
      message("geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?")
    }

    # If background_params are present in data, override original columns
    bkgd_clmns <- colnames(data)[grepl("^bkgd_", colnames(data))]
    clmns      <- gsub("^bkgd_", "", bkgd_clmns)

    data[clmns] <- data[bkgd_clmns]

    # Munch data
    # this divides data into line segments to plot
    munched <- coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows    <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]

    if (nrow(munched) < 2) {
      return(zeroGrob())
    }

    # Set values for params
    # if params are not present in munched, use default value
    arrow     <- munched$arrow %||% arrow
    lineend   <- munched$lineend %||% lineend
    linejoin  <- munched$linejoin %||% linejoin
    linemitre <- munched$linemitre %||% linemitre

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
    end        <- c(group_diff, TRUE)

    if (!constant) {

      create_seg_grob <- function(clr, strk, lty) {
        grid::segmentsGrob(
          munched$x[!end],   munched$y[!end],
          munched$x[!start], munched$y[!start],

          default.units = "native",
          arrow         = arrow,

          gp = grid::gpar(
            col       = alpha(clr, munched$alpha)[!end],
            fill      = alpha(clr, munched$alpha)[!end],  # modifies arrow fill
            lwd       = munched$size[!end] * .pt + strk * .pt * 2,
            lty       = lty,
            lineend   = lineend,
            linejoin  = linejoin,
            linemitre = linemitre
          )
        )
      }

      trace_grob <- create_seg_grob(
        clr  = munched$colour,
        strk = munched$stroke[!end],
        lty  = 1
      )

      line_grob <- create_seg_grob(
        clr  = munched$fill,
        strk = 0,
        lty  = munched$linetype[!end]
      )

    } else {

      id <- match(munched$group, unique(munched$group))

      if ("orig_group" %in% colnames(munched)) {
        id <- match(munched$orig_group, unique(munched$orig_group))
      }

      create_line_grob <- function(clr, strk, lty) {
        grid::polylineGrob(
          munched$x, munched$y,

          id            = id,
          default.units = "native",
          arrow         = arrow,

          gp = grid::gpar(
            col       = alpha(clr, munched$alpha)[start],
            fill      = alpha(clr, munched$alpha)[start],  # modifies arrow fill
            lwd       = munched$size[start] * .pt + strk * .pt * 2,
            lty       = lty,
            lineend   = lineend,
            linejoin  = linejoin,
            linemitre = linemitre
          )
        )
      }

      trace_grob <- create_line_grob(
        clr  = munched$colour,
        strk = munched$stroke,
        lty  = 1
      )

      line_grob <- create_line_grob(
        clr  = munched$fill,
        strk = 0,
        lty  = munched$linetype[start]
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
geom_line_trace <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE,
                            orientation = NA, show.legend = NA,
                            inherit.aes = TRUE, trace_position = "all",
                            background_params = list(color = NA), ...) {

  if (substitute(trace_position) != "all") {
    mapping <- add_dummy_aes(mapping, KEEP_CLMN)
  }

  params <- list(
    orientation = orientation,
    na.rm       = na.rm,
    ...
  )

  create_trace_layers(
    mapping           = mapping,
    data              = data,
    stat              = stat,
    geom              = GeomLineTrace,
    position          = position,
    show.legend       = show.legend,
    inherit.aes       = inherit.aes,
    params            = params,
    trace_position    = substitute(trace_position),
    background_params = background_params,
    trans_fn          = path_trans_fn,
    allow_bottom      = FALSE
  )
}

#' @rdname ggtrace-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLineTrace <- ggproto(
  "GeomLineTrace", GeomPathTrace,

  extra_params = c(GeomPathTrace$extra_params, "na.rm", "orientation"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)

    params
  },

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes

    data <- data[order(data$PANEL, data$group, data$x), ]
    data <- GeomPathTrace$setup_data(data, params)

    data <- flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    data <- flip_data(data, params$flipped_aes)

    data
  }
)


#' @rdname geom_path_trace
#' @param direction direction of stairs: 'vh' for vertical then horizontal,
#'   'hv' for horizontal then vertical, or 'mid' for step half-way between
#'   adjacent x-values.
#' @export
geom_step_trace <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", direction = "hv",
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, trace_position = "all",
                            background_params = list(color = NA), ...) {

  if (substitute(trace_position) != "all") {
    mapping <- add_dummy_aes(mapping, KEEP_CLMN)
  }

  params <- list(
    direction = direction,
    na.rm     = na.rm,
    ...
  )

  create_trace_layers(
    mapping           = mapping,
    data              = data,
    stat              = stat,
    geom              = GeomStepTrace,
    position          = position,
    show.legend       = show.legend,
    inherit.aes       = inherit.aes,
    params            = params,
    trace_position    = substitute(trace_position),
    background_params = background_params,
    trans_fn          = path_trans_fn,
    allow_bottom      = FALSE
  )
}

#' @rdname ggtrace-ggproto
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
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))

  } else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))

  } else if (direction == "mid") {
    xs <- rep(1:(n - 1), each = 2)
    ys <- rep(1:n, each = 2)

  } else {
    stop("Parameter `direction` is invalid.")
  }

  if (direction == "mid") {
    gaps      <- data$x[-1] - data$x[-n]
    mid_x     <- data$x[-n] + gaps / 2                 # map the mid-point between adjacent x-values
    x         <- c(data$x[1], mid_x[xs], data$x[n])
    y         <- c(data$y[ys])
    data_attr <- data[c(1, xs, n), setdiff(names(data), c("x", "y"))]

  } else {
    x         <- data$x[xs]
    y         <- data$y[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "y"))]
  }

  new_data_frame(c(list(x = x, y = y), data_attr))
}
