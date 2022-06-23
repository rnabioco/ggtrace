#' Trace points
#'
#' This geom is similar to \code{ggplot2::geom_point()}, but also includes the
#' ability to outline points of interest. \code{geom_point_trace()} accepts
#' normal ggplot2 graphical parameters with some modifications. \code{fill}
#' controls the color of each point, \code{color} controls the outline
#' color, and \code{stroke} controls outline width, similar to how filled
#' shapes are modified for other ggplot2 geoms. Additional parameters including
#' \code{size}, \code{linetype}, and \code{alpha} are also accepted.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @param trace_position Specifies which data points to outline, can be one of:
#' \itemize{
#'   \item "all" to outline every group plotted
#'   \item "bottom" to only outline the bottom layer of data points
#'   \item A predicate specifying which data points to outline. This must
#'         evaluate to \code{TRUE} or \code{FALSE} within the context of the
#'         input data. e.g. \code{value > 100}
#' }
#'
#' @param background_params Named list specifying aesthetic parameters to use
#'    for background data points when a predicate is passed to
#'    \code{trace_position}, e.g. \code{list(color = "red")}
#'
#' @eval rd_aesthetics("geom", "point_trace")
#'
#' @seealso \link[ggplot2]{geom_point}
#' @return ggplot object
#'
#' @examples
#' # Modify outline color for each group
#' ggplot2::ggplot(
#'   clusters,
#'   ggplot2::aes(UMAP_1, UMAP_2, color = cluster)
#' ) +
#'   geom_point_trace() +
#'   ggplot2::theme_minimal()
#'
#' # Outline a subset of points
#' ggplot2::ggplot(
#'   clusters,
#'   ggplot2::aes(UMAP_1, UMAP_2, fill = cluster)
#' ) +
#'   geom_point_trace(trace_position = signal < 0 | signal > 17) +
#'   ggplot2::theme_minimal()
#'
#' # Modify appearance of background points
#' ggplot2::ggplot(
#'   clusters,
#'   ggplot2::aes(UMAP_1, UMAP_2, fill = cluster)
#' ) +
#'   geom_point_trace(
#'     trace_position    = signal < 0 | signal > 17,
#'     background_params = list(color = NA, fill = "grey85")
#'   ) +
#'   ggplot2::theme_minimal()
#'
#' @export
geom_point_trace <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", ...,
                             trace_position = "all",
                             background_params = list(color = NA),
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE) {

  trans_fn <- function(dat, ex, inv = FALSE) {
    if (inv) {
      return(subset(dat, !eval(ex)))
    }

    subset(dat, eval(ex))
  }

  create_trace_layers(
    mapping           = mapping,
    data              = data,
    stat              = stat,
    geom              = GeomPointTrace,
    position          = position,
    show.legend       = show.legend,
    inherit.aes       = inherit.aes,
    params            = list(na.rm = na.rm, ...),
    trace_position    = substitute(trace_position),
    background_params = background_params,
    trans_fn          = trans_fn,
    allow_bottom      = TRUE
  )
}


#' Create geom_*_trace layers
#'
#' @inheritParams geom_point_trace
#' @param geom The geometric object to use to display the data.
#' @param params Additional parameters to pass to the geom and stat.
#' @param trans_fn Function to use for transforming data when predicate is
#' passed to trace_position. Must accept three arguments: dat, data to transform;
#' ex, expression to use for transforming data; inv, should the expression be
#' negated, this allows the inverse data points to be transformed.
#' @param allow_bottom Should 'bottom' be allowed as an argument for trace_position?
#' @noRd
create_trace_layers <- function(mapping, data, stat, geom, position,
                                show.legend, inherit.aes, params,
                                trace_position, background_params, trans_fn,
                                allow_bottom = TRUE) {

  if (!is.list(background_params)) {
    stop(
      "background_params must be a named list with additional parameters to ",
      "use for modifying background points."
    )
  }

  trace_expr <- trace_position
  lyrs       <- list()

  # If trace_position is 'bottom', create new column and use to override
  # original group specification.
  if (allow_bottom && trace_expr == "bottom") {
    data <- ggplot2::fortify(~ transform(.x, BOTTOM_TRACE_GROUP = "bottom"))

    if (is.null(mapping)) {
      mapping <- ggplot2::aes()
    }

    mapping$group <- as.name("BOTTOM_TRACE_GROUP")

    # If trace_position is not 'all', evaluate expression
  } else if (trace_expr != "all") {
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

      data      <- ggplot2::fortify(~ trans_fn(d_fn(...), trace_expr))
      bkgd_data <- ggplot2::fortify(~ trans_fn(d_fn(...), trace_expr, inv = TRUE))

    } else if (is.data.frame(data) || is.null(data)) {
      data      <- ggplot2::fortify(~ trans_fn(.x, trace_expr))
      bkgd_data <- ggplot2::fortify(~ trans_fn(.x, trace_expr, inv = TRUE))
    }

    # Adjust parameters for background points
    bkgd_params <- params

    if (length(background_params) > 0) {
      bkgd_params$bkgd_layer <- TRUE

      names(background_params) <- paste0("bkgd_", names(background_params))

      bkgd_params[names(background_params)] <- background_params
    }

    bkgd_lyr <- layer(
      data        = bkgd_data,
      mapping     = mapping,
      stat        = stat,
      geom        = geom,
      position    = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params      = bkgd_params
    )

    lyrs <- append(lyrs, list(bkgd_lyr))
  }

  # Create trace layer
  trace_lyr <- layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = params
  )

  lyrs <- append(lyrs, list(trace_lyr))

  lyrs
}


#' @rdname ggtrace-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointTrace <- ggplot2::ggproto(
  "GeomPointTrace", ggplot2::Geom,

  required_aes = c("x", "y"),

  non_missing_aes = c("size", "shape", "fill"),

  default_aes = ggplot2::aes(
    shape    = 19,
    colour   = "black",
    fill     = "black",
    size     = 1.5,
    stroke   = 1,
    linetype = 1,
    alpha    = NA
  ),

  # WISH THESE COULD BE AUTOMATICALLY DETERMINED BASED ON self$default_aes
  # paste0("bkgd_", names(self$default_aes))
  extra_params = c(extra_bkgd_params, "bkgd_shape"),

  setup_data = function(data, params) {
    # Add background new data columns for background_params
    # should not override the original columns since final parameters (colour,
    # fill, etc.) have not been set for groups yet
    bkgd_clmns       <- names(params)[grepl("^bkgd_", names(params))]
    data[bkgd_clmns] <- params[bkgd_clmns]

    data
  },

  draw_group = function(self, data, panel_params, coord, na.rm = FALSE) {

    # If background_params are present in data, override original columns
    bkgd_clmns <- colnames(data)[grepl("^bkgd_", colnames(data))]
    clmns      <- gsub("^bkgd_", "", bkgd_clmns)

    data[clmns] <- data[bkgd_clmns]

    # Set point shape
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }

    data$trace_shape <- translate_trace_shape(data$shape)
    data             <- calculate_trace_size(data)

    coords <- coord$transform(data, panel_params)

    trace_grob <- grid::pointsGrob(
      coords$x, coords$y,
      pch = coords$trace_shape,

      gp = grid::gpar(
        col      = alpha(coords$colour, 1),
        lty      = coords$linetype,
        fontsize = coords$trace_fontsize,
        lwd      = coords$trace_lwd
      )
    )

    pt_stroke <- 0.5

    points_grob <- grid::pointsGrob(
      coords$x, coords$y,
      pch = coords$shape,

      gp = grid::gpar(
        col      = alpha(coords$fill, coords$alpha),
        fontsize = coords$size * .pt + pt_stroke * .stroke / 2,
        lwd      = pt_stroke * .stroke / 2
      )
    )

    ggname("geom_point_trace", grid::grobTree(trace_grob, points_grob))
  },

  draw_key = draw_key_point_trace
)


#' Helper to translate shape strings
#' https://github.com/tidyverse/ggplot2/blob/master/R/geom-point.r
#' @noRd
translate_shape_string <- function(shape_string) {

  # strings of length 0 or 1 are interpreted as symbols by grid
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }

  pch_tbl <- c(
    "square open"          = 0,
    "circle open"          = 1,
    "triangle open"        = 2,
    "plus"                 = 3,
    "cross"                = 4,
    "diamond open"         = 5,
    "triangle down open"   = 6,
    "square cross"         = 7,
    "asterisk"             = 8,
    "diamond plus"         = 9,
    "circle plus"          = 10,
    "star"                 = 11,
    "square plus"          = 12,
    "circle cross"         = 13,
    "square triangle"      = 14,
    "triangle square"      = 14,
    "square"               = 15,
    "circle small"         = 16,
    "triangle"             = 17,
    "diamond"              = 18,
    "circle"               = 19,
    "bullet"               = 20,
    "circle filled"        = 21,
    "square filled"        = 22,
    "diamond filled"       = 23,
    "triangle filled"      = 24,
    "triangle down filled" = 25
  )

  shape_match <- charmatch(shape_string, names(pch_tbl))

  invalid_strings   <- is.na(shape_match)
  nonunique_strings <- shape_match == 0

  if (any(invalid_strings)) {
    bad_string <- unique(shape_string[invalid_strings])
    n_bad      <- length(bad_string)

    collapsed_names <- sprintf("\n* %s", bad_string)

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))

    } else {
      ""
    }

    stop("Can't find shape name:", collapsed_names, more_problems)
  }

  if (any(nonunique_strings)) {
    bad_string <- unique(shape_string[nonunique_strings])
    n_bad      <- length(bad_string)

    n_matches <- vapply(
      bad_string[1:min(5, n_bad)],
      function(shape_string) sum(grepl(paste0("^", shape_string), names(pch_tbl))),
      integer(1)
    )

    collapsed_names <- sprintf(
      "\n* '%s' partially matches %d shape names",
      bad_string[1:min(5, n_bad)], n_matches
    )

    collapsed_names <- paste0(collapsed_names, collapse = "")

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))

    } else {
      ""
    }

    stop(paste0("Shape names must be unambiguous: ", collapsed_names, more_problems))
  }

  unname(pch_tbl[shape_match])
}

#' Helper to adjust trace size
#'
#' Adjust fontsize and lwd depending on whether an open or or closed shape is
#' used. This is need to allow for the inside and outside of open shapes to be
#' outlined.
#'
#' @noRd
calculate_trace_size <- function(data) {
  pch_open  <- 0:14
  pt_stroke <- 0.5
  pch       <- data$shape

  # Calculate fontsize for closed shapes
  fontsize  <- data$size * .pt + pt_stroke * .stroke / 2

  fontsize[!pch %in% pch_open] <- fontsize[!pch %in% pch_open] + data$stroke * .stroke / 2

  # Calculate lwd for open shapes
  lwd <- data$stroke * .stroke / 2

  lwd[pch %in% pch_open] <- lwd[pch %in% pch_open] * 2 + (pt_stroke * .stroke / 2)

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
    "14" = 14,     # "triangle square, square triangle"
    "15" = 0,      # "square"
    "16" = 1,      # "circle small"
    "17" = 2,      # "triangle"
    "19" = 1       # "circle"

    # Exclude shapes that are filled
    # Exclude diamond and bullet since they do not have an open shape of the
    # same size
    # "18" = 18,     # "diamond"
    # "20" = 20      # "bullet"
    # "21" = 21,     # "circle filled"
    # "22" = 22,     # "square filled"
    # "23" = 23,     # "diamond filled"
    # "24" = 24,     # "triangle filled"
    # "25" = 25      # "triangle down filled"
  )

  pch_match <- charmatch(pch, names(pch_tbl))
  bad_pch   <- is.na(pch_match)

  if (any(bad_pch)) {

    bad_pch <- unique(pch[bad_pch])

    stop("Unsupported shape ", paste0(bad_pch, collapse = ", "))
  }

  res <- unname(pch_tbl[pch_match])

  res
}

#' Helper to name grid objects
#' @noRd
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)

  grob
}
