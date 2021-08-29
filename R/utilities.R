
#' Name ggplot grid object
#' Helper to name grid objects
#' https://github.com/tidyverse/ggplot2/blob/master/R/utilities-grid.r
#' @noRd
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

is.waive <- function(x) inherits(x, "waiver")
