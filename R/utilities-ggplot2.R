#' Base ggproto classes for ggplot2
#'
#' If you are creating a new geom, stat, position, or scale in another package,
#' you'll need to extend from `ggplot2::Geom`, `ggplot2::Stat`,
#' `ggplot2::Position`, or `ggplot2::Scale`.
#'
#' @seealso ggproto
#' @keywords internal
#' @name ggplot2-ggproto
NULL

# Helpers used to add info for geom documentation
# https://github.com/tidyverse/ggplot2/blob/master/R/utilities-help.r
rd_aesthetics <- function(type, name) {
  obj <- switch(
    type,
    geom = check_subclass(name, "Geom", env = globalenv()),
    stat = check_subclass(name, "Stat", env = globalenv())
  )

  aes <- rd_aesthetics_item(obj)

  res <- c(
    "@section Aesthetics:",
    paste0(
      "\\code{", type, "_", name, "()} ",
      "understands the following aesthetics (required aesthetics are in bold):"
    ),
    "\\itemize{",
    paste0("  \\item ", aes),
    "}",
    "Learn more about setting these aesthetics in \\code{vignette(\"ggplot2-specs\")}."
  )

  res
}

rd_aesthetics_item <- function(x) {
  req          <- x$required_aes
  req          <- sub("|", "} \\emph{or} \\code{", req, fixed = TRUE)
  req_aes      <- unlist(strsplit(x$required_aes, "|", fixed = TRUE))
  optional_aes <- setdiff(x$aesthetics(), req_aes)
  all          <- union(req, sort(optional_aes))

  ifelse(
    all %in% req,
    paste0("\\strong{\\code{", all, "}}"),
    paste0("\\code{", all, "}")
  )
}

# Used by rd_aesthetics
# https://github.com/tidyverse/ggplot2/blob/master/R/layer.r
check_subclass <- function(x, subclass, argname = to_lower_ascii(subclass),
                           env = parent.frame()) {

  if (inherits(x, subclass)) {
    x

  } else if (is.character(x) && length(x) == 1) {
    name <- paste0(subclass, camelize(x, first = TRUE))
    obj  <- find_global(name, env = env)

    if (is.null(obj) || !inherits(obj, subclass)) {
      stop(glue::glue("Can't find `{argname}` called '{x}'"))

    } else {
      obj
    }

  } else {
    stop(glue::glue(
      "`{argname}` must be either a string or a {subclass} object, not {obj_desc(x)}"
    ))
  }
}

# Look for object first in parent environment and if not found, then in
# ggplot2 namespace environment. This makes it possible to override default
# scales by setting them in the parent environment.
# Used by check_subclass
# https://github.com/tidyverse/ggplot2/blob/master/R/scale-type.R
find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}

# Convert to camel case
# Used by check_subclass
# https://github.com/tidyverse/ggplot2/blob/master/R/layer.r
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)

  if (first) {
    x <- firstUpper(x)
  }

  x
}

# Used by camelize
# https://github.com/tidyverse/ggplot2/blob/master/R/utilities.r
firstUpper <- function(s) {
  paste0(to_upper_ascii(substring(s, 1, 1)), substring(s, 2))
}

# Used by camelize and check_subclass
# https://github.com/tidyverse/ggplot2/blob/master/R/utilities.r
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)
