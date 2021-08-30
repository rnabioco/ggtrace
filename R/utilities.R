
#' Name ggplot grid object
#' Helper to name grid objects
#' https://github.com/tidyverse/ggplot2/blob/master/R/utilities-grid.r
#' @noRd
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

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

check_subclass <- function(x, subclass, argname = to_lower_ascii(subclass),
                           env = parent.frame()) {

  if (inherits(x, subclass)) {
    x

  } else if (is.character(x) && length(x) == 1) {
    name <- paste0(subclass, camelize(x, first = TRUE))
    obj  <- find_global(name, env = env)

    if (is.null(obj) || !inherits(obj, subclass)) {
      rlang::abort(glue::glue("Can't find `{argname}` called '{x}'"))

    } else {
      obj
    }

  } else {
    rlang::abort(glue::glue(
      "`{argname}` must be either a string or a {subclass} object, not {obj_desc(x)}"
    ))
  }
}

# Look for object first in parent environment and if not found, then in
# ggplot2 namespace environment.  This makes it possible to override default
# scales by setting them in the parent environment.
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

camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)

  if (first) {
    x <- firstUpper(x)
  }

  x
}

firstUpper <- function(s) {
  paste0(to_upper_ascii(substring(s, 1, 1)), substring(s, 2))
}

# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

binned_pal <- function(palette) {
  function(x) {
    palette(length(x))
  }
}

is.waive <- function(x) inherits(x, "waiver")
