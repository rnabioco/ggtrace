#' Apply function to unique subsets of a data.frame
#'
#' This function is akin to `plyr::ddply`. It takes a single data.frame,
#' splits it by the unique combinations of the columns given in `by`, apply a
#' function to each split, and then reassembles the results into a single
#' data.frame again.
#'
#' https://github.com/tidyverse/ggplot2/blob/master/R/compat-plyr.R
#'
#' Used by GeomPointTrace
#'
#' @param df A data.frame
#' @param by A character vector of column names to split by
#' @param fun A function to apply to each split
#' @param ... Further arguments to `fun`
#' @param drop Should unused factor levels in the columns given in `by` be
#' dropped.
#'
#' @return A data.frame if the result of `fun` does not include the columns
#' given in `by` these will be prepended to the result.
#'
#' @noRd
dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols  <- .subset(df, by)
  fallback_order <- unique(c(by, names(df)))

  apply_fun <- function(x) {
    res <- fun(x, ...)

    if (is.null(res)) return(res)

    if (length(res) == 0) return(new_data_frame())

    vars <- lapply(stats::setNames(by, by), function(col) .subset2(x, col)[1])

    if (is.matrix(res)) res <- split_matrix(res)

    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))

    if (all(by %in% names(res))) return(new_data_frame(unclass(res)))

    res <- modify_list(unclass(vars), unclass(res))

    new_data_frame(res[intersect(c(fallback_order, names(res)), names(res))])
  }

  # Shortcut when only one group
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }

  ids        <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)

  rbind_dfs(lapply(seq_along(group_rows), function(i) {
    cur_data <- df_rows(df, group_rows[[i]])

    apply_fun(cur_data)
  }))
}

# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
# Used by dapply
# https://github.com/tidyverse/ggplot2/blob/master/R/compat-plyr.R
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    stop("Elements must be named")
  }

  lengths <- vapply(x, length, integer(1))

  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }

  for (i in seq_along(x)) {
    if (lengths[i] == n) next

    if (lengths[i] != 1) {
      stop("Elements must equal the number of rows or 1")
    }

    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

data_frame <- function(...) {
  new_data_frame(list(...))
}

# Used by dapply
# https://github.com/tidyverse/ggplot2/blob/master/R/utilities.r
split_with_index <- function(x, f, n = max(f)) {
  if (n == 1) return(list(x))
  f <- as.integer(f)
  attributes(f) <- list(levels = as.character(seq_len(n)), class = "factor")
  unname(split(x, f))
}

# Used by dapply
# https://github.com/tidyverse/ggplot2/blob/master/R/compat-plyr.R
single_value <- function(x, ...) {
  UseMethod("single_value")
}

#' @export
single_value.default <- function(x, ...) {

  # This is set by id() used in creating the grouping var
  identical(attr(x, "n"), 1L)
}

#' @export
single_value.factor <- function(x, ...) {

  # Panels are encoded as factor numbers and can never be missing (NA)
  identical(levels(x), "1")
}

# More performant modifyList without recursion
# Used by dapply
# https://github.com/tidyverse/ggplot2/blob/master/R/performance.R
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

# Info needed for rbind_dfs date/time handling
ggtrace_global <- new.env(parent = emptyenv())

#' @importFrom rlang on_load
#' @noRd
rlang::on_load({
  date <- Sys.Date()
  ggtrace_global$date_origin <- date - unclass(date)

  time <- Sys.time()
  ggtrace_global$time_origin <- time - unclass(time)
})

#' Bind data frames together by common column names
#'
#' This function is akin to `plyr::rbind.fill`, `dplyr::bind_rows`, and
#' `data.table::rbindlist`. It takes data frames in a list and stacks them on
#' top of each other, filling out values with `NA` if the column is missing from
#' a data.frame
#'
#' Used by dapply
#' https://github.com/tidyverse/ggplot2/blob/master/R/compat-plyr.R
#'
#' @param dfs A list of data frames
#'
#' @return A data.frame with the union of all columns from the data frames given
#' in `dfs`
#'
#' @noRd
rbind_dfs <- function(dfs) {
  out <- list()
  columns <- unique(unlist(lapply(dfs, names)))
  nrows <- vapply(dfs, .row_names_info, integer(1), type = 2L)
  total <- sum(nrows)

  if (length(columns) == 0) return(new_data_frame(list(), total))

  allocated <- rep(FALSE, length(columns))
  names(allocated) <- columns

  col_levels <- list()
  ord_levels <- list()

  for (df in dfs) {
    new_columns <- intersect(names(df), columns[!allocated])

    for (col in new_columns) {
      if (is.factor(df[[col]])) {
        all_ordered <- all(vapply(dfs, function(df) {
          val <- .subset2(df, col)
          is.null(val) || is.ordered(val)
        }, logical(1)))

        all_factors <- all(vapply(dfs, function(df) {
          val <- .subset2(df, col)
          is.null(val) || is.factor(val)
        }, logical(1)))

        if (all_ordered) {
          ord_levels[[col]] <- unique(unlist(lapply(dfs, function(df) levels(.subset2(df, col)))))

        } else if (all_factors) {
          col_levels[[col]] <- unique(unlist(lapply(dfs, function(df) levels(.subset2(df, col)))))
        }

        out[[col]] <- rep(NA_character_, total)

      } else {
        out[[col]] <- rep(.subset2(df, col)[1][NA], total)
      }
    }

    allocated[new_columns] <- TRUE

    if (all(allocated)) break
  }

  is_date <- lapply(out, inherits, "Date")
  is_time <- lapply(out, inherits, "POSIXct")
  pos <- c(cumsum(nrows) - nrows + 1)

  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    rng <- seq(pos[i], length.out = nrows[i])

    for (col in names(df)) {
      date_col <- inherits(df[[col]], "Date")
      time_col <- inherits(df[[col]], "POSIXct")

      if (is_date[[col]] && !date_col) {
        out[[col]][rng] <- as.Date(
          unclass(df[[col]]),
          origin = ggtrace_global$date_origin
        )

      } else if (is_time[[col]] && !time_col) {
        out[[col]][rng] <- as.POSIXct(
          unclass(df[[col]]),
          origin = ggtrace_global$time_origin
        )

      } else if (date_col || time_col || inherits(df[[col]], "factor")) {
        out[[col]][rng] <- as.character(df[[col]])

      } else {
        out[[col]][rng] <- df[[col]]
      }
    }
  }

  for (col in names(ord_levels)) {
    out[[col]] <- ordered(out[[col]], levels = ord_levels[[col]])
  }

  for (col in names(col_levels)) {
    out[[col]] <- factor(out[[col]], levels = col_levels[[col]])
  }

  attributes(out) <- list(
    class = "data.frame",
    names = names(out),
    row.names = .set_row_names(total)
  )
  out
}

# Used by dapply
# https://github.com/tidyverse/ggplot2/blob/master/R/performance.R
split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names)) names(x) <- col_names
  x
}

# Used by rbind_dfs
# https://github.com/tidyverse/ggplot2/blob/master/R/performance.R
df_rows <- function(x, i) {
  new_data_frame(lapply(x, `[`, i = i))
}

# Helpers used to add info for geom documentation
# https://github.com/tidyverse/ggplot2/blob/master/R/utilities-help.r
rd_aesthetics <- function(type, name) {
  obj <- switch(
    type,
    geom = check_subclass(name, "Geom", env = globalenv()),
    stat = check_subclass(name, "Stat", env = globalenv())
  )

  aes <- rd_aesthetics_item(obj)
  aes <- aes[aes != paste0("\\code{", KEEP_CLMN, "}")]

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
      stop("Can't find `", argname, "` called '", x, "'")

    } else {
      obj
    }

  } else {
    stop("`", argname, "` must be either a string or a ", subclass, " object.")
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

  nsenv <- asNamespace("ggtrace")

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

  if (first) x <- firstUpper(x)

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

# Used by geom_path_trace
# https://github.com/tidyverse/ggplot2/blob/master/R/utilities.r
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
