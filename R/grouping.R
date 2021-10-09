# This needs to be less than 1, to distinguish it from "regular" return values
# of plyr::id() used by add_group()
NO_GROUP <- -1L

id <- function(.variables, drop = FALSE) {
  nrows <- NULL

  if (is.data.frame(.variables)) {
    nrows      <- nrow(.variables)
    .variables <- unclass(.variables)
  }

  lengths    <- vapply(.variables, length, integer(1))
  .variables <- .variables[lengths != 0]

  if (length(.variables) == 0) {
    n  <- nrows %||% 0L
    id <- seq_len(n)

    attr(id, "n") <- n

    return(id)
  }

  if (length(.variables) == 1) {
    return(id_var(.variables[[1]], drop = drop))
  }

  ids       <- rev(lapply(.variables, id_var, drop = drop))
  p         <- length(ids)
  ndistinct <- vapply(ids, attr, "n", FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  n         <- prod(ndistinct)

  if (n > 2^31) {
    char_id <- do.call("paste", c(ids, sep = "\r"))
    res     <- match(char_id, unique(char_id))

  } else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat   <- do.call("cbind", ids)
    res   <- c((mat - 1L) %*% combs + 1L)
  }

  if (drop) {
    id_var(res, drop = TRUE)

  } else {
    res <- as.integer(res)
    attr(res, "n") <- n
    res
  }
}

id_var <- function(x, drop = FALSE) {

  if (length(x) == 0) {
    id <- integer()
    n  <- 0L

  } else if (!is.null(attr(x, "n")) && !drop) {
    return(x)

  } else if (is.factor(x) && !drop) {
    x  <- addNA(x, ifany = TRUE)
    id <- as.integer(x)
    n  <- length(levels(x))

  } else {
    levels <- sort(unique(x), na.last = TRUE)
    id     <- match(x, levels)
    n      <- max(id)
  }

  attr(id, "n") <- n

  id
}

# Ensure that the data frame contains a grouping variable.
#
# If the `group` variable is not present, then a new group
# variable is generated from the interaction of all discrete (factor or
# character) vectors, excluding `label`. The special value `NO_GROUP`
# is used for all observations if no discrete variables exist.
add_group <- function(data) {
  if (empty(data)) return(data)

  if (is.null(data[["group"]])) {
    disc <- vapply(data, is.discrete, logical(1))

    disc[names(disc) %in% c("label", "PANEL")] <- FALSE

    if (any(disc)) {
      data$group <- id(data[disc], drop = TRUE)

    } else {
      data$group <- NO_GROUP
      attr(data$group, "n") <- 1L
    }

  } else {
    data$group <- id(data["group"], drop = TRUE)
  }

  data
}

# Is a grouping available?
# (Will return TRUE if an explicit group or a discrete variable with only one
# level existed when add_group() was called.)
has_groups <- function(data) {

  # If no group aesthetic is specified, all values of the group column equal to
  # NO_GROUP. On the other hand, if a group aesthetic is specified, all values
  # are different from NO_GROUP (since they are a result of plyr::id()). NA is
  # returned for 0-row data frames.
  data$group[1L] != NO_GROUP
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}

is.waive <- function(x) inherits(x, "waiver")

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}
