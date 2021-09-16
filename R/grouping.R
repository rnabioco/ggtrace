# This needs to be less than 1, to distinguish it from "regular" return values
# of plyr::id() used by add_group()
NO_GROUP <- -1L

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
      data$group <- vctrs::vec_group_id(data[disc])

    } else {
      data$group <- NO_GROUP
      attr(data$group, "n") <- 1L
    }

  } else {
    data$group <- vctrs::vec_group_id(data["group"])
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