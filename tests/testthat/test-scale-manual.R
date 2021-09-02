test_that("names of values used in manual scales", {
   s <- scale_trace_colour_manual(values = c("8" = "c","4" = "a","6" = "b"))
   s$train(c("4", "6", "8"))
   expect_equal(s$map(c("4", "6", "8")), c("a", "b", "c"))
})

test_that("missing values are replaced with na.value", {
  df <- data.frame(x = 1, y = 1:3, z = factor(c(1:2, NA), exclude = NULL))

  p  <- ggplot(df, aes(x, y, trace_colour = z)) +
    geom_point_trace() +
    scale_trace_colour_manual(values = c("black", "black"), na.value = "red")

  expect_equal(layer_data(p)$trace_colour, c("black", "black", "red"))
})

test_that("insufficient values raise an error", {
  df <- data.frame(x = 1, y = 1:3, z = factor(c(1:2, NA), exclude = NULL))

  p  <- ggplot(aes(x, y, trace_colour = z), data = df) +
    geom_point_trace()

  expect_error(ggplot_build(p + scale_trace_colour_manual(values = "black")),
    "Insufficient values")

  # Should be sufficient
  ggplot_build(p + scale_trace_colour_manual(values = c("black", "black")))
})

test_that("values are matched when scale contains more unique values than are in the data", {
  s <- scale_trace_colour_manual(values = c("8" = "c", "4" = "a",
    "22" = "d", "6"  = "b"))
  s$train(c("4", "6", "8"))
  expect_equal(s$map(c("4", "6", "8")), c("a", "b", "c"))
})

test_that("generic scale can be used in place of aesthetic-specific scales", {
  df <- data.frame(x = letters[1:3], y = LETTERS[1:3], z = factor(c(1, 2, 3)))

  p1 <- ggplot(df, aes(z, z, shape = x, trace_color = y, trace_alpha = z)) +
    scale_shape_manual(values = 1:3) +
    scale_trace_colour_manual(values = c("red", "green", "blue")) +
    scale_trace_alpha_manual(values = c(0.2, 0.4, 0.6))

  p2 <- ggplot(df, aes(z, z, shape = x, trace_color = y, trace_alpha = z)) +
    scale_discrete_manual(aesthetics = "shape", values = 1:3) +
    scale_discrete_manual(aesthetics = "trace_colour", values = c("red", "green", "blue")) +
    scale_discrete_manual(aesthetics = "trace_alpha", values = c(0.2, 0.4, 0.6))

  expect_equal(layer_data(p1), layer_data(p2))
})

test_that("named values do not match with breaks in manual scales", {
  s <- scale_fill_manual(
    values = c("data_red" = "red", "data_black" = "black"),
    breaks = c("data_black", "data_red")
  )
  s$train(c("data_black", "data_red"))
  expect_equal(s$map(c("data_red", "data_black")), c("red", "black"))
})

test_that("unnamed values match breaks in manual scales", {
  s <- scale_fill_manual(
    values = c("red", "black"),
    breaks = c("data_red", "data_black")
  )
  s$train(c("data_red", "data_black"))
  expect_equal(s$map(c("data_red", "data_black")), c("red", "black"))
})

test_that("limits works (#3262)", {
  # named charachter vector
  s1 <- scale_trace_colour_manual(values = c("8" = "c", "4" = "a", "6" = "b"), limits = c("4", "8"), na.value = NA)
  s1$train(c("4", "6", "8"))
  expect_equal(s1$map(c("4", "6", "8")), c("a", NA, "c"))

  # named charachter vector
  s2 <- scale_trace_colour_manual(values = c("c", "a", "b"), limits = c("4", "8"), na.value = NA)
  s2$train(c("4", "6", "8"))
  expect_equal(s2$map(c("4", "6", "8")), c("c", NA, "a"))
})

