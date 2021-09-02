context("test-scale-colour-continuous.R")

test_that("type argument is checked for proper input", {
  expect_error(
    scale_trace_colour_continuous(type = function() "abc"),
    "is not a scale function"
  )
  expect_error(
    scale_trace_colour_binned(type = function(...) scale_colour_binned(aesthetics = c("fill", "point_colour"))),
    "works with the following aesthetics: fill, point_colour"
  )
})
