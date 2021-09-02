test_that("unexpected args throw warning", {
  expect_warning(ggplot() + geom_point_trace(blah = "blerg"))
})
