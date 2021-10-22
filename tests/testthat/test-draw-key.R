test_that("draw_key_*_trace", {
  p <- ggplot(clusters, aes(UMAP_1, UMAP_2, color = cluster))

  expect_doppelganger("no shape", p + geom_point_trace(key_glyph = draw_key_point_trace))
  expect_doppelganger("numeric shape", p + geom_point_trace(shape = 3, key_glyph = draw_key_point_trace))
  expect_doppelganger("character shape", p + geom_point_trace(shape = "plus", key_glyph = draw_key_point_trace))

  expect_doppelganger("no linetype", p + geom_point_trace(key_glyph = draw_key_path_trace))
  expect_doppelganger("linetype", p + geom_point_trace(key_glyph = draw_key_path_trace, linetype = 2))
})
