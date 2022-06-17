test_that("unexpected args throw warning", {
  expect_warning(ggplot() + geom_point_trace(blah = "blerg"))
})

test_that("specify aes params", {
  p <- ggplot(clusters, aes(UMAP_1, UMAP_2))

  p2 <- p + geom_point_trace(linetype = 5)
  expect_identical(p2$layers[[1]]$aes_params$linetype, 5)

  p2 <- p + geom_point_trace(color = "red")
  expect_identical(p2$layers[[1]]$aes_params$colour, "red")

  p2 <- p + geom_point_trace(colour = "red")
  expect_identical(p2$layers[[1]]$aes_params$colour, "red")

  p2 <- p + geom_point_trace(fill = "red")
  expect_identical(p2$layers[[1]]$aes_params$fill, "red")

  p2 <- p + geom_point_trace(alpha = 0.5)
  expect_identical(p2$layers[[1]]$aes_params$alpha, 0.5)
})

test_that("aesthetics to variable", {
  p <- ggplot(clusters, aes(UMAP_1, UMAP_2, color = cluster))

  p2 <- p + geom_point_trace()
  expect_identical(as.character(p2$mapping$colour)[[2]], "cluster")
})

test_that("aesthetics from geom", {
  p <- geom_point_trace(aes(color = cluster))
  expect_identical(as.character(p[[1]]$mapping$colour)[2], "cluster")

  p <- geom_point_trace(aes(fill = cluster))
  expect_identical(as.character(p[[1]]$mapping$fill)[2], "cluster")

  p <- geom_point_trace(aes(linetype = cluster))
  expect_identical(as.character(p[[1]]$mapping$linetype)[2], "cluster")

  p <- geom_point_trace(aes(alpha = cluster))
  expect_identical(as.character(p[[1]]$mapping$alpha)[2], "cluster")

  p <- geom_point_trace(aes(stroke = cluster))
  expect_identical(as.character(p[[1]]$mapping$stroke)[2], "cluster")
})

test_that("trace_position bottom", {
  p <- geom_point_trace(trace_position = "bottom")

  expect_identical(as.character(p[[1]]$mapping$group)[2], "BOTTOM_TRACE_GROUP")
  expect_doppelganger("trace_position bottom", p)
})

test_that("trace_position predicate return list", {
  p <- geom_point_trace(trace_position = signal > 10)

  expect_type(p, "list")
  expect_true(length(p) == 2)
  expect_identical(p[[1]]$geom_params$bkgd_colour, NA)
  expect_true(length(p[[2]]$aes_params) == 0)
  expect_doppelganger("trace_position predicate return list", p)
})

test_that("trace_position predicate data", {
  p <- ggplot(clusters, aes(UMAP_1, UMAP_2)) +
    geom_point_trace(trace_position = signal > 10)

  expect_identical(p$layers[[2]]$data(clusters), subset(clusters, signal > 10))
  expect_doppelganger("trace_position predicate data", p)
})

test_that("trace_position pass data as data.frame", {
  dat <- subset(clusters, signal > 10)
  p   <- geom_point_trace(data = dat, trace_position = signal > 10)

  expect_identical(p[[2]]$data(clusters), subset(clusters, signal > 10))
  expect_doppelganger("trace_position pass data as data.frame", p)
})

test_that("trace_position pass data as function", {
  dat <- function(x) subset(x, signal > 10)
  p   <- geom_point_trace(data = dat, trace_position = signal > 10)

  expect_identical(p[[2]]$data(clusters), subset(clusters, signal > 10))
  expect_doppelganger("trace_position pass data as function", p)
})

test_that("background_params color", {
  p <- ggplot(clusters, aes(UMAP_1, UMAP_2)) +
    geom_point_trace(
      trace_position = signal > 10,
      background_params = list(fill = "blue")
    )

  expect_true(p$layers[[1]]$geom_params$bkgd_fill == "blue")
  expect_doppelganger("background_params color 1", p)

  p <- ggplot(clusters, aes(UMAP_1, UMAP_2)) +
    geom_point_trace(trace_position = signal > 10)

  expect_true(is.na(p$layers[[1]]$geom_params$bkgd_colour))
  expect_doppelganger("background_params color 2", p)
})

test_that("single strings translate to their corresponding integers", {
  expect_equal(translate_shape_string("square open"), 0)
})

test_that("vectors of strings translate to corresponding integers", {
  shape_strings <- c(
    "square open",
    "circle open",
    "square open",
    "triangle open"
  )

  expect_equal(translate_shape_string(shape_strings), c(0, 1, 0, 2))
})

test_that("single characters are not translated to integers", {
  expect_equal(translate_shape_string(letters), letters)
  expect_equal(translate_shape_string(as.character(0:9)), as.character(0:9))
})

test_that("invalid shape names raise an error", {
  expect_error(translate_shape_string("void"), "Can't find shape name")
  expect_error(translate_shape_string("tri"), "Shape names must be unambiguous")
  expect_error(translate_shape_string(paste0("bad", 1:8)), "more problems")
  expect_error(translate_shape_string(c("tri", "tr", "cir", "ci", "squa", "squ", "sq")), "more problems")
})

test_that("correct shape translation", {
  s <- translate_trace_shape(c("15", "16", "17", "19"))
  expect_identical(s, c(0, 1, 2, 1))

  s <- translate_trace_shape(as.character(0:14))
  expect_identical(s, as.double(0:14))
})

test_that("bad shape", {
  expect_error(translate_trace_shape("18"), "Unsupported shape")
  expect_error(translate_trace_shape("20"), "Unsupported shape")
  expect_error(translate_trace_shape("21"), "Unsupported shape")
  expect_error(translate_trace_shape("22"), "Unsupported shape")
  expect_error(translate_trace_shape("23"), "Unsupported shape")
  expect_error(translate_trace_shape("24"), "Unsupported shape")
  expect_error(translate_trace_shape("25"), "Unsupported shape")
})

test_that("calculate_trace_size", {
  dat <- data.frame(
    shape  = 0,
    size   = 1,
    stroke = 1
  )

  sz  <- dat$size * .pt + 0.5 * .stroke / 2
  lwd <- dat$stroke * .stroke / 2 * 2 + (0.5 * .stroke / 2)
  expect_identical(calculate_trace_size(dat)$trace_fontsize, sz)
  expect_identical(calculate_trace_size(dat)$trace_lwd, lwd)

  dat <- data.frame(
    shape  = 19,
    size   = 1,
    stroke = 1
  )

  sz  <- dat$size * .pt + 0.5 * .stroke / 2 + dat$stroke * .stroke / 2
  lwd <- dat$stroke * .stroke / 2
  expect_identical(calculate_trace_size(dat)$trace_fontsize, sz)
  expect_identical(calculate_trace_size(dat)$trace_lwd, lwd)
})

test_that("bad background_params", {
  expect_error(
    ggplot(clusters, aes(UMAP_1, UMAP_2, color = cluster)) +
      geom_point_trace(background_params = "BAD")
  )
})
