test_that("unexpected args throw warning", {
  expect_warning(ggplot() + geom_line_trace(blah = "blerg"))
})

test_that("specify aes params", {
  p <- ggplot(stocks, aes(day, value, color = name))

  p2 <- p + geom_line_trace(linetype = 5)
  expect_identical(p2$layers[[1]]$aes_params$linetype, 5)
  expect_doppelganger("specify aes params 1", p2)

  p2 <- p + geom_line_trace(color = "red")
  expect_identical(p2$layers[[1]]$aes_params$colour, "red")
  expect_doppelganger("specify aes params 2", p2)

  p2 <- p + geom_line_trace(colour = "red")
  expect_identical(p2$layers[[1]]$aes_params$colour, "red")
  expect_doppelganger("specify aes params 3", p2)

  p2 <- p + geom_line_trace(fill = "red")
  expect_identical(p2$layers[[1]]$aes_params$fill, "red")
  expect_doppelganger("specify aes params 4", p2)

  p2 <- p + geom_line_trace(alpha = 0.5)
  expect_identical(p2$layers[[1]]$aes_params$alpha, 0.5)
  expect_doppelganger("specify aes params 5", p2)
})

test_that("aesthetics to variable", {
  p <- ggplot(stocks, aes(day, value, color = name))

  p2 <- p + geom_line_trace()
  expect_identical(as.character(p2$mapping$colour)[[2]], "name")
  expect_doppelganger("aesthetics to variable", p2)
})

test_that("aesthetics from geom", {
  p <- ggplot(stocks, aes(day, value, color = name))
  expect_identical(as.character(p$mapping$colour)[2], "name")
  expect_doppelganger("aesthetics from geom 1", p)

  p <- geom_line_trace(aes(fill = name))
  expect_identical(as.character(p[[1]]$mapping$fill)[2], "name")
  expect_doppelganger("aesthetics from geom 2", p)

  p <- geom_line_trace(aes(linetype = name))
  expect_identical(as.character(p[[1]]$mapping$linetype)[2], "name")
  expect_doppelganger("aesthetics from geom 3", p)

  p <- geom_line_trace(aes(alpha = name))
  expect_identical(as.character(p[[1]]$mapping$alpha)[2], "name")
  expect_doppelganger("aesthetics from geom 4", p)

  p <- geom_line_trace(aes(stroke = name))
  expect_identical(as.character(p[[1]]$mapping$stroke)[2], "name")
  expect_doppelganger("aesthetics from geom 5", p)
})

test_that("trace_position predicate return list", {
  p <- geom_line_trace(trace_position = day < 500 | day > 1500)

  expect_type(p, "list")
  expect_true(length(p) == 2)
  expect_identical(p[[1]]$geom_params$bkgd_colour, NA)
  expect_true(length(p[[2]]$aes_params) == 0)
})

test_that("trace_position predicate data", {
  p <- ggplot(stocks, aes(day, value, color = name)) +
    geom_line_trace(trace_position = day < 500 | day > 1500)

  df1 <- p$layers[[2]]$data(stocks) %>%
    dplyr::filter(KEEP_THIS_ROW_PLEASE) %>%
    dplyr::select(-KEEP_THIS_ROW_PLEASE) %>%
    tibble::as_tibble()

  df2 <- subset(stocks, day < 500 | day > 1500)

  expect_identical(df1, df2)
})

test_that("trace_position pass data as data.frame", {
  dat <- subset(stocks, day < 500 | day > 1500)
  p   <- geom_line_trace(data = dat, trace_position = subset(stocks, day < 500 | day > 1500))

  df1 <- p[[2]]$data(dat) %>%
    dplyr::select(day, name, value) %>%
    tibble::as_tibble()

  df2 <- subset(stocks, day < 500 | day > 1500)

  expect_identical(df1, df2)
})

test_that("trace_position pass data as function", {
  dat <- function(x) subset(x, day < 500 | day > 1500)
  p   <- geom_line_trace(data = dat, trace_position = day < 500 | day > 1500)

  df1 <- p[[2]]$data(stocks) %>%
    dplyr::select(-KEEP_THIS_ROW_PLEASE) %>%
    tibble::as_tibble()

  df2 <- subset(stocks, day < 500 | day > 1500)

  expect_identical(df1, df2)
})

test_that("background_params color", {
  p <- ggplot(stocks, aes(day, value, color = name)) +
    geom_line_trace(
      trace_position    = day < 500 | day > 1500,
      background_params = list(color = NA, fill = "grey75")
    )

  expect_true(p$layers[[1]]$geom_params$bkgd_fill == "grey75")
  expect_doppelganger("background_params color 1", p)

  p <- ggplot(stocks, aes(day, value, color = name)) +
    geom_line_trace(trace_position = day < 500 | day > 1500)

  expect_true(is.na(p$layers[[1]]$geom_params$bkgd_colour))
  expect_doppelganger("background_params color 2", p)
})
