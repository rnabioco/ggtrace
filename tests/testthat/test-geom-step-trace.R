test_that("unexpected args throw warning", {
  expect_warning(ggplot() + geom_step_trace(blah = "blerg"))
})

test_that("specify aes params", {
  p <- ggplot(stocks, aes(day, value, color = name))

  p2 <- p + geom_step_trace(linetype = 5)
  expect_identical(p2$layers[[1]]$aes_params$linetype, 5)

  p2 <- p + geom_step_trace(color = "red")
  expect_identical(p2$layers[[1]]$aes_params$colour, "red")

  p2 <- p + geom_step_trace(colour = "red")
  expect_identical(p2$layers[[1]]$aes_params$colour, "red")

  p2 <- p + geom_step_trace(fill = "red")
  expect_identical(p2$layers[[1]]$aes_params$fill, "red")

  p2 <- p + geom_step_trace(alpha = 0.5)
  expect_identical(p2$layers[[1]]$aes_params$alpha, 0.5)
})

test_that("aesthetics to variable", {
  p <- ggplot(stocks, aes(day, value, color = name))

  p2 <- p + geom_step_trace()
  expect_identical(as.character(p2$mapping$colour)[[2]], "name")
})

test_that("aesthetics from geom", {
  p <- ggplot(stocks, aes(day, value, color = name))
  expect_identical(as.character(p$mapping$colour)[2], "name")

  p <- geom_step_trace(aes(fill = name))
  expect_identical(as.character(p[[1]]$mapping$fill)[2], "name")

  p <- geom_step_trace(aes(linetype = name))
  expect_identical(as.character(p[[1]]$mapping$linetype)[2], "name")

  p <- geom_step_trace(aes(alpha = name))
  expect_identical(as.character(p[[1]]$mapping$alpha)[2], "name")

  p <- geom_step_trace(aes(stroke = name))
  expect_identical(as.character(p[[1]]$mapping$stroke)[2], "name")
})

test_that("trace_position predicate return list", {
  p <- geom_step_trace(trace_position = day < 500 | day > 1500)

  expect_type(p, "list")
  expect_true(length(p) == 2)
  expect_identical(p[[1]]$geom_params$bkgd_colour, NA)
  expect_true(length(p[[2]]$aes_params) == 0)
})

test_that("trace_position predicate data", {
  p <- ggplot(stocks, aes(day, value, color = name)) +
    geom_step_trace(trace_position = day < 500 | day > 1500)

  expect_identical(p$layers[[2]]$data(stocks) %>% dplyr::filter(KEEP_THIS_ROW_PLEASE) %>% dplyr::select(-KEEP_THIS_ROW_PLEASE) %>% tibble::as_tibble(), subset(stocks, day < 500 | day > 1500))
})

test_that("trace_position pass data as data.frame", {
  dat <- subset(stocks, day < 500 | day > 1500)
  p   <- geom_step_trace(data = dat, trace_position = subset(stocks, day < 500 | day > 1500))

  expect_identical(p[[2]]$data(dat) %>% dplyr::select(day, name, value) %>% tibble::as_tibble(), subset(stocks, day < 500 | day > 1500))
})

test_that("trace_position pass data as function", {
  dat <- function(x) subset(x, day < 500 | day > 1500)
  p   <- geom_step_trace(data = dat, trace_position = day < 500 | day > 1500)

  expect_identical(p[[2]]$data(stocks) %>% dplyr::select(-KEEP_THIS_ROW_PLEASE) %>% tibble::as_tibble(), subset(stocks, day < 500 | day > 1500))
})

test_that("background_params color", {
  p <- ggplot(stocks, aes(day, value, color = name)) +
    geom_step_trace(
      trace_position    = day < 500 | day > 1500,
      background_params = list(color = NA, fill = "grey75")
    )

  expect_true(p$layers[[1]]$geom_params$bkgd_fill == "grey75")

  p <- ggplot(stocks, aes(day, value, color = name)) +
    geom_step_trace(trace_position = day < 500 | day > 1500)

  expect_true(is.na(p$layers[[1]]$geom_params$bkgd_colour))
})
