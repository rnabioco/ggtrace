test_that("unexpected args throw warning", {
  expect_warning(ggplot() + geom_path_trace(blah = "blerg"))
})

test_that("specify aes params", {
  p <- ggplot(stocks, aes(day, value, color = name))

  p2 <- p + geom_path_trace(linetype = 5)
  expect_identical(p2$layers[[1]]$aes_params$linetype, 5)

  p2 <- p + geom_path_trace(color = "red")
  expect_identical(p2$layers[[1]]$aes_params$colour, "red")

  p2 <- p + geom_path_trace(colour = "red")
  expect_identical(p2$layers[[1]]$aes_params$colour, "red")

  p2 <- p + geom_path_trace(fill = "red")
  expect_identical(p2$layers[[1]]$aes_params$fill, "red")

  p2 <- p + geom_path_trace(alpha = 0.5)
  expect_identical(p2$layers[[1]]$aes_params$alpha, 0.5)
})

test_that("aesthetics to variable", {
  p <- ggplot(stocks, aes(day, value, color = name))

  p2 <- p + geom_path_trace()
  expect_identical(as.character(p2$mapping$colour)[[2]], "name")
})

test_that("aesthetics from geom", {
  p <- ggplot(stocks, aes(day, value, color = name))
  expect_identical(as.character(p$mapping$colour)[2], "name")

  p <- geom_path_trace(aes(fill = name))
  expect_identical(as.character(p[[1]]$mapping$fill)[2], "name")

  p <- geom_path_trace(aes(linetype = name))
  expect_identical(as.character(p[[1]]$mapping$linetype)[2], "name")

  p <- geom_path_trace(aes(alpha = name))
  expect_identical(as.character(p[[1]]$mapping$alpha)[2], "name")

  p <- geom_path_trace(aes(stroke = name))
  expect_identical(as.character(p[[1]]$mapping$stroke)[2], "name")
})

test_that("trace_position predicate return list", {
  p <- geom_path_trace(trace_position = day < 500 | day > 1500)

  expect_type(p, "list")
  expect_true(length(p) == 2)
  expect_identical(p[[1]]$geom_params$bkgd_colour, NA)
  expect_true(length(p[[2]]$aes_params) == 0)
})

test_that("trace_position predicate data", {
  p <- ggplot(stocks, aes(day, value, color = name)) +
    geom_path_trace(trace_position = day < 500 | day > 1500)

  expect_identical(p$layers[[2]]$data(stocks) %>% dplyr::filter(KEEP_THIS_ROW_PLEASE) %>% dplyr::select(-KEEP_THIS_ROW_PLEASE) %>% tibble::as_tibble(), subset(stocks, day < 500 | day > 1500))
})

test_that("trace_position pass data as data.frame", {
  dat <- subset(stocks, day < 500 | day > 1500)
  p   <- geom_path_trace(data = dat, trace_position = subset(stocks, day < 500 | day > 1500))

  expect_identical(p[[2]]$data(dat) %>% dplyr::select(day, name, value) %>% tibble::as_tibble(), subset(stocks, day < 500 | day > 1500))
})

test_that("trace_position pass data as function", {
  dat <- function(x) subset(x, day < 500 | day > 1500)
  p   <- geom_path_trace(data = dat, trace_position = day < 500 | day > 1500)

  expect_identical(p[[2]]$data(stocks) %>% dplyr::select(-KEEP_THIS_ROW_PLEASE) %>% tibble::as_tibble(), subset(stocks, day < 500 | day > 1500))
})

test_that("background_params color", {
  p <- ggplot(stocks, aes(day, value, color = name)) +
    geom_path_trace(
      trace_position    = day < 500 | day > 1500,
      background_params = list(color = NA, fill = "grey75")
    )

  expect_true(p$layers[[1]]$geom_params$bkgd_fill == "grey75")

  p <- ggplot(stocks, aes(day, value, color = name)) +
    geom_path_trace(trace_position = day < 500 | day > 1500)

  expect_true(is.na(p$layers[[1]]$geom_params$bkgd_colour))
})

test_that("geom_path regroup order", {
  lvls <- c("SMI", "CAC", "DAX", "FTSE")
  dat  <- stocks

  dat$name <- factor(dat$name, lvls)

  p <- ggplot(dat, aes(day, value, fill = name)) +
    geom_path_trace()

  expect_doppelganger("geom_path group reorder trace_position", p)
})

test_that("keep_mid_true drops leading/trailing FALSE", {
  expect_equal(keep_mid_true(c(F, F)), c(F, F))
  expect_equal(keep_mid_true(c(F, T, F, T, F)), c(F, T, T, T, F))
  expect_equal(keep_mid_true(c(T, T, F, T, F)), c(T, T, T, T, F))
  expect_equal(keep_mid_true(c(F, T, F, T, T)), c(F, T, T, T, T))
})

test_that("stairstep() does not error with too few observations", {
  df <- data_frame(x = 1, y = 1)
  expect_silent(stairstep(df))
})

test_that("stairstep() exists with error when an invalid `direction` is given", {
  df <- data_frame(x = 1:3, y = 1:3)
  expect_error(stairstep(df, direction = "invalid"))
})

test_that("stairstep() output is correct for direction = 'vh'", {
  df <- data_frame(x = 1:3, y = 1:3)
  stepped_expected <- data_frame(x = c(1L, 1L, 2L, 2L, 3L), y = c(1L, 2L, 2L, 3L, 3L))
  stepped <- stairstep(df, direction = "vh")
  expect_equal(stepped, stepped_expected)
})

test_that("stairstep() output is correct for direction = 'hv'", {
  df <- data_frame(x = 1:3, y = 1:3)
  stepped_expected <- data_frame(x = c(1L, 2L, 2L, 3L, 3L), y = c(1L, 1L, 2L, 2L, 3L))
  stepped <- stairstep(df, direction = "hv")
  expect_equal(stepped, stepped_expected)
})

test_that("stairstep() output is correct for direction = 'mid'", {
  df <- data_frame(x = 1:3, y = 1:3)
  stepped_expected <- data_frame(x = c(1, 1.5, 1.5, 2.5, 2.5, 3), y = c(1L, 1L, 2L, 2L, 3L, 3L))
  stepped <- stairstep(df, direction = "mid")
  expect_equal(stepped, stepped_expected)
})

# test_that("geom_path group reorder trace_position", {
#   lvls <- c("SMI", "CAC", "DAX", "FTSE")
#   dat  <- stocks
#
#   dat$name <- factor(dat$name, lvls)
#
#   p <- ggplot(dat, aes(day, value, fill = name)) +
#     geom_path_trace(
#       trace_position = day > 500
#     )
#
#   expect_doppelganger("geom_path group reorder trace_position", p)
# })
