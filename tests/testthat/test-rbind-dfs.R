# From ggplot2
test_that("rbind_dfs keep classes of columns", {
  df1 <- data_frame(
    integer   = seq_len(10),
    numeric   = as.numeric(seq_len(10)),
    character = letters[1:10],
    factor    = factor(letters[1:10]),
    ordered   = ordered(letters[1:10]),
    date      = Sys.Date()
  )

  df2 <- rbind_dfs(list(df1[1:5, ], df1[6:10, ]))
  expect_equal(df1, df2)
})
