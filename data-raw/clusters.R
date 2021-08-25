set.seed(1111)

n_clst <- 10

dat <- list(
  m_1  = sample(-5:20, n_clst),
  m_2  = sample(-5:20, n_clst),
  clst = str_c("c", 1:n_clst),
  x    = sample(100:2000, n_clst),
  sd   = sample(1:5, n_clst, replace = TRUE)
)

clusters <- dat %>%
  pmap_dfr(~ {
    l <- list(...)

    tibble(
      cluster = l$clst,
      sample  = sample(c("sample_1", "sample_2"), l$x, replace = TRUE),
      UMAP_1  = rnorm(l$x, l$m_1, l$sd),
      UMAP_2  = rnorm(l$x, l$m_2, l$sd),
      signal  = rnorm(l$x, l$m_1, 1)
    )
  })

usethis::use_data(clusters)
