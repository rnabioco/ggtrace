# By default, if vdiffr is not installed, all visual tests are skipped unless
# VDIFFR_RUN_TESTS is explicitly set to "true", which should be the case only on
# a GitHub Actions CI runner with stable version of R.
# From ggplot2

if (requireNamespace("vdiffr", quietly = TRUE) && utils::packageVersion("testthat") >= "3.0.3") {
  expect_doppelganger <- vdiffr::expect_doppelganger

  # If vdiffr is not available and visual tests are explicitly required, raise error
} else {
  if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) {
    stop("vdiffr is not installed")
  }

  # Otherwise, assign a dummy function
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}
