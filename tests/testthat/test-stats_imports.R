# library(testthat)
# library(dendextend)


context("stats_imports")


test_that("stats_.memberDend works", {
   x <- matrix(1:4)
   expect_identical(
      stats_.memberDend(x),
      1L
   )
})
