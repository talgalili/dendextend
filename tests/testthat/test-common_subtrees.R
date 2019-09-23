# library(testthat)
suppressPackageStartupMessages(library(dendextend))

context("common subtrees")


test_that("rank_values_with_clusters works", {
  expect_equal(
    rank_values_with_clusters(c(1, 2, 3)),
    1:3
  )

  expect_equal(
    rank_values_with_clusters(c(1, 1, 3)),
    c(1L, 1L, 2L)
  )

  expect_equal(
    rank_values_with_clusters(c(0.1, 0.1, 3000)),
    c(1L, 1L, 2L)
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 2)),
    1:3
  )
  #
  expect_equal(
    rank_values_with_clusters(c(1, 3, 3, 3, 3, 3, 3, 4, 2, 2)),
    c(1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 4L, 4L)
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 2), ignore0 = TRUE),
    1:3
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 2), ignore0 = FALSE),
    1:3
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 0, 2), ignore0 = TRUE),
    c(1, 2, 0, 3)
  )

  expect_equal(
    rank_values_with_clusters(c(3, 1, 0, 2), ignore0 = FALSE),
    1:4
  )
})
