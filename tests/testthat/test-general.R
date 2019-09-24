# library(testthat)
# library(dendextend)

context("General functions")


test_that("all.unique works", {
  expect_true(all_unique(c(1:10)))
  expect_true(all_unique(c(1, 3, 2)))
  expect_false(all_unique(c(1, 1, 2)))
  expect_false(all_unique(c(1:5, 1, 1)))
})
