# library(testthat)
# library(dendextend)


context("colored_dots")


test_that("zero_range works", {
   
   x <- c(1)
   expect_true(
      zero_range(x)
   )
   x <- c(1, 2, 3)
   expect_error(
      zero_range(x)
   )   
   x <- c(1, NA)
   expect_true(
      is.na(zero_range(x))
   )   
   x <- c(1, 1)
   expect_true(
      zero_range(x)
   )   
   x <- c(-Inf, Inf)
   expect_false(
      zero_range(x)
   )   
   x <- c(0, 1)
   expect_false(
      zero_range(x)
   )   
   x <- c(1,2)
   expect_false(
      zero_range(x)
   )   
   
})


test_that("rescale works", {
   
   x <- c(1, 2)
   expect_identical(
      rescale(x),
      c(0, 1)
   )
   x <- c(1, 1)
   expect_identical(
      rescale(x),
      c(0.5, 0.5)
   )
   
})
