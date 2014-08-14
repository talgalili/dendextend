# library(testthat)

context("General functions")


test_that("all.unique works",{

   expect_true(all.unique(c(1:10)))
   expect_true(all.unique(c(1,3,2)))
   expect_false(all.unique(c(1,1,2)))
   expect_false(all.unique(c(1:5, 1,1)))
   
})

