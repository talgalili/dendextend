# cat("\n")
# library(testthat)


context("onLoad")


test_that("onLoad works", {
   # testing to see if test-zzz files interferes with codecov
   expect_identical(1,1)
   
   # dendextend:::.onLoad()
   # expect_identical(getOption("dendextend"), NULL)
})

