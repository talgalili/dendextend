# cat("\n")
# library(testthat)


context("onLoad")


test_that("onLoad works", {
   # no options for dendextend yet, should be empty
   dendextend:::.onLoad()
   expect_identical(getOption("dendextend"), NULL)
   
   dendextend:::assign_dendextend_options()
   dendextend:::remove_dendextend_options()
})

