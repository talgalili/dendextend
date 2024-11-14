# cat("\n")
# library(testthat)


context("onLoad")


test_that("onLoad works", {
   # no options for dendextend yet, should be empty
   dendextend:::.onLoad()
   expect_identical(getOption("dendextend"), NULL)
})


test_that("dendextend_options works", {
   
   opt <- dendextend_options()
   expect_true(
      !opt$warn
   )

})