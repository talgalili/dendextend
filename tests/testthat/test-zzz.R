# cat("\n")
# library(testthat)


context("onLoad")


test_that("onLoad works", {
   # no options for dendextend yet, should be empty
   dendextend:::.onLoad()
   expect_identical(getOption("dendextend"), NULL)
   
   # only execute for sake of coverage
   if (testthat:::in_covr()){
      unloadNamespace("dendextend")
      loadNamespace("dendextend")
      library(dendextend)
   }

})


test_that("dendextend_options works", {
   
   opt <- dendextend_options()
   expect_true(
      !opt$warn
   )

})
