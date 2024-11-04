context("Test extract au")

test_that("extract au", {
  suppressWarnings(RNGversion("3.5.0"))
  library(pvclust)
  set.seed(123)
  capture.output(suppressWarnings(dend <- USArrests[1:5, ] %>% pvclust()))
  # plot(dend)
  # pvclust_edges(dend)
  expect_identical(as.character(pvclust_edges(dend)[, 1]), c("Assault", "UrbanPop", "Murder"))
  expect_identical(as.character(pvclust_edges(dend)[, 2]), c("Rape", "1", "2"))
  # expect_identical(round(pvclust_edges(dend)[, 3], digits = 2), c(0.97, 1.00, 1.00)) # there is a stochastic element to the outcome so while it works using test_that it may be causing error with travis-ci checks
})


test_that("na_locf works", {
   
   # replace NA values with value prior to it
   x <- c(NA,1,2,NA,4,NA,6)
   result <- na_locf(x)
   expect_identical(result, c(0,1,2,2,4,4,6))
   
   result <- na_locf(x, recursive = F)
   expect_identical(result, c(0,1,2,2,4,4,6))
   
})


test_that("text.pvclust works", {
   
   # add same text to dendrogram object as normally comes with pvclust
   suppressWarnings(RNGversion("3.5.0"))
   library(pvclust)
   set.seed(123)
   capture.output(suppressWarnings(dend <- mtcars[1:5, ] %>% pvclust()))
   
   plot(as.dendrogram(dend))
   expect_no_error(
      text.pvclust(dend)
   )

})


test_that("pvrect2 works", {
   
   # add same text to dendrogram object as normally comes with pvclust
   suppressWarnings(RNGversion("3.5.0"))
   library(pvclust)
   set.seed(123)
   capture.output(suppressWarnings(dend <- mtcars[1:5, ] %>% pvclust()))
   
   plot(as.dendrogram(dend))
   expect_no_error(
      pvrect2(dend)
   )
})
