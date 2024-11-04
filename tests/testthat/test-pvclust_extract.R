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
   
   # add rect dendrogram object to highlight consistent clusters
   suppressWarnings(RNGversion("3.5.0"))
   library(pvclust)
   set.seed(123)
   capture.output(suppressWarnings(dend <- mtcars[1:5, ] %>% pvclust()))
   
   plot(as.dendrogram(dend))
   expect_no_error(
      pvrect2(dend, type = "geq")
   )
   expect_no_error(
      pvrect2(dend, type = "leq")
   )
   expect_no_error(
      pvrect2(dend, type = "gt")
   )
   expect_no_error(
      pvrect2(dend, type = "lt")
   )
   expect_error(
      pvrect2(dend, type = NA)
   )
})


test_that("pvclust_show_signif works", {
   
   # bold branches to highlight consistent clusters
   suppressWarnings(RNGversion("3.5.0"))
   library(pvclust)
   data(lung) # 916 genes for 73 subjects
   set.seed(13134)
   capture.output(result <- pvclust(lung[, 1:20], method.dist = "cor", method.hclust = "average", nboot = 100))
   dend <- as.dendrogram(result)
   
   expect_no_error(
      pvclust_show_signif(dend, result) %>% plot() 
   )
     
})


test_that("pvclust_show_signif_gradient works", {
   
   # color branches to highlight consistent clusters
   suppressWarnings(RNGversion("3.5.0"))
   library(pvclust)
   data(lung) # 916 genes for 73 subjects
   set.seed(13134)
   capture.output(result <- pvclust(lung[, 1:20], method.dist = "cor", method.hclust = "average", nboot = 100))
   dend <- as.dendrogram(result)
   
   expect_no_error(
      pvclust_show_signif_gradient(dend, result) %>% plot() 
   )
   
})


test_that("strwidth2 works", {
   
   # test that adjusted string widths are numeric
   suppressWarnings(RNGversion("3.5.0"))
   library(pvclust)
   set.seed(123)
   capture.output(suppressWarnings(dend <- mtcars[1:5, ] %>% pvclust()))
   
   expect_true(
      is.numeric(strwidth2(dend))
   )
   
})


test_that("as.hclust.pvclust works", {
   
   library(pvclust)
   set.seed(123)
   capture.output(suppressWarnings(dend <- mtcars[1:5, ] %>% pvclust()))
   result <- as.hclust.pvclust(dend)
   
   # should convert pvclust to hclust object
   expect_identical(
      attributes(result)$class,
      "hclust"
   )
   
})


test_that("as.dendrogram.pvclust works", {
   
   library(pvclust)
   set.seed(123)
   capture.output(suppressWarnings(dend <- mtcars[1:5, ] %>% pvclust()))
   result <- as.dendrogram.pvclust(dend)
   
   # should convert pvclust to hclust object
   expect_identical(
      attributes(result)$class,
      "dendrogram"
   )
   
})
