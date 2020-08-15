context("Test extract au")

test_that("extract au", {
  suppressWarnings(RNGversion("3.5.0"))
  library(pvclust)
  set.seed(123)
  suppressWarnings(dend <- USArrests[1:5, ] %>% pvclust())
  # plot(dend)
  # pvclust_edges(dend)
  expect_identical(as.character(pvclust_edges(dend)[, 1]), c("Assault", "UrbanPop", "Murder"))
  expect_identical(as.character(pvclust_edges(dend)[, 2]), c("Rape", "1", "2"))
  # expect_identical(round(pvclust_edges(dend)[, 3], digits = 2), c(0.97, 1.00, 1.00)) # there is a stochastic element to the outcome so while it works using test_that it may be causing error with travis-ci checks
})
