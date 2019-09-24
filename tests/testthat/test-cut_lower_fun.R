# library(testthat)

context("cut_lower_fun works")

# library(dendextend)
# library(dendextendRcpp)

test_that("cut_lower_fun works", {
  dend <- datasets::iris[1:4, -5] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  # this is really cool!
  expect_identical(
    cut_lower_fun(dend, .4, labels),
    lapply(cut(dend, h = .4)$lower, labels)
  )

  expect_identical(
    cut_lower_fun(dend, 0, labels),
    lapply(cut(dend, h = 0)$lower, labels)
  )

  # this is the way to test this:
  #    expect_identical(
  #       old_cut_lower_fun(dend[[1]], .4, function(x)x),
  #       list(dend[[1]])
  #    )

  # function on a leaf gives what we expect
  expect_identical(
    cut_lower_fun(dend[[1]], .4, labels),
    list("1")
  )

  # cut, however, returns an empty list instead of itself...
  expect_identical(
    cut(dend[[1]], h = .4)$lower,
    list()
  )




  #    library(microbenchmark)
  #    microbenchmark(
  #       cut_lower_fun(dend, .4, order.dendrogram),
  #       lapply(cut(dend, h = .4)$lower, order.dendrogram)
  #    )
  #    # Rcpp is 3 times faster...
  #


  #    dend_big = as.dendrogram(hclust(dist(iris[1:150,-5])))
  #       microbenchmark(
  #          cut_lower_fun(dend_big, .4, order.dendrogram),
  #          lapply(cut(dend_big, h = .4)$lower, order.dendrogram)
  #       )
  # Rcpp is 16 times faster...
})





test_that("cut_lower_fun in dendextend", {
  dend <- as.dendrogram(hclust(dist(c(1, 1, 1, 2, 2))))
  expect_identical(
    cut_lower_fun(dend, -.5, labels),
    list()
  )
  # But this will NOT be the same with dendextendRcpp !!
})
