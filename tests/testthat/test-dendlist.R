# library(testthat)
# suppressPackageStartupMessages(library(dendextend))

context("dendlist")

test_that("dendlist works", {
  dend <- iris[, -5] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  dend2 <- iris[, -5] %>%
    dist() %>%
    hclust(method = "single") %>%
    as.dendrogram()

  expect_error(dendlist(1:4, 5, a = dend))

  expect_equal(dendlist(dend) %>% class(), "dendlist")

  # dendlist <- function (...) list(...)
  # check lengths
  expect_equal(dendlist(dend) %>% length(), 1L)
  expect_equal(dendlist(dend, dend) %>% length(), 2L)
  expect_equal(dendlist(dend, dend, dendlist(dend)) %>% length(), 3L)

  #  Checking that the piping works
  expect_identical(
    dendlist(dend, dend2),
    dendlist(dend) %>% dendlist(dend2)
  )

  # the dendlist of a dendlist should be a dendlist!
  expect_identical(
    dendlist(dend), dendlist(dendlist(dend))
  )
  #    dendextend:::all.equal.dendlist(dendlist(dend), dendlist(dendlist(dend)))
  # This fails?


  expect_identical(
    dendlist(dend, dend), dendlist(dendlist(dend, dend))
  )

  # dendlist does not handle weird stuff.
  expect_error(
    dendlist(unclass(dendlist(dend, dend)))
  )

  ### I removed the warning when creating an empty dendlist
  #    # but it handles empty lists just fine:
  #    expect_warning(
  #       dendlist()
  #    )
  #    expect_warning(
  #       dendlist(list())
  #    )
  #    # It can merge a dendlist of an empty list with a dend just fine:
  #    expect_warning(
  #       dendlist(dendlist(list()), dend)
  #    )

  # and the result is of length 1! (as we wanted it to)
  expect_equal(
    suppressWarnings(length(dendlist(dendlist(list()), dend))),
    1
  )


  # checks is.dendlist
  expect_true(suppressWarnings(is.dendlist(dendlist())))
  expect_true(is.dendlist(dendlist(dend)))
  
  # if hclust object passed in and/or 'which' parameter used
  hc <- iris[, -5] %>%
     dist() %>%
     hclust()
  result <- dendlist(dend, hc, dend, which = c(1,2))
  expect_true(is.dendlist(result))
})





test_that("all.equal.dendlist works", {
  dend <- iris[, -5] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()

  expect_true(
    all.equal(dendlist(dend, dend))
  )
  expect_true(
    all.equal(dendlist(dend, dend, dend))
  )

  expect_true(
    all.equal(dendlist(dend, dend), dendlist(dend, dend))
  )

  p_dend <- prune(dend, "1")
  expect_true(
    is.character(
      all.equal(dendlist(dend, p_dend))
    )
  )
})





test_that("plot.dendlist works", {
   dend <- iris[1:10, -5] %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   dend2 <- iris[1:10, -5] %>%
      dist() %>%
      hclust(method = "single") %>%
      as.dendrogram()
   
   dend12 <- dendlist(dend, dend2)
   expect_no_error(
      plot.dendlist(dend12)
   )
   dend12 <- dendlist(dend)
   expect_no_error(
      plot.dendlist(dend12)
   )
   
   # if nondendlist object passed in
   expect_error(
      plot.dendlist(dend)   
   )
   
})





test_that("as.dendlist works", {
   hc <- iris[1:10, -5] %>%
      dist() %>%
      hclust()
   dend <- as.dendrogram(hc)
   
   # automatically removes non-dend objects
   x <- list(1, hc, dend, list())
   expect_equal(
      length(as.dendlist(x)),
      2
   )
   
})





test_that("head.dendlist works", {
   dend <- iris[1:10, -5] %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   dend2 <- iris[1:10, -5] %>%
      dist() %>%
      hclust(method = "single") %>%
      as.dendrogram()
   dend12 <- dendlist(dend, dend2)
   
   capture.output(expect_no_error(
      head.dendlist(dend12)
   ))
})
