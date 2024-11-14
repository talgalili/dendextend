# library(testthat)

context("highlight_branches")


test_that("highlight_branches can be run", {
  dat <- iris[1:3, -5]
  hca <- hclust(dist(dat))
  dend <- as.dendrogram(hca)
  dend2 <- dend %>% highlight_branches()
  # plot(dend2)

  # dput(dend2)
  dend_expected <- structure(list(
    structure(1L, members = 1L, height = 0, label = "1", leaf = TRUE, edgePar = structure(list(
      col = "#BBDF27FF", lwd = 1
    ), .Names = c("col", "lwd"))),
    structure(list(
      structure(2L, label = "2", members = 1L, height = 0, leaf = TRUE, edgePar = structure(list(
        col = "#BBDF27FF", lwd = 1
      ), .Names = c("col", "lwd"))),
      structure(3L, label = "3", members = 1L, height = 0, leaf = TRUE, edgePar = structure(list(
        col = "#BBDF27FF", lwd = 1
      ), .Names = c("col", "lwd")))
    ), members = 2L, midpoint = 0.5, height = 0.3, edgePar = structure(list(
      col = "#2A788EFF", lwd = 6.01801801801802
    ), .Names = c(
      "col",
      "lwd"
    )))
  ), members = 3L, midpoint = 0.75, height = 0.53851648071345, edgePar = structure(list(
    col = "#440154FF", lwd = 10
  ), .Names = c("col", "lwd")), class = "dendrogram")


  expect_equal(dend2, dend_expected)
  # expect_identical(dend2, dend_expected)
  
  # if non dendrogram object is passed in
  expect_error(
     highlight_branches(hca)   
  )
  
})
