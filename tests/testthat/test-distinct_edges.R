# cat("\n")
# library(testthat)


context("Distinct edges")


test_that("partition_leaves works", {
  #    library(magrittr)

  x <- 1:3 %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  #    partition_leaves(x)
  #    plot(x)
  expect_identical(partition_leaves(x), list(c(3L, 1L, 2L), 3L, 1:2, 1L, 2L))
  
  # check case where something other than a dendrogram is passed in
  x <- matrix(1:4, nrow=2)
  expect_error(partition_leaves(x))
})



test_that("distinct_edges works", {
  x <- 1:5 %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  y <- set(x, "labels", 5:1)
  expect_identical(distinct_edges(x, y), c(5L, 7L))
  expect_identical(distinct_edges(y, x), c(2L, 5L))
})



test_that("dist.dendlist work", {
  x <- 1:5 %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  y <- set(x, "labels", 5:1)

  #    dend_diff(x,y)
  tmp <- dist.dendlist(dendlist(x, x, y))

  expect_identical(class(tmp), "dist")

  #    dput(as.matrix(tmp))
  expect_identical(
    as.matrix(tmp),
    structure(c(0, 0, 4, 0, 0, 4, 4, 4, 0), .Dim = c(3L, 3L), .Dimnames = list(
      c("1", "2", "3"), c("1", "2", "3")
    ))
  )
  
  # check case where something other than a dendrogram is passed in
  x <- matrix(1:4, nrow=2)
  expect_error(dist.dendlist(x))
})



test_that("highlight_distinct_edges.dendrogram works", {
   dend1 <- as.dendrogram(hclust(dist(iris[1:5, -5]), method = "average"))
   dend2 <- as.dendrogram(hclust(dist(iris[2:6, -5]), method = "average"))
   
   # check that edge color was indeed changed to identify which edges differ
   result <- highlight_distinct_edges.dendrogram(dend1, dend2, value = 3, edgePar = "col")
   expect_identical(attributes(result)$edgePar$col, 3)
})



test_that("highlight_distinct_edges.dendlist works", {
   dend1 <- as.dendrogram(hclust(dist(iris[1:5, -5]), method = "average"))
   dend2 <- as.dendrogram(hclust(dist(iris[2:6, -5]), method = "average"))
   dend_list <- dendlist(dend1, dend2)
   
   # check that edge color was indeed changed to identify which edges differ for both inserted dendrograms
   result <- highlight_distinct_edges.dendlist(dend_list, value = 3, edgePar = "col")
   expect_identical(attributes(result[[1]])$edgePar$col, 3)
   expect_identical(attributes(result[[2]])$edgePar$col, 3)
})



test_that("dend_diff.dendrogram works", {
   dend1 <- as.dendrogram(hclust(dist(iris[1:5, -5]), method = "average"))
   dend2 <- as.dendrogram(hclust(dist(iris[2:6, -5]), method = "average"))
   
   # check that edge color was indeed changed to identify which edges differ
   pdf(file = NULL)
   capture.output(suppressWarnings(
      result <- dend_diff.dendrogram(dend1, dend2, value = 3, edgePar = "col")
   ))
   dev.off()
   expect_identical(result, dendlist(dend1, dend2))
})



test_that("dend_diff.dendlist works", {
   dend1 <- as.dendrogram(hclust(dist(iris[1:5, -5]), method = "average"))
   dend2 <- as.dendrogram(hclust(dist(iris[2:6, -5]), method = "average"))
   dend_list <- dendlist(dend1, dend2)
   
   # check that edge color was indeed changed to identify which edges differ
   pdf(file = NULL)
   capture.output(suppressWarnings(
      result <- dend_diff.dendlist(dend_list, value = 3, edgePar = "col")
   ))
   dev.off()
   expect_identical(result, dendlist(dend1, dend2))
})
