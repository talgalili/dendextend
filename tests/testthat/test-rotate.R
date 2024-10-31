# library(testthat)

context("Rotate a tree around its hinges")


test_that("Rotate a dendrogram", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  #    labels(dend)
  #    rev(labels(dend))

  expect_equal(rev(labels(dend)), labels(rotate(dend, 3:1)))
  expect_equal(rev(labels(dend)), labels(rotate(dend, rev(labels(dend)))))
  
  # test case where an object other than dendrogram/hclust/phylo is passed in
  x <- matrix(1:4, nrow = 2)
  expect_error(rotate(x))
  
  # test case where order argument is missing
  expect_warning(
     rotated_dend <- rotate(dend)
  )
  expect_identical(rotated_dend, dend)
  
  # test case where not all leaves are specified in order and order is not numeric
  expect_error(rotate(dend, c("Alaska", "Alabama")))
})


test_that("Rotate a dendrogram (but not exactly to what we asked for)", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  new_order <- c(3, 1, 2) # an impossible order, will fail
  expect_false(all(labels(dend)[new_order] == labels(rotate(dend, new_order))))
  expect_false(all(labels(dend)[new_order] == labels(rotate(dend, labels(dend)[new_order]))))
  #    plot(rotate(dend, new_order))
  #    plot(dend)
})



test_that("Rotate a dendrogram - works with either numeric or character", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  new_order <- c(2, 3, 1) # a possible order

  expect_equivalent(rotate(dend, new_order), rotate(dend, labels(dend)[new_order]))
  #    plot(rotate(dend, new_order))
  #    plot(rotate(dend, labels(dend)[new_order]) )
  #    match(labels(dend)[new_order], labels(dend)) # NEW-OLD
  #    new_order
  expect_equivalent(
    labels(rotate(dend, new_order)),
    labels(rotate(dend, labels(dend)[new_order]))
  )
})







test_that("Rotate hclust", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")

  #    labels(hc)
  #    rev(labels(hc))
  #    plot(hc)
  #    plot(rotate(hc, 3:1))

  expect_equal(rev(labels(hc)), labels(rotate(hc, 3:1)))
  expect_equal(rev(labels(hc)), labels(rotate(hc, rev(labels(hc)))))
})


test_that("Rotate a hclust (but not exactly to what we asked for)", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")

  new_order <- c(3, 1, 2) # an impossible order, will fail
  expect_false(all(labels(hc)[new_order] == labels(rotate(hc, new_order))))
  expect_false(all(labels(hc)[new_order] == labels(rotate(hc, labels(hc)[new_order]))))
  #    plot(rotate(hc, new_order))
  #    plot(hc)
})


test_that("rotate.phylo works", {
   hc <- hclust(dist(USArrests[1:3, ]), "ave")
   dend <- as.dendrogram(hc)
   
   x = ape::as.phylo(dend)
   rotated_x = rotate.phylo(x, c("Alaska", "Alabama"))
   
   dend_labels_reodered = labels(dend) 
   dend_labels_reodered[2:3] = dend_labels_reodered[3:2]
   rotated_dend = rotate(dend, order = dend_labels_reodered)
   # same rotation applied, so labels should be ordered the same
   expect_identical(labels(rotated_dend), labels(as.dendrogram(rotated_x)))
})


test_that("sort.dendrogram works", {
   hc <- hclust(dist(USArrests[1:3, ]), "ave")
   dend <- as.dendrogram(hc)
   
   # sorting should switch order
   expect_identical(labels(dend), c("Arizona", "Alabama", "Alaska"))
   sorted_dend = sort.dendrogram(dend, type = "nodes")
   expect_identical(labels(sorted_dend), c("Arizona", "Alaska", "Alabama"))
})


