context("unbranching a tree")

# set this for testing
dendextend_options("warn", TRUE)

test_that("Get attribute of node's branches (height)", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  expect_equal(get_root_branches_attr(dend, "height"), c(0, 37.17700902))
  expect_true(unlist(get_root_branches_attr(dend, "leaf")))
  expect_warning(get_root_branches_attr(unclass(dend), "height")) # warn when used on an object which is NOT of class "dendrogram"
})


test_that("Raising a dendrogram's root height", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  taller_dend <- raise.dendrogram(dend, 10)
  shorter_dend <- raise.dendrogram(dend, -10)

  expect_equal(attr(dend, "height"), 54.800410723)
  expect_equal(attr(taller_dend, "height"), 64.800410723)
  expect_equal(attr(shorter_dend, "height"), 44.800410723)
})



test_that("unbranching a dendrogram", {
  hc <- hclust(dist(USArrests[10:13, ]), "ave")
  dend <- as.dendrogram(hc)
  #    plot(dend)

  unbranched_dend <- unbranch(dend, 1)
  unbranched_dend_2 <- unbranch(unbranched_dend, 3)
  #    plot(unbranched_dend)
  #    plot(unbranched_dend_2)

  expect_equal(length(unbranched_dend), 3L) # our new tree has 3 branches
  expect_equal(length(unbranched_dend_2), 4L) # our new tree has 3 branches
  expect_equal(labels(dend), labels(unbranched_dend_2)) # order of leaves is preserved
  expect_warning(unbranch(dend[[1]])) # since the new root can not be a leaf.
  expect_error(unbranch(hc)) # While there is an hclust method,
  # this object can not handle non-binary trees.
  
  # if 'new_root_height' parameter used
  result <- unbranch.dendrogram(dend, new_root_height = 4.5)
  expect_equal(length(result), 3)
  
  # no default method
  expect_error(
     unbranch.default(dend)
  )
})


test_that("flatten.dendrogram works", {
   hc <- hclust(dist(USArrests[10:13, ]), "ave")
   dend <- as.dendrogram(hc)
   
   result <- flatten.dendrogram(dend)
   expect_equal(
      attr(result[[1]], "height"),
      attr(result[[2]], "height")
   )
   
})


test_that("collapse_branch works", {
   hc <- hclust(dist(USArrests[10:13, ]), "ave")
   dend <- as.dendrogram(hc)
   
   result <- collapse_branch(dend, lower = F)
   expect_equal(
      attr(result[[1]], "height"),
      attr(result[[2]], "height")
   )
   
   # if leaf object passed in
   expect_warning(
      collapse_branch(dend[[1]][[1]])
   )
   
   # if non-dendrogram object passed in
   expect_error(
      collapse_branch(hc)
   )
})
