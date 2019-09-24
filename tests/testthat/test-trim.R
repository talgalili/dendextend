# library(dendextend)
# library(testthat)


context("Trimming a tree")


test_that("Trimming removes leaves and updates the object", {
  assign_dendextend_options()

  hc <- hclust(dist(USArrests[1:5, ]), "ave")
  dend <- as.dendrogram(hc)
  pruned_dend <- prune(dend, c("Alaska", "California"))


  expect_equal(nleaves(dend), 5L)
  expect_equal(nleaves(pruned_dend), 3L) # we pruned two leaves
  expect_equal(attr(pruned_dend, "members"), 3L) # checking the attributes are correct.
  expect_equal(attr(dend, "height"), attr(pruned_dend, "height")) # root height should not change due to this specific pruning
  expect_true(all(labels(pruned_dend) %in% labels(dend)))
  expect_warning(prune(dend, c("non-existing leaf")))
})



test_that("Trimming works for unbranched trees", {
  assign_dendextend_options()

  hc <- hclust(dist(USArrests[1:5, ]), "ave")
  dend <- as.dendrogram(hc)

  unbranched_dend <- unbranch(dend, 2)
  pruned_unbranched_dend <- prune(unbranched_dend, c("Alaska", "California"))

  expect_equal(nleaves(pruned_unbranched_dend), 3L)
  expect_equal(attr(pruned_unbranched_dend, "members"), 3L) # checking the attributes are correct.
  expect_true(all(labels(pruned_unbranched_dend) %in% labels(dend)))
  expect_warning(prune(unbranched_dend, c("non-existing leaf")))
})



test_that("Intersecting-trees works", {
  assign_dendextend_options()

  hc_1 <- hclust(dist(USArrests[1:5, ]), "single")
  hc_2 <- hclust(dist(USArrests[1:5, ]), "complete")
  dend_1 <- as.dendrogram(hc_1)
  dend_2 <- as.dendrogram(hc_2)

  pruned_dend_1 <- prune(dend_1, c("Alaska"))
  pruned_dend_2 <- prune(dend_2, c("California"))

  dends_12 <- intersect_trees(pruned_dend_1, pruned_dend_2)



  expect_equal(length(dends_12), 2L)
  expect_equal(nleaves(dends_12[[1]]), 3L) # 3 leaves - tree 1
  expect_equal(nleaves(dends_12[[2]]), 3L) # 3 leaves - tree 2
  expect_equal(
    length(
      intersect(labels(dends_12[[1]]), labels(dends_12[[2]]))
    ),
    3L
  ) # 3 labels are the same for both trees.


  # that it returns the same object
  dends_11 <- intersect_trees(dend_1, dend_1)
  expect_identical(dends_11[[1]], dend_1)
})

test_that("Trimming works for non-binary trees", {
  assign_dendextend_options()

  ## Simplest case: A flat, non-binary dendrogram
  hc <- hclust(dist(iris[1:5, -5]))
  dend_a <- as.dendrogram(hc)
  dend_a <- collapse_branch(dend_a, tol = 0.5)

  # Remove 1 non-binary leaf
  targets_1 <- c("2")
  pruned_dend_1 <- prune(dend_a, targets_1)

  # Since this is flat, expect a length of 5 - 1 = 4
  expect_equal(length(pruned_dend_1), 4L)
  # Labels should transfer as expected
  expect_equal(labels(pruned_dend_1), setdiff(labels(dend_a), targets_1))

  # Remove multiple non-binary leaves
  targets_2 <- c("2", "5")
  pruned_dend_2 <- prune(dend_a, targets_2)

  # Since this is flat, expect a length of 5 - 2 = 3
  expect_equal(length(pruned_dend_2), 3L)
  # Labels should transfer as expected
  expect_equal(labels(pruned_dend_2), setdiff(labels(dend_a), targets_2))


  ## A more complex example
  hc <- hclust(dist(iris[1:15, -5]))
  dend_b <- as.dendrogram(hc)
  dend_b <- collapse_branch(dend_b, tol = 0.3)

  # Remove 1 non-binary leaf in a terminal node
  targets_3 <- c("2")
  pruned_dend_3 <- prune(dend_b, targets_3)

  # The top of the dend shouldn't change, so length is 2
  expect_equal(length(pruned_dend_3), 2L)
  # N Leaves should now be reduced. 15 - 1 = 14
  expect_equal(nleaves(pruned_dend_3), 14L)
  # Labels should transfer as expected
  expect_equal(labels(pruned_dend_3), setdiff(labels(dend_b), targets_3))

  # Remove multiple non-binary leaves in a terminal node
  targets_4 <- c("2", "10", "13")
  pruned_dend_4 <- prune(dend_b, targets_4)

  # The top of the dend shouldn't change, so length is 2
  expect_equal(length(pruned_dend_4), 2L)
  # N Leaves should now be reduced. 15 - 3 = 12
  expect_equal(nleaves(pruned_dend_4), 12L)
  # Labels should transfer as expected
  expect_equal(labels(pruned_dend_4), setdiff(labels(dend_b), targets_4))

  # Remove all but one leaf from a non-binary node
  # Should merge up to the next level.
  targets_5 <- c("2", "3", "4", "10")
  pruned_dend_5 <- prune(dend_b, targets_5)

  # The top of the dend shouldn't change, so length is 2
  expect_equal(length(pruned_dend_5), 2L)
  # N Leaves should now be reduced. 15 - 4 = 11
  expect_equal(nleaves(pruned_dend_5), 11L)
  # Labels should transfer as expected
  expect_equal(labels(pruned_dend_5), setdiff(labels(dend_b), targets_5))
  # The remaining leaf, 13, was originally at:
  # dend[[2]][[2]][[1]][[2]][[3]]
  expect_true("13" == labels(dend_b[[2]][[2]][[1]][[2]][[3]]))
  expect_false(is.leaf(dend_b[[2]][[2]][[1]][[2]])) # This is the node with the non-binary split
  # The remaining leaf, 13, should now move up to:
  # dend[[2]][[2]][[1]][[2]]
  expect_true("13" == labels(pruned_dend_5[[2]][[2]][[1]][[2]]))
  expect_true(is.leaf(pruned_dend_5[[2]][[2]][[1]][[2]])) # This should now be true.

  # Remove a non-binary leaf from an upper branch
  targets_6 <- "1"
  pruned_dend_6 <- prune(dend_b, targets_6)

  # The top of the dend shouldn't change, so length is 2
  expect_equal(length(pruned_dend_6), 2L)
  # N Leaves should now be reduced. 15 - 1 = 14
  expect_equal(nleaves(pruned_dend_6), 14L)
  # Labels should transfer as expected
  expect_equal(labels(pruned_dend_6), setdiff(labels(dend_b), targets_6))

  # Remove multiple non-binary leaves from an upper branch
  targets_7 <- c("1", "5")
  pruned_dend_7 <- prune(dend_b, targets_7)

  # The top of the dend shouldn't change, so length is 2
  expect_equal(length(pruned_dend_7), 2L)
  # N Leaves should now be reduced. 15 - 2 = 13
  expect_equal(nleaves(pruned_dend_7), 13L)
  # Labels should transfer as expected
  expect_equal(labels(pruned_dend_7), setdiff(labels(dend_b), targets_7))
  # dend_b[[2]][[2]] should now be binary
  expect_equal(length(dend_b[[2]][[2]]), 4L)
  expect_equal(length(pruned_dend_7[[2]][[2]]), 2L)

  # Remove all non-binary leaves from an upper branch
  targets_8 <- c("1", "5", "8")
  pruned_dend_8 <- prune(dend_b, targets_8)

  # The top of the dend shouldn't change, so length is 2
  expect_equal(length(pruned_dend_8), 2L)
  # N Leaves should now be reduced. 15 - 3 = 12
  expect_equal(nleaves(pruned_dend_8), 12L)
  # Labels should transfer as expected
  expect_equal(labels(pruned_dend_8), setdiff(labels(dend_b), targets_8))
  # The branch at dend_b[[2]][[2]] should merge up and should have length 2, with 7 leaves
  expect_equal(length(pruned_dend_8[[2]][[2]]), 2L)
  expect_equal(nleaves(pruned_dend_8[[2]][[2]]), 7L)
  # Max depth for this branching should be reduced by 1
  expect_true(is.leaf(dend_b[[2]][[2]][[1]][[1]][[1]]))
  expect_true(is.leaf(pruned_dend_8[[2]][[2]][[1]][[1]]))


  # Remove non-binary and binary branches from multiple levels in one go
  targets_9 <- c("6", "14", "13", "8")
  pruned_dend_9 <- prune(dend_b, targets_9)

  # The top of the dend shouldn't change, so length is 2
  expect_equal(length(pruned_dend_9), 2L)
  # N Leaves should now be reduced. 15 - 4 = 11
  expect_equal(nleaves(pruned_dend_9), 11L)
  # Labels should transfer as expected
  expect_equal(labels(pruned_dend_9), setdiff(labels(dend_b), targets_9))
})


# unbranched_dend <- unbranch(dend,2)
# pruned_dend <- prune(unbranched_dend , c("Alaska", "California"))
# str(unclass(pruned_dend))
# str(unclass(unbranched_dend))
# plot(unbranched_dend)
# plot(pruned_dend)
#
#
