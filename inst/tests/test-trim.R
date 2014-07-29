# library(dendextend)
# library(testthat)


context("Trimming a tree")


test_that("Trimming removes leaves and updates the object",{
   hc <- hclust(dist(USArrests[1:5,]), "ave")
   dend <- as.dendrogram(hc)
   pruned_dend <- prune(dend , c("Alaska", "California"))
   

   expect_equal(nleaves(dend), 5L)
   expect_equal(nleaves(pruned_dend), 3L) # we pruned two leaves
   expect_equal(attr(pruned_dend, "members"), 3L) # checking the attributes are correct.
   expect_equal(attr(dend, "height"), attr(pruned_dend, "height")) # root height should not change due to this specific pruning
   expect_true(all(labels(pruned_dend) %in% labels(dend)))
   expect_warning(prune(dend , c("non-existing leaf")))
})



test_that("Trimming works for unbranched trees",{
   hc <- hclust(dist(USArrests[1:5,]), "ave")
   dend <- as.dendrogram(hc)

   unbranched_dend <- unbranch(dend,2)
   pruned_unbranched_dend <- prune(unbranched_dend , c("Alaska", "California"))
   
   expect_equal(nleaves(pruned_unbranched_dend), 3L)
   expect_equal(attr(pruned_unbranched_dend, "members"), 3L) # checking the attributes are correct.
   expect_true(all(labels(pruned_unbranched_dend) %in% labels(dend)))
   expect_warning(prune(unbranched_dend , c("non-existing leaf")))
})



test_that("Intersecting-trees works",{
   
   hc_1 <- hclust(dist(USArrests[1:5,]), "single")
   hc_2 <- hclust(dist(USArrests[1:5,]), "complete")
   dend_1 <- as.dendrogram(hc_1)
   dend_2 <- as.dendrogram(hc_2)
   
   pruned_dend_1 <- prune(dend_1 , c("Alaska"))
   pruned_dend_2 <- prune(dend_2 , c("California"))

   dends_12 <- intersect_trees(pruned_dend_1,pruned_dend_2)   
   
   
   
   expect_equal(length(dends_12), 2L)
   expect_equal(nleaves(dends_12[[1]]), 3L) # 3 leaves - tree 1
   expect_equal(nleaves(dends_12[[2]]), 3L) # 3 leaves - tree 2
   expect_equal(length(
      intersect(labels(dends_12[[1]]), labels(dends_12[[2]]))),
                3L) # 3 labels are the same for both trees.
   
   
   # that it returns the same object
   dends_11 <- intersect_trees(dend_1,dend_1)   
   expect_identical(dends_11[[1]], dend_1)
   
   
})






# unbranched_dend <- unbranch(dend,2)
# pruned_dend <- prune(unbranched_dend , c("Alaska", "California"))
# str(unclass(pruned_dend))
# str(unclass(unbranched_dend))
# plot(unbranched_dend)
# plot(pruned_dend)
# 
# 

