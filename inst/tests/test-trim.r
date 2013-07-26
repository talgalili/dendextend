context("Trimming a tree")


test_that("Trimming removes leaves and updates the object",{
   hc <- hclust(dist(USArrests[1:5,]), "ave")
   dend <- as.dendrogram(hc)
   trimmed_dend <- trim(dend , c("Alaska", "California"))
   

   expect_equal(nleaves(dend), 5L)
   expect_equal(nleaves(trimmed_dend), 3L) # we trimmed two leaves
   expect_equal(attr(trimmed_dend, "members"), 3L) # checking the attributes are correct.
   expect_equal(attr(dend, "height"), attr(trimmed_dend, "height")) # root height should not change due to this specific trimming
   expect_true(all(labels(trimmed_dend) %in% labels(dend)))
   expect_warning(trim(dend , c("non-existing leaf")))
})



test_that("Trimming works for unrooted trees",{
   hc <- hclust(dist(USArrests[1:5,]), "ave")
   dend <- as.dendrogram(hc)

   unrooted_dend <- unroot(dend,2)
   trimmed_unrooted_dend <- trim(unrooted_dend , c("Alaska", "California"))
   
   expect_equal(nleaves(trimmed_unrooted_dend), 3L)
   expect_equal(attr(trimmed_unrooted_dend, "members"), 3L) # checking the attributes are correct.
   expect_true(all(labels(trimmed_unrooted_dend) %in% labels(dend)))
   expect_warning(trim(unrooted_dend , c("non-existing leaf")))
})



test_that("Intersecting-trees works",{
   
   hc_1 <- hclust(dist(USArrests[1:5,]), "single")
   hc_2 <- hclust(dist(USArrests[1:5,]), "complete")
   dend_1 <- as.dendrogram(hc_1)
   dend_2 <- as.dendrogram(hc_2)
   
   trimmed_dend_1 <- trim(dend , c("Alaska"))
   trimmed_dend_2 <- trim(dend , c("California"))

   dends_12 <- intersect_trees(trimmed_dend_1,trimmed_dend_2)   
   
   expect_equal(length(dends_12), 2L)
   expect_equal(nleaves(dends_12[[1]]), 3L) # 3 leaves - tree 1
   expect_equal(nleaves(dends_12[[2]]), 3L) # 3 leaves - tree 2
   expect_equal(length(
      intersect(labels(dends_12[[1]]), labels(dends_12[[2]]))),
                3L) # 3 labels are the same for both trees.
})






# unrooted_dend <- unroot(dend,2)
# trimmed_dend <- trim(unrooted_dend , c("Alaska", "California"))
# str(unclass(trimmed_dend))
# str(unclass(unrooted_dend))
# plot(unrooted_dend)
# plot(trimmed_dend)
# 
# 

