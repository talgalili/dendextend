context("unbranching a tree")


test_that("Get attribute of node's branches (height)",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
      
   expect_equal(get_branches_attr(dend, "height") , c(0, 37.17700902))   
   expect_true(unlist(get_branches_attr(dend, "leaf")))
   expect_warning(get_branches_attr(unclass(dend), "height")) # warn when used on an object which is NOT of class "dendrogram"
})


test_that("Raising a dendrogram's root height",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   taller_dend <- raise.dendrogram(dend, 10)
   shorter_dend <- raise.dendrogram(dend, -10)

   expect_equal(attr(dend, "height"), 54.800410723)   
   expect_equal(attr(taller_dend, "height"), 64.800410723)   
   expect_equal(attr(shorter_dend, "height"), 44.800410723)   
})



test_that("unbranching a dendrogram",{
   hc <- hclust(dist(USArrests[10:13,]), "ward")
   dend <- as.dendrogram(hc)
#    plot(dend)
   
   unbranched_dend <- unbranch(dend,1)
   unbranched_dend_2 <- unbranch(unbranched_dend,3)
   #    plot(unbranched_dend)
   #    plot(unbranched_dend_2)
   
   expect_equal(length(unbranched_dend), 3L) # our new tree has 3 branches
   expect_equal(length(unbranched_dend_2), 4L) # our new tree has 3 branches
   expect_warning(unbranch(dend[[1]])) # since the new root can not be a leaf.
   expect_error(unbranch(hc)) # While there is an hclust method, 
                              # this object can not handle non-binary trees.
})


