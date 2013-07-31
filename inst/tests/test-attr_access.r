# require(testthat)

context("Attribute access")


test_that("Get a dendrogram leaves attributes",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   expect_identical(get_leaves_attr(dend, "label"), labels(dend, "label"))   
   expect_identical(get_leaves_attr(dend, "height"), rep(0, 3))
   expect_identical(get_leaves_attr(dend, "leaf"), rep(TRUE, 3))
   expect_identical(get_leaves_attr(dend, "members"), rep(1L, 3))
   
   expect_false(    
      identical(get_leaves_attr(hang.dendrogram(dend), "height"), rep(0, 3))
   )
   
})


test_that("Get a dendrogram nodes attributes",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   expect_error(get_nodes_attr(dend)) # we need attribute!
   
   
   # NA's are from the nodes which are not leaves:
   expect_identical(get_nodes_attr(dend, "label"),
      c(NA, "Arizona", NA, "Alabama", "Alaska"))
   
   # removing NA's
   expect_identical(get_nodes_attr(dend, "label", na.rm = TRUE), labels(dend, "label"))
   
   # leaves have 0 height
   expect_equal(get_nodes_attr(dend, "height"), c(54.8004107236398, 0, 37.1770090243957, 0, 0))
   
   # when excluding leaves - it replaces them with NA:
   expect_equal(get_nodes_attr(dend, "height",include_leaves=FALSE), c(54.8004107236398, NA, 37.1770090243957, NA, NA))
   
   # this gives ONLY the attribute of the branches nodes (not that of the leaves)
   expect_equal(get_nodes_attr(dend, "height",include_leaves=FALSE, na.rm = TRUE), c(54.8004107236398,  37.1770090243957))

   expect_identical(get_nodes_attr(dend, "leaf", na.rm = TRUE), rep(TRUE, 3))
   
   # how to make get_nodes_attr act like get_leaves_attr
   expect_identical(   get_leaves_attr(dend, "members"), # should be 1's
                       get_nodes_attr(dend, "members", include_branches = FALSE, na.rm = TRUE)
   )
   
   expect_identical(   get_leaves_attr(dend, "height"), # should be 1's
                       get_nodes_attr(dend, "height", include_branches = FALSE, na.rm = TRUE)
   )
   
   expect_identical(get_nodes_attr(dend, "members"), c(3L, 1L, 2L, 1L, 1L))
})


# \donotrun{
# require(microbenchmark)
# # get_leaves_attr is twice faster than get_nodes_attr
# microbenchmark(   get_leaves_attr(dend, "members"), # should be 1's
#                     get_nodes_attr(dend, "members", include_branches = FALSE, na.rm = TRUE)
# )
# }



test_that("Get a dendrogram's branches heights",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)  
   
   expect_equal(get_branches_heights(dend),c(37.1770090243957, 54.8004107236398))
                    
   expect_identical(get_branches_heights(dend),
                    sort(get_nodes_attr(dend, "height",include_leaves=FALSE, na.rm = TRUE))
   )
   
})





test_that("Hanging dendrogram works just like hclust",{
   
   hc <- hclust(dist(USArrests[1:5,]), "ave")
   dend <- as.dendrogram(hc)   
   
   expect_true(identical(as.dendrogram(hc, hang = 0.1),
                         hang.dendrogram(dend, hang = 0.1)))   
})



test_that("Hanging dendrogram works for unrooted trees",{
   hc <- hclust(dist(USArrests[1:5,]), "ave")
   dend <- as.dendrogram(hc)
   
   unrooted_dend <- unroot(dend,2)
   
   # we can't hclust an unrooted tree...
   expect_error(as.hclust(unrooted_dend))
   
   #    plot(hang.dendrogram(unrooted_dend, hang = 0.1))
   
   # showing that we can hang an unrooted tree   
   expect_false(identical(unrooted_dend,
                          hang.dendrogram(unrooted_dend, hang = 0.1)))   
})

