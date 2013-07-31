# require(testthat)

context("Cutting a dendrogram")


test_that("Checking if a number is natural",{

   expect_true(is.natural.number(1))    # is TRUE
   expect_true(all(is.natural.number( seq(1,5, by=1)  )))
   expect_false(all(is.natural.number( seq(0,5, by=1)  )))
   expect_false(all(is.natural.number( seq(-1,5, by=0.5)  )))
   expect_error(is.natural.number( "a" ))
   
})



test_that("cutree a dendrogram by height h",{
   # data
   hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
   dend <- as.dendrogram(hc)
   
   # we need h!
   expect_error(cutree_1h.dendrogram(dend)) 
   
   # the same as cutree
   expect_identical( 
      cutree_1h.dendrogram(dend, h=100),
      cutree(hc, h=100)      )  
   
   expect_identical( 
      cutree_1h.dendrogram(dend, h=30),
      cutree(hc, h=30)      )  

   # the same as cutree - also when there are NO clusters
   expect_identical( 
      cutree_1h.dendrogram(dend, h=1000),
      cutree(hc, h=1000)      )  

   # the same as cutree - also when there are NO clusters
   expect_identical( 
      cutree_1h.dendrogram(dend, h=0),
      cutree(hc, h=0)      )  
   expect_identical( 
      cutree_1h.dendrogram(dend, h=1),
      cutree(hc, h=1)      )  
   
   # get return in the order of the dendrogram:
   expect_identical( 
      names(cutree_1h.dendrogram(dend, 100,order_clusters_as_data=FALSE)),
      labels(dend)      )  
   
})





test_that("get dendrogram heights for k clusters",{
   # data
   hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
   dend <- as.dendrogram(hc)
   unroot_dend <- unroot(dend,2)
   
   # plot(unroot_dend)

   dend_heights <- heights_per_k.dendrogram(dend)
   unroot_dend_heights <- heights_per_k.dendrogram(unroot_dend)
   
   cutree_1h.dendrogram(dend, h=dend_heights[[3]])
   
   
   
   
   expect_equal(length(dend_heights), 5)
   expect_equal(length(unroot_dend_heights), 4)
   
   expect_equal(nnodes(unroot_dend), 8)
   
   # dput(names(unroot_dend_heights))
   expect_equal(names(unroot_dend_heights), c("1", "3", "4", "5"))   
   
   
})
