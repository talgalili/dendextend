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
   
#    cutree_1h.dendrogram(dend, h=dend_heights[[3]])   
   
   expect_equal(length(dend_heights), 5)
   expect_equal(length(unroot_dend_heights), 4)
   
   expect_equal(nnodes(unroot_dend), 8)
   
   # dput(names(unroot_dend_heights))
   expect_equal(names(unroot_dend_heights), c("1", "3", "4", "5"))      
})






test_that("cutree a dendrogram to k clusters",{
   # data
   hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
   dend <- as.dendrogram(hc)
   unroot_dend <- unroot(dend,2)
   
#    plot(unroot_dend)
   
   
   # the same as cutree
   expect_identical( 
      cutree_1k.dendrogram(dend, k=3),
      cutree(hc, k=3)      )  
   
   expect_identical( 
      cutree_1k.dendrogram(dend, k=1),
      cutree(hc,  k=1)      )  
   
   # the same as cutree - also when there are NO clusters
   expect_identical( 
      cutree_1k.dendrogram(dend, k=5),
      cutree(hc, k=5)      )  

   # if ignoring the "names" on the vector - the numbers will be identical:
   expect_identical( 
      unname(cutree_1k.dendrogram(dend, k=3, use_labels_not_values = FALSE)),
      unname(cutree(hc, k=3) )     )    
   
   
   # errors:
   expect_error(cutree_1k.dendrogram(dend))  # we need h!
   expect_error( cutree_1k.dendrogram(dend, k = -1))
   expect_error( cutree_1k.dendrogram(dend, k = 0))
#    expect_error( cutree_1k.dendrogram(dend, k = 1.5)) # I no longer expect an error since it is turned into "as.integer"
   expect_error( cutree_1k.dendrogram(dend, k = 50))
   expect_error( cutree(hc, k=50)      )  
   
   
   # get return in the order of the dendrogram:
   expect_identical( 
      names(cutree_1k.dendrogram(dend, k=3,order_clusters_as_data=FALSE)),
      labels(dend)      )  
   
   # cases of no possible k's:
   expect_warning(cutree_1k.dendrogram(unroot_dend, 2))
   expect_equal(cutree_1k.dendrogram(unroot_dend, 2, warn = FALSE), rep(NA,5))
   
   
})






test_that("cutree dendrogram method works for k",{
   # data
   hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
   dend <- as.dendrogram(hc)
   unroot_dend <- unroot(dend,2)
   
   #    plot(unroot_dend)
   
   
   # the same as cutree
   expect_identical( 
      cutree(dend, k=3),
      cutree(hc, k=3)      )  
   
   expect_identical( 
      cutree(dend, k=1),
      cutree(hc,  k=1)      )  
   
   # the same as cutree - also when there are NO clusters
   expect_identical( 
      cutree(dend, k=5),
      cutree(hc, k=5)      )  
   
   # if ignoring the "names" on the vector - the numbers will be identical:
   expect_identical( 
      unname(cutree(dend, k=3, use_labels_not_values = FALSE)),
      unname(cutree(hc, k=3) )     )    
   
   
   # errors:
   expect_error(cutree(dend))  # we need h!
   expect_error( cutree(dend, k=-1))
   expect_error( cutree(dend, k=0))
#    expect_error( cutree(dend, k = 1.5)) # I no longer expect an error since it is turned into "as.integer"
   expect_error( cutree(dend, k=50))
   expect_error( cutree(hc, k=50)      )  
   
   
   # get return in the order of the dendrogram:
   expect_identical( 
      names(cutree(dend, k=3,order_clusters_as_data=FALSE,try_cutree_hclust=FALSE)),
      labels(dend)      )  
   
   expect_identical( 
      names(cutree(dend, k=3,order_clusters_as_data=FALSE,try_cutree_hclust=TRUE)),
      labels(dend)      )  

   # cases of no possible k's:
   expect_warning(cutree(unroot_dend, 2))
   expect_equal(cutree(unroot_dend, 2, warn = FALSE), rep(NA,5))

   # now to check vectorization
   
   
})



test_that("cutree for dendrogram works (k,h and vectorization)",{
   
   # data
   hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
   dend <- as.dendrogram(hc)
   unroot_dend <- unroot(dend,2)
   
   #    plot(unroot_dend)
   
   
   
   # the same as cutree
   expect_identical( 
      cutree(dend, k=1:4),
      cutree(hc, k=1:4)      )

   expect_identical( 
      cutree(dend, h = c(20, 25.5, 50,170)),
      cutree(hc, h = c(20, 25.5, 50,170))      )
   
   expect_warning( cutree(unroot_dend, k=1:2) )    
   
   # it still works for missing k's, it just returns NA's in the second column
   cutree_unroot_dend <- cutree(unroot_dend, k=1:4, warn = FALSE)
   expect_true( is.matrix(cutree_unroot_dend) )
   expect_true( all(is.na(cutree_unroot_dend[,2])) ) # 2nd column is NA.
   
   cutree_unroot_dend_2 <- cutree(unroot_dend, k=1:4,   
                                warn = FALSE, order_clusters_as_data = FALSE,
                                try_cutree_hclust = FALSE
                                 )
   expect_identical(rownames(cutree_unroot_dend_2), labels(unroot_dend))
   
   
})
