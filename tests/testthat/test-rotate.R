# library(testthat)


if("package:dendextendRcpp" %in% search()) {
   detach("package:dendextendRcpp")
   return_dendextendRcpp <- TRUE
} else {
   return_dendextendRcpp <- FALSE   
}

detach("package:dendextend")
suppressWarnings(require(ape, quietly = TRUE)) # if ape is available, let's make sure it is attached BEFORE dendextend!
suppressPackageStartupMessages(library(dendextend))
if(return_dendextendRcpp) suppressPackageStartupMessages(library(dendextendRcpp))
rm(return_dendextendRcpp)
# search()

context("Rotate a tree around its hinges")


test_that("Rotate a dendrogram",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)  

#    labels(dend)
#    rev(labels(dend))

   expect_equal(rev(labels(dend)), labels(rotate(dend, 3:1)))
   expect_equal(rev(labels(dend)), labels(rotate(dend, rev(labels(dend)))))
   
})


test_that("Rotate a dendrogram (but not exactly to what we asked for)",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)  
   
   new_order <- c(3,1,2) # an impossible order, will fail
   expect_false(all(labels(dend)[new_order] == labels(rotate(dend, new_order))))
   expect_false(all(labels(dend)[new_order] == labels(rotate(dend, labels(dend)[new_order]))))
   #    plot(rotate(dend, new_order))
   #    plot(dend)   
})



test_that("Rotate a dendrogram - works with either numeric or character",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)  
   
   new_order <- c(2,3,1) # a possible order
   
   expect_equivalent(rotate(dend, new_order), rotate(dend, labels(dend)[new_order]) )
#    plot(rotate(dend, new_order))
#    plot(rotate(dend, labels(dend)[new_order]) )
#    match(labels(dend)[new_order], labels(dend)) # NEW-OLD
#    new_order
   expect_equivalent(labels(rotate(dend, new_order)), 
                     labels(rotate(dend, labels(dend)[new_order]) ))
   
})







test_that("Rotate hclust",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   
   #    labels(hc)
   #    rev(labels(hc))
#    plot(hc)
#    plot(rotate(hc, 3:1))
      
   expect_equal(rev(labels(hc)), labels(rotate(hc, 3:1)))
   expect_equal(rev(labels(hc)), labels(rotate(hc, rev(labels(hc)))))   
})


test_that("Rotate a hclust (but not exactly to what we asked for)",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   
   new_order <- c(3,1,2) # an impossible order, will fail
   expect_false(all(labels(hc)[new_order] == labels(rotate(hc, new_order))))
   expect_false(all(labels(hc)[new_order] == labels(rotate(hc, labels(hc)[new_order]))))
   #    plot(rotate(hc, new_order))
   #    plot(hc)   
})

