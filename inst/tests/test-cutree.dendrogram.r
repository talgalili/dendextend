context("Cutting a dendrogram")


test_that("Rotate a dendrogram",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)  

#    labels(dend)
#    rev(labels(dend))

   expect_equal(rev(labels(dend)), labels(rotate(dend, 3:1)))
   expect_equal(rev(labels(dend)), labels(rotate(dend, rev(labels(dend)))))
   
})
