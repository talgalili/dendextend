# library(testthat)
# library(dendextend)


context("all.equal")


test_that("all.equal.dendrogram works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   hc <- hclust(dist(1:5))
   dend <- as.dendrogram(hc)
   dend2 <- shuffle(dend)
   dend3 <- as.dendrogram(hclust(dist(iris[1:10, -5])))
                          
   expect_identical(
      typeof(all.equal.dendrogram(dend, dend2, use.edge.length = F, use.tip.label.order = T)),
      "character"
   )
   expect_identical(
      typeof(all.equal.dendrogram(dend, dend3, use.edge.length = F, use.tip.label.order = F)),
      "character"
   )
   expect_identical(
      typeof(all.equal.dendrogram(dend, dend2[[2]], use.edge.length = F, use.tip.label = F)),
      "character"
   )
   
   # if non-dend object passed in
   expect_error(
      all.equal.dendrogram(hc, dend)  
   )
   expect_identical(
      all.equal.dendrogram(dend, hc),
      "current is not a dendrogram"
   )
   
   # if non-dendlist object passed in
   expect_error(
      all.equal.dendlist(dend)
   )
   # if dendlist has only one dend in it
   dend12 <- dendlist(dend)
   expect_warning(
      all.equal.dendlist(dend12)
   )
   
})


all.equal(c("a","b"),c("b","a"))
