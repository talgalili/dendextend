# library(testthat)
# library(dendextend)


context("find_k")


test_that("dudahart2 works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(98765)
   
   iriss <- iris[sample(150,20),-5]
   km <- kmeans(iriss,2)
   p <- dudahart2(iriss,km$cluster)$p.value
   expect_identical(signif(p, 3), 2.38e-06)
   
   p <- dudahart2(iriss,rep(1, length(km$cluster)))$p.value
   expect_identical(signif(p, 3), 0.87)
})


test_that("calinhara works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(98765)
   
   iriss <- iris[sample(150,20),-5]
   km <- kmeans(iriss,3)
   result <- round(calinhara(iriss,km$cluster),digits=2)
   expect_identical(result, 63.34)
   
   set.seed(1)
   result <- round(calinhara(iriss,rep(1, length(km$cluster)),cn=2),digits=2)
   expect_identical(result, 0)
})


test_that("distcritmulti works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(20000)
   
   options(digits=3)
   face <- fpc::rFace(50,dMoNo=2,dNoEy=0,p=2)
   clustering <- as.integer(attr(face,"grouping"))
   
   capture.output(
      result <- distcritmulti(face,clustering,ns=3,seed=100000,count=T,criterion="asw")   
   )
   expect_equal(result$subsets[[1]], c(34, 24, 32, 45, 5, 22, 8, 18, 41, 4, 30, 11, 13, 1, 27, 40))
   
   result <- distcritmulti(face,clustering,ns=3,seed=100000,fun="daisy",criterion="pearsongamma")
   expect_equal(result$subsets[[1]], c(34, 24, 32, 45, 5, 22, 8, 18, 41, 4, 30, 11, 13, 1, 27, 40))
})


test_that("cluster.stats works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(20000)
   if (!exists("silhouette")) silhouette <<- cluster::silhouette
   
   face <- fpc::rFace(200,dMoNo=2,dNoEy=0,p=2)
   dface <- dist(face)
   complete3 <- cutree(hclust(dface),3)
   
   result <- cluster.stats(dface,complete3,alt.clustering=as.integer(attr(face,"grouping")),noisecluster=T,silhouette=T,G2=T,G3=T,wgap=T,sepindex=T)
   expect_equal(
      round(result$entropy, 3),
      0.702
   )
   result <- cluster.stats(dface,complete3,alt.clustering=as.integer(attr(face,"grouping")),noisecluster=F,silhouette=T,aggregateonly=T)
   expect_equal(
      round(result$entropy, 3),
      0.702
   )
   result <- cluster.stats(dface,complete3,alt.clustering=as.integer(attr(face,"grouping")),noisecluster=F,silhouette=T)
   expect_equal(
      round(result$entropy, 3),
      0.702
   )
   result <- cluster.stats(dface,complete3,alt.clustering=as.integer(attr(face,"grouping")),noisecluster=T,sepwithnoise=F)
   expect_equal(
      round(result$entropy, 3),
      0.702
   )
   
   # if number of clusters != index of clusters in clustering parameter
   expect_warning(
      cluster.stats(dface,complete3-1,alt.clustering=as.integer(attr(face,"grouping"))-1,compareonly=T)   
   )
   
})


test_that("pamk works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(20000)
   
   face <- fpc::rFace(50,dMoNo=2,dNoEy=0,p=2)
   
   # setting diss = T raises warnings
   suppressWarnings(capture.output(
      pk <- pamk(face,krange=1:5,criterion="asw",critout=TRUE,diss=T)
   ))
   expect_equal(
      round(pk$crit, 3), c(0.000, 0.972, 0.913, 0.877, 0.877)
   )
   
   pk <- pamk(face,krange=1:5,criterion="asw",scaling=T,usepam=F,diss=F)
   expect_equal(
      round(pk$crit, 3), c(0.069, 0.529, 0.614, 0.404, 0.420)
   )
   
})


test_that("find_k works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   
   k <- find_k(dend)$k
   expect_equal(k, 3)
   
})


test_that("plot.find_k works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   
   x <- find_k(dend)
   expect_no_error(
      plot.find_k(x)      
   )
})