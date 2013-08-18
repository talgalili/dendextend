# require(testthat)

context("Bk method (FM Index) between two trees")


test_that("FM_index_profdpm works",{
   
#    set.seed(23235)
   ss <- TRUE # sample(1:150, 10 )
   hc1 <- hclust(dist(iris[ss,-5]), "com")
   hc2 <- hclust(dist(iris[ss,-5]), "single")
   # dend1 <- as.dendrogram(hc1)
   # dend2 <- as.dendrogram(hc2)
   #    cutree(dend1)   
   
   # FM index of a cluster with himself is 1:
   expect_equivalent(FM_index_profdpm(cutree(hc1, k=3), cutree(hc1, k=3)) ,1)
   # removing the attr - it is EXACTLY 1: (but NOT 1L)
   expect_identical(as.vector(FM_index_profdpm(cutree(hc1, k=3), cutree(hc1, k=3))) ,1)

   # sorting of the clusters based on their names:
   set.seed(1341)
   expect_false(identical(
      as.vector(
         FM_index_profdpm(cutree(hc1, k=3),
                       sample(cutree(hc1, k=3)),
                       assume_sorted_vectors =TRUE)) ,1))
   # It actually becomes: 0.38037
   # but if we leave sorting as TRUE, we will get 1:
   expect_true(identical(
      as.vector(FM_index_profdpm(cutree(hc1, k=3),
                       sample(cutree(hc1, k=3)),
                       assume_sorted_vectors =FALSE)) ,1))
   # it is actually the default:
   expect_true(identical(
      as.vector(FM_index_profdpm(cutree(hc1, k=3),
                                 sample(cutree(hc1, k=3))
                                 )) ,1))
   
   
   # we can get a range of FM inexes using the following:
   fo <- function(k) FM_index_profdpm(cutree(hc1, k), cutree(hc2, k)) 
#    dput(round(unname(sapply(1:4, fo)),2))

   expect_identical(round(unname(sapply(1:4, fo)),2),
                    c(1, 0.71, 0.81, 0.75)
                    )
#    ks <- 1:150
#    plot(sapply(ks, fo)~ ks, type = "b", main = "Bk plot for the iris dataset")
#      
   
   
   
})




