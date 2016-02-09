# library(testthat)

context("Bk method (FM Index) between two trees")


test_that("sort_2_clusters_vectors works",{
   set.seed(23235)
   ss <- sample(1:150, 4 )
   hc1 <- hclust(dist(datasets::iris[ss,-5]), "com")
   hc2 <- hclust(dist(datasets::iris[ss,-5]), "single")
   # dend1 <- as.dendrogram(hc1)
   # dend2 <- as.dendrogram(hc2)
   #    cutree(dend1)   
   
   A1_clusters <- cutree(hc1, k=3)
   A2_clusters <- sample(cutree(hc1, k=3))
   
   sorted_As <- sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors =TRUE) # no sorting
   expect_false(identical(sorted_As[[1]],sorted_As[[2]]))
   
   sorted_As <- sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors =FALSE) # Sorted
   expect_true(identical(sorted_As[[1]],sorted_As[[2]]))
   
})






test_that("FM_index_profdpm works",{
   
   #    set.seed(23235)
   ss <- TRUE # sample(1:150, 10 )
   hc1 <- hclust(dist(datasets::iris[ss,-5]), "com")
   hc2 <- hclust(dist(datasets::iris[ss,-5]), "single")
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
   #    plot(sapply(ks, fo)~ ks, type = "b", main = "Bk plot for the datasets::iris dataset")
   #      
   
   
   
})




test_that("FM_index_R works",{
   
   #    set.seed(23235)
   ss <- TRUE # sample(1:150, 10 )
   hc1 <- hclust(dist(datasets::iris[ss,-5]), "com")
   hc2 <- hclust(dist(datasets::iris[ss,-5]), "single")
   # dend1 <- as.dendrogram(hc1)
   # dend2 <- as.dendrogram(hc2)
   #    cutree(dend1)   
   
   # FM index of a cluster with himself is 1:
   expect_equivalent(FM_index_R(cutree(hc1, k=3), cutree(hc1, k=3)) ,1)
   # removing the attr - it is EXACTLY 1: (but NOT 1L)
   expect_identical(as.vector(FM_index_R(cutree(hc1, k=3), cutree(hc1, k=3))) ,1)
   
   # sorting of the clusters based on their names:
   set.seed(1341)
   expect_false(identical(
      as.vector(
         FM_index_R(cutree(hc1, k=3),
                          sample(cutree(hc1, k=3)),
                          assume_sorted_vectors =TRUE)) ,1))
   # It actually becomes: 0.38037
   # but if we leave sorting as TRUE, we will get 1:
   
   expect_true(identical(
      as.vector(FM_index_R(cutree(hc1, k=3),
                                 sample(cutree(hc1, k=3)),
                                 assume_sorted_vectors =FALSE)) ,1))
   # it is actually the default:
   expect_true(identical(
      as.vector(FM_index_R(cutree(hc1, k=3),
                                 sample(cutree(hc1, k=3))
      )) ,1))
   
   
   # we can get a range of FM inexes using the following:
   fo <- function(k) FM_index_R(cutree(hc1, k), cutree(hc2, k)) 
   #    dput(round(unname(sapply(1:4, fo)),2))
   
   expect_identical(round(unname(sapply(1:4, fo)),2),
                    c(1, 0.71, 0.81, 0.75)
   )
   #    ks <- 1:150
   #    plot(sapply(ks, fo)~ ks, type = "b", main = "Bk plot for the datasets::iris dataset")
   #      
   
   set.seed(234234)
   tmp_index <- FM_index_R(cutree(hc1, k=3), sample(cutree(hc1, k=3)), assume_sorted_vectors=TRUE)
   expect_identical(round(as.vector(tmp_index), 2), 0.38)
   expect_identical(round(attr(tmp_index, "E_FM"), 2), 0.37)
   expect_identical(round(sqrt(attr(tmp_index, "V_FM")), 3), 0.008)   
})



test_that("FM_index works",{
   
   #    set.seed(23235)
   ss <- TRUE # sample(1:150, 10 )
   hc1 <- hclust(dist(datasets::iris[ss,-5]), "com")
   hc2 <- hclust(dist(datasets::iris[ss,-5]), "single")
   # dend1 <- as.dendrogram(hc1)
   # dend2 <- as.dendrogram(hc2)
   #    cutree(dend1)   
   
   # FM index of a cluster with himself is 1:
   expect_equivalent(FM_index(cutree(hc1, k=3), cutree(hc1, k=3)) ,1)
   # removing the attr - it is EXACTLY 1: (but NOT 1L)
   expect_identical(as.vector(FM_index(cutree(hc1, k=3), cutree(hc1, k=3))) ,1)
   
   # sorting of the clusters based on their names:
   set.seed(1341)
   expect_false(identical(
      as.vector(
         FM_index(cutree(hc1, k=3),
                    sample(cutree(hc1, k=3)),
                    assume_sorted_vectors =TRUE)) ,1))
   # It actually becomes: 0.38037
   # but if we leave sorting as TRUE, we will get 1:
   
   expect_true(identical(
      as.vector(FM_index(cutree(hc1, k=3),
                           sample(cutree(hc1, k=3)),
                           assume_sorted_vectors =FALSE)) ,1))
   # it is actually the default:
   expect_true(identical(
      as.vector(FM_index(cutree(hc1, k=3),
                           sample(cutree(hc1, k=3))
      )) ,1))
   
   
   # we can get a range of FM inexes using the following:
   fo <- function(k) FM_index(cutree(hc1, k), cutree(hc2, k)) 
   #    dput(round(unname(sapply(1:4, fo)),2))
   
   expect_identical(round(unname(sapply(1:4, fo)),2),
                    c(1, 0.71, 0.81, 0.75)
   )
   #    ks <- 1:150
   #    plot(sapply(ks, fo)~ ks, type = "b", main = "Bk plot for the datasets::iris dataset")
   #      
   
   
   expect_identical(as.vector(FM_index(cutree(hc1, k=3), cutree(hc1, k=3),include_EV=TRUE)) ,
                    as.vector(FM_index(cutree(hc1, k=3), cutree(hc1, k=3),include_EV=FALSE)) 
                    )
   
   
   
})







test_that("FM_index_permutation works",{
   
   
   set.seed(23235)
   ss <- TRUE # sample(1:150, 10 )
   hc1 <- hclust(dist(datasets::iris[ss,-5]), "com")
   hc2 <- hclust(dist(datasets::iris[ss,-5]), "single")
   # dend1 <- as.dendrogram(hc1)
   # dend2 <- as.dendrogram(hc2)
   #    cutree(dend1)   
   
   A1_clusters <- cutree(hc1, k=3)
   A2_clusters <- A1_clusters
   
   R <- 10
   set.seed(414130)
   FM_index_H0 <- replicate(R, FM_index_permutation(A1_clusters, A2_clusters)) # can take 10 sec
   
   expect_identical( round(mean(FM_index_H0), 2) ,
                     0.37)
   expect_identical( round(sd(FM_index_H0), 2) ,
                     0.01)
   
   
   
})








test_that("Bk works",{
   
   
   set.seed(23235)
   ss <- TRUE # sample(1:150, 10 )
   hc1 <- hclust(dist(datasets::iris[ss,-5]), "com")
   hc2 <- hclust(dist(datasets::iris[ss,-5]), "single")
   dend1 <- as.dendrogram(hc1)
   dend2 <- as.dendrogram(hc2)
   #    cutree(tree1)   
   
   expect_identical(
      Bk(hc1, hc2, k = 3),
      Bk(dend1, dend2, k = 3,)
   )

   expect_identical(
      Bk(hc1, hc2, k = 3,include_EV=FALSE),
      Bk(dend1, dend2, k = 3,include_EV=FALSE)
   )

   expect_identical(
      round(as.numeric(Bk(hc1, hc2, k = 3)),3),
      0.806
   )
   expect_identical(
      round(as.numeric(Bk(dend1, dend2, k = 3)),3),
      0.806
   )

   
   set.seed(23234535)
   expect_identical(
      round(
         Bk_permutations(dend1, 
                         dend2, 
                         k = 3, R = 2)[[1]], 2),
      c(.45,.44))
   
   
   
   #    Bk_plot(hc1, hc2, main = "WRONG Bk plot \n(due to the way cutree works with ties)")
   #    Bk_plot(dend1, dend2, main = "CORRECT Bk plot")
   
   
   
})



