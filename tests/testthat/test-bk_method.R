# library(testthat)
# library(dendextend)

context("Bk method (FM Index) between two trees")


test_that("sort_2_clusters_vectors works", {
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(23235)
  ss <- sample(1:150, 4)
  hc1 <- hclust(dist(datasets::iris[ss, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[ss, -5]), "single")
  # dend1 <- as.dendrogram(hc1)
  # dend2 <- as.dendrogram(hc2)
  #    cutree(dend1)

  A1_clusters <- cutree(hc1, k = 3)
  A2_clusters <- sample(cutree(hc1, k = 3))

  sorted_As <- sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors = TRUE) # no sorting
  expect_false(identical(sorted_As[[1]], sorted_As[[2]]))

  sorted_As <- sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors = FALSE) # Sorted
  expect_true(identical(sorted_As[[1]], sorted_As[[2]]))
  
  # test edge case warnings
  A2_clusters <- A2_clusters[1:3]
  expect_warning(
     result <- sort_2_clusters_vectors(A1_clusters, A2_clusters, warn = T)
  )
  expect_identical(A1_clusters, result[[1]])
  expect_identical(A2_clusters, result[[2]])
  
  names(A2_clusters) <- NULL
  expect_warning(
     result <- sort_2_clusters_vectors(A1_clusters, A2_clusters, warn = T)
  )
  expect_identical(A1_clusters, result[[1]])
  expect_identical(A2_clusters, result[[2]])
  
  # create custom object with different length of names as length of object to trigger warning
  custom_object <- list(a = 1, b = 2, c = 3) 
  class(custom_object) <- "custom_class"
  names.custom_class <<- function(x) {
     return(c("Name1", "Name2", "Name3", "Name4", "Name5"))
  }
  expect_warning(sort_2_clusters_vectors(custom_object, custom_object, warn = T))
})




test_that("FM_index_R works", {
  suppressWarnings(RNGversion("3.5.0"))

  #    set.seed(23235)
  ss <- TRUE # sample(1:150, 10 )
  hc1 <- hclust(dist(datasets::iris[ss, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[ss, -5]), "single")
  # dend1 <- as.dendrogram(hc1)
  # dend2 <- as.dendrogram(hc2)
  #    cutree(dend1)

  # FM index of a cluster with himself is 1:
  expect_equivalent(FM_index_R(cutree(hc1, k = 3), cutree(hc1, k = 3)), 1)
  # removing the attr - it is EXACTLY 1: (but NOT 1L)
  expect_identical(as.vector(FM_index_R(cutree(hc1, k = 3), cutree(hc1, k = 3))), 1)

  # sorting of the clusters based on their names:
  set.seed(1341)
  expect_false(identical(
    as.vector(
      FM_index_R(cutree(hc1, k = 3),
        sample(cutree(hc1, k = 3)),
        assume_sorted_vectors = TRUE
      )
    ), 1
  ))
  # It actually becomes: 0.38037
  # but if we leave sorting as TRUE, we will get 1:

  expect_true(identical(
    as.vector(FM_index_R(cutree(hc1, k = 3),
      sample(cutree(hc1, k = 3)),
      assume_sorted_vectors = FALSE
    )), 1
  ))
  # it is actually the default:
  expect_true(identical(
    as.vector(FM_index_R(
      cutree(hc1, k = 3),
      sample(cutree(hc1, k = 3))
    )), 1
  ))


  # we can get a range of FM inexes using the following:
  fo <- function(k) FM_index_R(cutree(hc1, k), cutree(hc2, k))
  #    dput(round(unname(sapply(1:4, fo)),2))

  expect_identical(
    round(unname(sapply(1:4, fo)), 2),
    c(1, 0.71, 0.81, 0.75)
  )
  #    ks <- 1:150
  #    plot(sapply(ks, fo)~ ks, type = "b", main = "Bk plot for the datasets::iris dataset")
  #

  set.seed(234234)
  tmp_index <- FM_index_R(cutree(hc1, k = 3), sample(cutree(hc1, k = 3)), assume_sorted_vectors = TRUE)
  expect_identical(round(as.vector(tmp_index), 2), 0.38)
  expect_identical(round(attr(tmp_index, "E_FM"), 2), 0.37)
  expect_identical(round(sqrt(attr(tmp_index, "V_FM")), 3), 0.008)
  
  # test case where a cluster with an NA value is passed in
  hc1_with_na = cutree(hc1, k = 3)
  hc1_with_na[1] = NA
  expect_warning(
     FM_index_R(hc1_with_na, cutree(hc1, k = 3), warn = T)
  )
  
  # create custom class to pass in which triggers error otherwise not possible
  x <<- c(1, 2, 3, 4, 5) 
  class(x) <<- "x"
  length.x <<- function(x) 1
  y <<- c(1, 2, 3, 4, 5) 
  class(y) <<- "y"
  length.y <<- function(y) 1
  expect_error(
     FM_index_R(x, y, assume_sorted_vectors = T)
  )

})



test_that("FM_index works", {
  suppressWarnings(RNGversion("3.5.0"))
  #    set.seed(23235)
  ss <- TRUE # sample(1:150, 10 )
  hc1 <- hclust(dist(datasets::iris[ss, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[ss, -5]), "single")
  # dend1 <- as.dendrogram(hc1)
  # dend2 <- as.dendrogram(hc2)
  #    cutree(dend1)

  # FM index of a cluster with himself is 1:
  expect_equivalent(FM_index(cutree(hc1, k = 3), cutree(hc1, k = 3)), 1)
  # removing the attr - it is EXACTLY 1: (but NOT 1L)
  expect_identical(as.vector(FM_index(cutree(hc1, k = 3), cutree(hc1, k = 3))), 1)

  # sorting of the clusters based on their names:
  set.seed(1341)
  expect_false(identical(
    as.vector(
      FM_index(cutree(hc1, k = 3),
        sample(cutree(hc1, k = 3)),
        assume_sorted_vectors = TRUE
      )
    ), 1
  ))
  # It actually becomes: 0.38037
  # but if we leave sorting as TRUE, we will get 1:

  expect_true(identical(
    as.vector(FM_index(cutree(hc1, k = 3),
      sample(cutree(hc1, k = 3)),
      assume_sorted_vectors = FALSE
    )), 1
  ))
  # it is actually the default:
  expect_true(identical(
    as.vector(FM_index(
      cutree(hc1, k = 3),
      sample(cutree(hc1, k = 3))
    )), 1
  ))


  # we can get a range of FM inexes using the following:
  fo <- function(k) FM_index(cutree(hc1, k), cutree(hc2, k))
  #    dput(round(unname(sapply(1:4, fo)),2))

  expect_identical(
    round(unname(sapply(1:4, fo)), 2),
    c(1, 0.71, 0.81, 0.75)
  )
  #    ks <- 1:150
  #    plot(sapply(ks, fo)~ ks, type = "b", main = "Bk plot for the datasets::iris dataset")
  
})







test_that("FM_index_permutation works", {
  set.seed(23235)
  ss <- TRUE # sample(1:150, 10 )
  hc1 <- hclust(dist(datasets::iris[ss, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[ss, -5]), "single")
  # dend1 <- as.dendrogram(hc1)
  # dend2 <- as.dendrogram(hc2)
  #    cutree(dend1)

  A1_clusters <- cutree(hc1, k = 3)
  A2_clusters <- A1_clusters

  R <- 10
  set.seed(414130)
  FM_index_H0 <- replicate(R, FM_index_permutation(A1_clusters, A2_clusters)) # can take 10 sec

  expect_identical(
    round(mean(FM_index_H0), 2),
    0.37
  )
  expect_identical(
    round(sd(FM_index_H0), 2),
    0.01
  )
})








test_that("Bk works", {
  set.seed(23235)
  ss <- TRUE # sample(1:150, 10 )
  hc1 <- hclust(dist(datasets::iris[ss, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[ss, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)
  #    cutree(tree1)

  expect_identical(
    Bk(hc1, hc2, k = 3),
    Bk(dend1, dend2, k = 3, )
  )

  expect_identical(
    Bk(hc1, hc2, k = 3),
    Bk(dend1, dend2, k = 3)
  )

  expect_identical(
    round(as.numeric(Bk(hc1, hc2, k = 3)), 3),
    0.806
  )
  expect_identical(
    round(as.numeric(Bk(dend1, dend2, k = 3)), 3),
    0.806
  )


  set.seed(23234535)
  expect_identical(
    round(
      Bk_permutations(dend1,
        dend2,
        k = 3, R = 2
      )[[1]], 2
    ),
    c(.45, .44)
  )

  # test case where k is not specified
  expect_identical(
    Bk(dend1, dend2),
    Bk(dend1, dend2, k = 2:(nleaves(dend1) - 1))
  )

  # test case where different number of items are provided
  hc1 <- hclust(dist(datasets::iris[1:149, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[1:150, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)
  expect_error(Bk(dend1, dend2, warn = T))
  
  # test case where different labels are provided
  hc1 <- hclust(dist(datasets::iris[1:149, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[2:150, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)
  expect_error(Bk(dend1, dend2, warn = T))
  
  # make computationally easier for permutations
  hc1 <- hclust(dist(datasets::iris[1:15, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[1:15, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)
  
  # test case where k is not specified
  expect_no_error(
     Bk_permutations(dend1, dend2)
  )
  
  # test case where different number of items are provided
  hc1 <- hclust(dist(datasets::iris[1:29, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[1:30, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)
  expect_error(Bk_permutations(dend1, dend2, warn = T))
  
  # test case where different labels are provided
  hc1 <- hclust(dist(datasets::iris[1:29, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[2:30, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)
  expect_error(Bk_permutations(dend1, dend2, warn = T))
})


test_that("Bk_plot works", {

  ss <- TRUE
  hc1 <- hclust(dist(datasets::iris[1:15, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[1:15, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)

  # simply check if the function can run without error
  expect_no_error(
    Bk_plot(dend1, dend2, try_cutree_hclust = T, add_E = T, rejection_line_asymptotic = T, rejection_line_permutation = T, p.adjust.methods = "bonferroni")
  )
  expect_no_error(
    Bk_plot(dend1, dend2, try_cutree_hclust = T, add_E = F, rejection_line_asymptotic = T, rejection_line_permutation = T)
  )
  
  # case where object which cant be converted to phylo is passed in
  x <- matrix(1:4, nrow = 2)
  expect_error(
    Bk_plot(x, dend2, try_cutree_hclust = T, add_E = F, rejection_line_asymptotic = T, rejection_line_permutation = T)
  )
  expect_error(
    Bk_plot(dend1, x, try_cutree_hclust = T, add_E = F, rejection_line_asymptotic = T, rejection_line_permutation = T)
  )
})
