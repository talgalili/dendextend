# Copyright (C) Tal Galili
#
# This file is part of dendextend.
#
# dendextend is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# dendextend is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#





#' @title Sorts two clusters vector by their names
#' @description Sorts two clusters vector by their names and returns a list with the sorted vectors.
#' @export
#' @param A1_clusters a numeric vector of cluster grouping (numeric) of items,
#' with a name attribute of item name for each element from group A1.
#' These are often obtained by using some k cut on a dendrogram.
#' @param A2_clusters a numeric vector of cluster grouping (numeric) of items,
#' with a name attribute of item name for each element from group A2.
#' These are often obtained by using some k cut on a dendrogram.
#' @param assume_sorted_vectors logical (FALSE). Can we assume to two group
#' vectors are sorter so that they have the same order of items?
#' IF FALSE (default), then the vectors will be sorted based on their
#' name attribute.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... Ignored.
#'
#' @return
#' A list with two elements, corresponding to the two clustering vectors.
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- sample(1:150, 4)
#' hc1 <- hclust(dist(iris[ss, -5]), "com")
#' hc2 <- hclust(dist(iris[ss, -5]), "single")
#' # dend1 <- as.dendrogram(hc1)
#' # dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)
#'
#' A1_clusters <- cutree(hc1, k = 3)
#' A2_clusters <- sample(cutree(hc1, k = 3))
#'
#' sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors = TRUE) # no sorting
#' sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors = FALSE) # Sorted
#' }
sort_2_clusters_vectors <- function(A1_clusters, A2_clusters, assume_sorted_vectors = FALSE, warn = dendextend_options("warn"), ...) {

  # sanity checks in case the names of the vectors do not make sense:
  if (!assume_sorted_vectors) {
    names_A1_clusters <- names(A1_clusters)
    names_A2_clusters <- names(A2_clusters)

    if (is.null(names_A1_clusters) || is.null(names_A2_clusters)) {
      if (warn) warning("Names of the clusters are NULL, we will assume the vectors are sorted.")
      assume_sorted_vectors <- TRUE
    }
    if (length(names_A1_clusters) != length(names_A2_clusters)) {
      if (warn) warning("Names of the clusters are not of equal length, we will assume the vectors are sorted.")
      assume_sorted_vectors <- TRUE
    }
    if (length(names_A1_clusters) != length(A1_clusters)) {
      if (warn) warning("Names of the clusters are not of equal length as that of the clusters, we will assume the vectors are sorted.")
      assume_sorted_vectors <- TRUE
    }
  }
  # if we are still NOT assuming the vectors are sorted - we can sort them...
  if (!assume_sorted_vectors) {
    A1_clusters <- A1_clusters[order(names_A1_clusters)] # order the vec accourding to the names, so to allow a comparison
    A2_clusters <- A2_clusters[order(names_A2_clusters)] # order the vec accourding to the names, so to allow a comparison
  }

  return(list(A1_clusters = A1_clusters, A2_clusters = A2_clusters))
}



#' @title Calculating Fowlkes-Mallows index in R
#' @export
#' @description
#'
#' Calculating Fowlkes-Mallows index.
#'
#' The \code{FM_index_R}
#' function also calculates the expectancy and variance of the FM Index
#' under the null hypothesis of no relation.
#'
#'
#' @param A1_clusters a numeric vector of cluster grouping (numeric) of items,
#' with a name attribute of item name for each element from group A1.
#' These are often obtained by using some k cut on a dendrogram.
#' @param A2_clusters a numeric vector of cluster grouping (numeric) of items,
#' with a name attribute of item name for each element from group A2.
#' These are often obtained by using some k cut on a dendrogram.
#' @param assume_sorted_vectors logical (FALSE). Can we assume to two group
#' vectors are sorter so that they have the same order of items?
#' IF FALSE (default), then the vectors will be sorted based on their
#' name attribute.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... Ignored.
#'
#' @details
#' From Wikipedia:
#'
#' Fowlkes-Mallows index (see references) is an external evaluation method
#' that is used to determine the similarity between two clusterings
#' (clusters obtained after a clustering algorithm). This measure of similarity
#' could be either between two hierarchical clusterings or a clustering and
#' a benchmark classification. A higher the value for the Fowlkes-Mallows index
#' indicates a greater similarity between the clusters and the benchmark
#' classifications.
#'
#' @seealso
#' \link{cor_bakers_gamma}
#' @return
#' The Fowlkes-Mallows index between two vectors of clustering groups.
#'
#' Includes the attributes E_FM and V_FM for the relevant expectancy and
#' variance under the null hypothesis of no-relation.
#'
#' @references
#'
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983).
#' "A Method for Comparing Two Hierarchical Clusterings".
#' Journal of the American Statistical Association 78 (383): 553.
#'
#' \url{http://en.wikipedia.org/wiki/Fowlkes-Mallows_index}
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- TRUE # sample(1:150, 10 )
#' hc1 <- hclust(dist(iris[ss, -5]), "com")
#' hc2 <- hclust(dist(iris[ss, -5]), "single")
#' # dend1 <- as.dendrogram(hc1)
#' # dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)
#'
#' FM_index_R(cutree(hc1, k = 3), cutree(hc1, k = 3)) # 1
#' set.seed(1341)
#' FM_index_R(cutree(hc1, k = 3),
#'            sample(cutree(hc1, k = 3)), 
#'            assume_sorted_vectors = TRUE) # 0.38037
#' FM_index_R(cutree(hc1, k = 3), 
#'            sample(cutree(hc1, k = 3)), 
#'            assume_sorted_vectors = FALSE) # 1 again :)
#' FM_index_R(cutree(hc1, k = 3), 
#'            cutree(hc2, k = 3)) # 0.8059
#' FM_index_R(cutree(hc1, k = 30), 
#'            cutree(hc2, k = 30)) # 0.4529
#'
#' fo <- function(k) FM_index_R(cutree(hc1, k), cutree(hc2, k))
#' lapply(1:4, fo)
#' ks <- 1:150
#' plot(sapply(ks, fo) ~ ks, type = "b", main = "Bk plot for the iris dataset")
#'
#' clu_1 <- cutree(hc2, k = 100) # this is a lie - since this one is NOT well defined!
#' clu_2 <- cutree(as.dendrogram(hc2), k = 100) # We see that we get a vector of NAs for this...
#'
#' FM_index_R(clu_1, clu_2) # NA
#' }
FM_index_R <- function(A1_clusters, A2_clusters, assume_sorted_vectors = FALSE, warn = dendextend_options("warn"), ...) {
  if (!assume_sorted_vectors) {
    sorted_As <- sort_2_clusters_vectors(A1_clusters, A2_clusters,
      assume_sorted_vectors = assume_sorted_vectors,
      warn = warn
    )
    A1_clusters <- sorted_As[[1]]
    A2_clusters <- sorted_As[[2]]
  }

  if (any(is.na(A1_clusters)) | any(is.na(A2_clusters))) {
    if (warn) warning("The clusterings have some NA's in them - returned NA.")
    FM_index <- NA
    attr(FM_index, "E_FM") <- NA
    attr(FM_index, "V_FM") <- NA
    return(FM_index)
  }



  # creating matrix M
  # ----------
  # a not-so-smart way
  # M <- matrix(0, nrow = k, ncol = k)
  # for(i in 1:k)
  # {
  # for(j in 1:k)
  # {
  # M[i,j] <- sum(rect.hclust(A1, k = k)[[i]] %in% rect.hclust(A2, k = k)[[j]])
  # how many of the objects in cluser i in tree A1, exist in cluster j in tree A2
  # }
  # }
  # ----------
  # a much better way!
  M <- table(A1_clusters, A2_clusters)
  # calculate n
  n <- length(A1_clusters)

  Tk <- sum(M^2) - n
  m_i. <- apply(M, 1, sum)
  m_.j <- apply(M, 2, sum)
  m_.. <- n # sum(M)
  if (sum(M) != n) stop("Why does M matrix doesn't sum up to n ??")
  Pk <- sum(m_i.^2) - n
  Qk <- sum(m_.j^2) - n

  FM <- Tk / sqrt(Pk * Qk)

  # Expectancy of the FM (according to H0)
  E_FM <- sqrt(Pk * Qk) / (n * (n - 1))


  Pk2 <- sum(m_i. * (m_i. - 1) * (m_i. - 2))
  Qk2 <- sum(m_.j * (m_.j - 1) * (m_.j - 2))
  # variance of the FM (according to H0)
  V_FM <- 2 / (n * (n - 1)) +
    4 * Pk2 * Qk2 / ((n * (n - 1) * (n - 2)) * Pk * Qk) +
    (Pk - 2 - 4 * Pk2 / Pk) * (Qk - 2 - 4 * Qk2 / Qk) / ((n * (n - 1) * (n - 2) * (n - 3))) -
    Pk * Qk / (n^2 * (n - 1)^2)


  FM_index <- FM # c(, E_FM, V_FM)
  attr(FM_index, "E_FM") <- E_FM
  attr(FM_index, "V_FM") <- V_FM

  return(FM_index)
}









#' @title Calculating Fowlkes-Mallows Index
#' @export
#' @description
#'
#' Calculating Fowlkes-Mallows index.
#'
#' The \code{FM_index_R} function calculates the expectancy and variance of the FM Index
#' under the null hypothesis of no relation.
#'
#'
#' @param A1_clusters a numeric vector of cluster grouping (numeric) of items,
#' with a name attribute of item name for each element from group A1.
#' These are often obtained by using some k cut on a dendrogram.
#' @param A2_clusters a numeric vector of cluster grouping (numeric) of items,
#' with a name attribute of item name for each element from group A2.
#' These are often obtained by using some k cut on a dendrogram.
#' @param assume_sorted_vectors logical (FALSE). Can we assume to two group
#' vectors are sorter so that they have the same order of items?
#' IF FALSE (default), then the vectors will be sorted based on their
#' name attribute.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... Ignored
#'
#' @details
#' From Wikipedia:
#'
#' Fowlkes-Mallows index (see references) is an external evaluation method
#' that is used to determine the similarity between two clusterings
#' (clusters obtained after a clustering algorithm). This measure of similarity
#' could be either between two hierarchical clusterings or a clustering and
#' a benchmark classification. A higher the value for the Fowlkes-Mallows index
#' indicates a greater similarity between the clusters and the benchmark
#' classifications.
#'
#' @seealso
#' \link{cor_bakers_gamma}
#' @return
#' The Fowlkes-Mallows index between two vectors of clustering groups.
#'
#' Includes the attributes E_FM and V_FM for the relevant expectancy and
#' variance under the null hypothesis of no-relation.
#'
#' @references
#'
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983).
#' "A Method for Comparing Two Hierarchical Clusterings".
#' Journal of the American Statistical Association 78 (383): 553.
#'
#' \url{http://en.wikipedia.org/wiki/Fowlkes-Mallows_index}
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- TRUE # sample(1:150, 10 )
#' hc1 <- hclust(dist(iris[ss, -5]), "com")
#' hc2 <- hclust(dist(iris[ss, -5]), "single")
#' # dend1 <- as.dendrogram(hc1)
#' # dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)
#'
#' FM_index(cutree(hc1, k = 3), cutree(hc1, k = 3)) # 1 with EV
#'
#' # checking speed gains
#' library(microbenchmark)
#' microbenchmark(
#'   FM_index(cutree(hc1, k = 3), cutree(hc1, k = 3)),
#'   FM_index(cutree(hc1, k = 3), cutree(hc1, k = 3),
#'     assume_sorted_vectors = TRUE
#'   ),
#'   FM_index(cutree(hc1, k = 3), cutree(hc1, k = 3),
#'     assume_sorted_vectors = TRUE
#'   )
#' )
#' # C code is 1.2-1.3 times faster.
#'
#' set.seed(1341)
#' FM_index(cutree(hc1, k = 3), sample(cutree(hc1, k = 3)),
#'   assume_sorted_vectors = TRUE
#' ) # 0.38037
#' FM_index(cutree(hc1, k = 3), sample(cutree(hc1, k = 3)),
#'   assume_sorted_vectors = FALSE
#' ) # 1 again :)
#' FM_index(cutree(hc1, k = 3), cutree(hc2, k = 3)) # 0.8059
#' FM_index(cutree(hc1, k = 30), cutree(hc2, k = 30)) # 0.4529
#'
#' fo <- function(k) FM_index(cutree(hc1, k), cutree(hc2, k))
#' lapply(1:4, fo)
#' ks <- 1:150
#' plot(sapply(ks, fo) ~ ks, type = "b", main = "Bk plot for the iris dataset")
#' }
FM_index <- function(A1_clusters, A2_clusters, assume_sorted_vectors = FALSE, warn = dendextend_options("warn"), ...) {
   FM <- FM_index_R(A1_clusters, A2_clusters,
                    assume_sorted_vectors = assume_sorted_vectors, warn = warn, ...
   )
   
  return(FM)
}







#' @title Calculating Fowlkes-Mallows Index under H0
#' @export
#' @description
#'
#' Calculating Fowlkes-Mallows index under the null hypothesis of no relation
#' between the clusterings (random order of the items labels).
#'
#' @param A1_clusters a numeric vector of cluster grouping (numeric) of items,
#' with a name attribute of item name for each element from group A1.
#' These are often obtained by using some k cut on a dendrogram.
#' @param A2_clusters a numeric vector of cluster grouping (numeric) of items,
#' with a name attribute of item name for each element from group A2.
#' These are often obtained by using some k cut on a dendrogram.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... Ignored
#'
#'
#' @seealso
#' \link{cor_bakers_gamma},
#' \code{\link{FM_index_R}}, \code{\link{FM_index}}
#'
#' @return
#' The Fowlkes-Mallows index between two vectors of clustering groups.
#' Under H0. (a double without attr)
#'
#' @references
#'
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983).
#' "A Method for Comparing Two Hierarchical Clusterings".
#' Journal of the American Statistical Association 78 (383): 553.
#'
#' \url{http://en.wikipedia.org/wiki/Fowlkes-Mallows_index}
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- TRUE # sample(1:150, 10 )
#' hc1 <- hclust(dist(iris[ss, -5]), "com")
#' hc2 <- hclust(dist(iris[ss, -5]), "single")
#' # dend1 <- as.dendrogram(hc1)
#' # dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)
#'
#' # small k
#' A1_clusters <- cutree(hc1, k = 3) # will give a right tailed distribution
#' # large k
#' A1_clusters <- cutree(hc1, k = 50) # will give a discrete distribution
#' # "medium" k
#' A1_clusters <- cutree(hc1, k = 25) # gives almost the normal distribution!
#' A2_clusters <- A1_clusters
#'
#' R <- 10000
#' set.seed(414130)
#' FM_index_H0 <- replicate(R, FM_index_permutation(A1_clusters, A2_clusters)) # can take 10 sec
#' plot(density(FM_index_H0), main = "FM Index distribution under H0\n (10000 permutation)")
#' abline(v = mean(FM_index_H0), col = 1, lty = 2)
#' # The permutation distribution is with a heavy right tail:
#' library(psych)
#' skew(FM_index_H0) # 1.254
#' kurtosi(FM_index_H0) # 2.5427
#'
#' mean(FM_index_H0)
#' var(FM_index_H0)
#' the_FM_index <- FM_index(A1_clusters, A2_clusters)
#' the_FM_index
#' our_dnorm <- function(x) {
#'   dnorm(x,
#'     mean = attr(the_FM_index, "E_FM"),
#'     sd = sqrt(attr(the_FM_index, "V_FM"))
#'   )
#' }
#' # our_dnorm(0.35)
#' curve(our_dnorm,
#'   col = 4,
#'   from = -1, to = 1, n = R, add = TRUE
#' )
#' abline(v = attr(the_FM_index, "E_FM"), col = 4, lty = 2)
#'
#' legend("topright", legend = c("asymptotic", "permutation"), fill = c(4, 1))
#' }
FM_index_permutation <- function(A1_clusters, A2_clusters, warn = dendextend_options("warn"), ...) {
  return(
    as.vector(FM_index(sample(A1_clusters),
      sample(A2_clusters),
      assume_sorted_vectors = TRUE, warn = warn, ...
    ))
  )
}



















#' @title Bk - Calculating Fowlkes-Mallows Index for two dendrogram
#' @export
#' @description
#'
#' Bk is the calculation of Fowlkes-Mallows index for a series of k cuts
#' for two dendrograms.
#'
#' @param tree1 a dendrogram/hclust/phylo object.
#' @param tree2 a dendrogram/hclust/phylo object.
#' @param k an integer scalar or vector with the desired number
#' of cluster groups.
#' If missing - the Bk will be calculated for a default k range of
#' 2:(nleaves-1).
#' No point in checking k=1/k=n, since both will give Bk=1.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... Ignored (passed to FM_index_R).
#'
#' @details
#' From Wikipedia:
#'
#' Fowlkes-Mallows index (see references) is an external evaluation method
#' that is used to determine the similarity between two clusterings
#' (clusters obtained after a clustering algorithm). This measure of similarity
#' could be either between two hierarchical clusterings or a clustering and
#' a benchmark classification. A higher the value for the Fowlkes-Mallows index
#' indicates a greater similarity between the clusters and the benchmark
#' classifications.
#'
#' @seealso
#' \link{FM_index}, \link{cor_bakers_gamma}, \link{Bk_plot}
#' @return
#' A list (of k's length) of Fowlkes-Mallows index between two dendrogram for
#' a scalar/vector of k values.
#' The names of the lists' items is the k for which it was calculated.
#'
#' @references
#'
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983).
#' "A Method for Comparing Two Hierarchical Clusterings".
#' Journal of the American Statistical Association 78 (383): 553.
#'
#' \url{http://en.wikipedia.org/wiki/Fowlkes-Mallows_index}
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- TRUE # sample(1:150, 10 )
#' hc1 <- hclust(dist(iris[ss, -5]), "com")
#' hc2 <- hclust(dist(iris[ss, -5]), "single")
#' tree1 <- as.dendrogram(hc1)
#' tree2 <- as.dendrogram(hc2)
#' #    cutree(tree1)
#'
#' Bk(hc1, hc2, k = 3)
#' Bk(hc1, hc2, k = 2:10)
#' Bk(hc1, hc2)
#'
#' Bk(tree1, tree2, k = 3)
#' Bk(tree1, tree2, k = 2:5)
#'
#' system.time(Bk(hc1, hc2, k = 2:5)) # 0.01
#' system.time(Bk(hc1, hc2)) # 1.28
#' system.time(Bk(tree1, tree2, k = 2:5)) # 0.24 # after fixes.
#' system.time(Bk(tree1, tree2, k = 2:10)) # 0.31 # after fixes.
#' system.time(Bk(tree1, tree2)) # 7.85
#' Bk(tree1, tree2, k = 99:101)
#'
#' y <- Bk(hc1, hc2, k = 2:10)
#' plot(unlist(y) ~ c(2:10), type = "b", ylim = c(0, 1))
#'
#' # can take a few seconds
#' y <- Bk(hc1, hc2)
#' plot(unlist(y) ~ as.numeric(names(y)),
#'   main = "Bk plot", pch = 20,
#'   xlab = "k", ylab = "FM Index",
#'   type = "b", ylim = c(0, 1)
#' )
#' # we are still missing some hypothesis testing here.
#' # for this we'll have the Bk_plot function.
#' }
Bk <- function(tree1, tree2, k, warn = dendextend_options("warn"), ...) {

  # some sanity checks!
  if (warn) { # the sanity checks are turned off by default since the "labels" function for dendrogram is one which takes some time to run...
    # notice that we must have labels.hclust and labels.dendrogram defined!
    tree1_labels <- labels(tree1)
    tree2_labels <- labels(tree2)
    length_tree1_labels <- length(tree1_labels)
    length_tree2_labels <- length(tree2_labels)

    # Checking for common error options:
    if (length_tree1_labels != length_tree2_labels) stop("The two clusters don't have the same number of items!") # If cluster sized are different - stop
    if (!all(sort(tree1_labels) == sort(tree2_labels))) stop("Your trees are having leaves with different names - please correct it in order to use this function")
  }


  if (missing(k)) k <- 2:(nleaves(tree1) - 1)
  cutree_tree1 <- cutree(tree1, k, NA_to_0L = FALSE)
  cutree_tree2 <- cutree(tree2, k, NA_to_0L = FALSE)
  # makes sure the output is a matrix:
  # This is if length(k)==1 since in that case
  # the output would be a vector, not a matrix.
  if (length(k) == 1) {
    cutree_tree1 <- as.matrix(cutree_tree1)
    cutree_tree2 <- as.matrix(cutree_tree2)
  }
  # example of such a case:
  #    a = cutree(tree1, k=1:2)
  #    is.vector(a)
  #    is.matrix(a)
  #    a = as.matrix(a)
  #    is.vector(a)
  #    is.matrix(a)

  n_ks <- ncol(cutree_tree1)

  Bk_for_each_k <- function(i_k) {
    FM_index(
      cutree_tree1[, i_k], cutree_tree2[, i_k],
      assume_sorted_vectors = FALSE,
      # We can't trust cutree to give the same order of items!
      # In order to assume it, we would need to match order by labels
      # and then have cutree( ) with order_clusters_as_data=TRUE
      # but for small length of k's, this per-process (/checks)
      # will likely be more expensive than simply running it with
      # assume_sorted_vectors = FALSE,
      warn = warn
    )
  }

  the_Bks <- lapply(seq_len(n_ks), Bk_for_each_k)
  names(the_Bks) <- k

  return(the_Bks)
}






#' @title Bk permutation - Calculating Fowlkes-Mallows Index for two dendrogram
#' @export
#' @description
#'
#' Bk is the calculation of Fowlkes-Mallows index for a series of k cuts
#' for two dendrograms.
#'
#' Bk permutation calculates the Bk under the null hypothesis of no similarirty
#' between the two trees by randomally shuffling the labels of the two trees
#' and calculating their Bk.
#' @param tree1 a dendrogram/hclust/phylo object.
#' @param tree2 a dendrogram/hclust/phylo object.
#' @param k an integer scalar or vector with the desired number
#' of cluster groups.
#' If missing - the Bk will be calculated for a default k range of
#' 2:(nleaves-1).
#' No point in checking k=1/k=n, since both will give Bk=1.
#' @param R integer (Default is 1000). The number of Bk permutation to perform for each k.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' If set to TRUE, extra checks are made to varify that the two clusters have
#' the same size and the same labels.
#' @param ... Ignored (passed to FM_index_R).
#'
#' @details
#' From Wikipedia:
#'
#' Fowlkes-Mallows index (see references) is an external evaluation method
#' that is used to determine the similarity between two clusterings
#' (clusters obtained after a clustering algorithm). This measure of similarity
#' could be either between two hierarchical clusterings or a clustering and
#' a benchmark classification. A higher the value for the Fowlkes-Mallows index
#' indicates a greater similarity between the clusters and the benchmark
#' classifications.
#'
#' @seealso
#' \code{\link{FM_index}}, \link{Bk}
#' @return
#' A list (of the length of k's), where each element of the list has
#' R (number of permutations) calculations of Fowlkes-Mallows index
#' between two dendrogram after having their labels shuffled.
#'
#' The names of the lists' items is the k for which it was calculated.
#'
#' @references
#'
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983).
#' "A Method for Comparing Two Hierarchical Clusterings".
#' Journal of the American Statistical Association 78 (383): 553.
#'
#' \url{http://en.wikipedia.org/wiki/Fowlkes-Mallows_index}
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- TRUE # sample(1:150, 10 )
#' hc1 <- hclust(dist(iris[ss, -5]), "com")
#' hc2 <- hclust(dist(iris[ss, -5]), "single")
#' # tree1 <- as.treerogram(hc1)
#' # tree2 <- as.treerogram(hc2)
#' #    cutree(tree1)
#'
#' some_Bk <- Bk(hc1, hc2, k = 20)
#' some_Bk_permu <- Bk_permutations(hc1, hc2, k = 20)
#'
#' # we can see that the Bk is much higher than the permutation Bks:
#' plot(
#'   x = rep(1, 1000), y = some_Bk_permu[[1]],
#'   main = "Bk distribution under H0",
#'   ylim = c(0, 1)
#' )
#' points(1, y = some_Bk, pch = 19, col = 2)
#' }
Bk_permutations <- function(tree1, tree2, k, R = 1000, warn = dendextend_options("warn"), ...) {

  # some sanity checks!
  if (warn) { # the sanity checks are turned off by default since the "labels" function for dendrogram is one which takes some time to run...
    # notice that we must have labels.hclust and labels.dendrogram defined!
    tree1_labels <- labels(tree1)
    tree2_labels <- labels(tree2)
    length_tree1_labels <- length(tree1_labels)
    length_tree2_labels <- length(tree2_labels)

    # Checking for common error options:
    if (length_tree1_labels != length_tree2_labels) stop("The two clusters don't have the same number of items!") # If cluster sized are different - stop
    if (!all(sort(tree1_labels) == sort(tree2_labels))) stop("Your trees are having leaves with different names - please correct it in order to use this function")
  }

  Bk_permutations_for_each_k <- function(k) {
    A1_clusters <- cutree(tree1, k)
    A2_clusters <- cutree(tree2, k)

    FM_index_H0 <- replicate(R, FM_index_permutation(A1_clusters, A2_clusters, warn = warn)) # can take 10 sec

    return(FM_index_H0)
  }

  if (missing(k)) k <- 2:(nleaves(tree1) - 1)
  the_Bks_permutations <- lapply(k, Bk_permutations_for_each_k)
  names(the_Bks_permutations) <- k

  return(the_Bks_permutations)
}










#' @title Bk plot - ploting the Fowlkes-Mallows Index of two dendrogram for various k's
#' @export
#' @description
#'
#' Bk is the calculation of Fowlkes-Mallows index for a series of k cuts
#' for two dendrograms.
#' A Bk plot is simply a scatter plot of Bk versus k.
#' This plot helps in identifiying the similarity between two dendrograms in
#' different levels of k (number of clusters).
#'
#' @param tree1 a dendrogram/hclust/phylo object.
#' @param tree2 a dendrogram/hclust/phylo object.
#' @param k an integer scalar or vector with the desired number
#' of cluster groups.
#' If missing - the Bk will be calculated for a default k range of
#' 2:(nleaves-1).
#' No point in checking k=1/k=n, since both will give Bk=1.
#' @param add_E logical (TRUE). Should we add a line of the Expected Bk value
#' for each k, under the null hypothesis of no relation between the clusterings?
#' @param rejection_line_asymptotic logical (TRUE). Should we add a line of
#' the one sided rejection region based on the asymptotic distribution
#' of Bk values, for each k, under the null hypothesis of no relation
#' between the clusterings?
#' @param rejection_line_permutation logical (FALSE). Should we add a line of
#' the one sided rejection region based on the asymptotic distribution
#' of Bk values, for each k, under the null hypothesis of no relation
#' between the clusterings?
#' @param R integer (Default is 1000). The number of Bk permutation to perform
#' for each k. Applicable only if rejection_line_permutation is TRUE.
#' @param k_permutation the k's to be used for permutation (sometimes we might
#' be only interested in some k's and it is not important to run the simulation
#' for all possible ks). If missing - k itself will be used.
#' @param conf.level the level of one sided confidence interval used for creation
#' of the rejection lines.
#' @param p.adjust.methods a character scalar of either "none" (default), or
#' "bonferroni". This controls the multiple correction method to use for the
#' critical rejection values. Currently only the Bonferroni method
#' is implemented (based on the number of different k values).
#' @param col_line_Bk the color of the Bk line.
#' @param col_line_asymptotic the color of the rejection asymptotic Bk line.
#' @param col_line_permutation the color of the rejection asymptotic Bk line.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' If set to TRUE, extra checks are made to varify that the two clusters have
#' the same size and the same labels.
#' @param main passed to \link{plot}.
#' @param xlab passed to \link{plot}.
#' @param ylab passed to \link{plot}.
#' @param xlim passed to \link{plot}. If missign, xlim is from 2 to nleaves-1
#' @param ylim passed to \link{plot}.
#' @param try_cutree_hclust logical (TRUE). Since cutree for hclust is MUCH
#' faster than for dendrogram - Bk_plot will first try to change the dendrogram
#' into an hclust object. If it will fail (for example, with unbranched trees),
#' it will continue using the cutree.dendrogram functions.
#' If try_cutree_hclust=FALSE, it will force to use cutree.dendrogram and
#' not cutree.hclust.
#' @param ... Ignored.
#'
#' @details
#' From Wikipedia:
#'
#' Fowlkes-Mallows index (see references) is an external evaluation method
#' that is used to determine the similarity between two clusterings
#' (clusters obtained after a clustering algorithm). This measure of similarity
#' could be either between two hierarchical clusterings or a clustering and
#' a benchmark classification. A higher the value for the Fowlkes-Mallows index
#' indicates a greater similarity between the clusters and the benchmark
#' classifications.
#'
#' The default Bk plot comes with a line with dots (type "b") of the Bk values.
#' Also with a fragmented (lty=2) line (of the same color) of the expected Bk
#' line under H0,
#' And a solid red line of the upper critical Bk values for rejection
#'
#'
#' @seealso
#' \code{\link{FM_index}}, \link{Bk}, \link{Bk_permutations}
#' @return
#' After plotting the Bk plot.
#' Returns (invisible) the output of the elements used for constructing the plot:
#' The Bk values, Bk permutations (if used), Bk theoratical values, etc.
#'
#' @references
#'
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983).
#' "A Method for Comparing Two Hierarchical Clusterings".
#' Journal of the American Statistical Association 78 (383): 553.
#'
#' \url{http://en.wikipedia.org/wiki/Fowlkes-Mallows_index}
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- TRUE # sample(1:150, 10 )
#' hc1 <- hclust(dist(iris[ss, -5]), "com")
#' hc2 <- hclust(dist(iris[ss, -5]), "single")
#' # tree1 <- as.treerogram(hc1)
#' # tree2 <- as.treerogram(hc2)
#' #    cutree(tree1)
#'
#' Bk_plot(hc1, hc2, k = 2:20, xlim = c(2, 149))
#' Bk_plot(hc1, hc2)
#'
#' Bk_plot(hc1, hc2, k = 3)
#' Bk_plot(hc1, hc2, k = 3:10)
#' Bk_plot(hc1, hc2)
#' Bk_plot(hc1, hc2, p.adjust.methods = "bonferroni") # higher rejection lines
#'
#' # this one can take a bit of time:
#' Bk_plot(hc1, hc2,
#'   rejection_line_permutation = TRUE,
#'   k_permutation = c(2, 4, 6, 8, 10, 20, 30, 40, 50), R = 100
#' )
#' # we can see that the permutation line is VERY close to the asymptotic line.
#' # This is great since it means one can often use the asymptotic results
#' # Without having to do many simulations.
#'
#' # works just as well for dendrograms:
#' dend1 <- as.dendrogram(hc1)
#' dend2 <- as.dendrogram(hc2)
#' Bk_plot(dend1, dend2, k = 2:3, try_cutree_hclust = FALSE) # slower than hclust, but works...
#' Bk_plot(hc1, dend2, k = 2:3, try_cutree_hclust = FALSE) # slower than hclust, but works...
#' Bk_plot(dend1, dend1, k = 2:3, try_cutree_hclust = TRUE) # slower than hclust, but works...
#' Bk_plot(hc1, hc1, k = 2:3) # slower than hclust, but works...
#' # for some reason it can't turn dend2 back to hclust :(
#' a <- Bk_plot(hc1, hc2, k = 2:3, try_cutree_hclust = TRUE) # slower than hclust, but works...
#'
#' hc1_mixed <- as.hclust(sample(as.dendrogram(hc1)))
#' Bk_plot(
#'   tree1 = hc1, tree2 = hc1_mixed,
#'   add_E = FALSE,
#'   rejection_line_permutation = TRUE, k_permutation = c(2, 4, 6, 8, 10, 20, 30, 40, 50), R = 100
#' )
#' }
Bk_plot <- function(tree1, tree2, k,
                    add_E = TRUE,
                    rejection_line_asymptotic = TRUE,
                    rejection_line_permutation = FALSE,
                    R = 1000,
                    k_permutation,
                    conf.level = 0.95,
                    p.adjust.methods = c("none", "bonferroni"),
                    col_line_Bk = 1,
                    col_line_asymptotic = 2,
                    col_line_permutation = 4,
                    warn = dendextend_options("warn"),
                    main = "Bk plot",
                    xlab = "k (number of clusters)",
                    ylab = "Bk (Fowlkes-Mallows Index)",
                    xlim,
                    ylim = c(0, 1),
                    try_cutree_hclust = TRUE,
                    ...) {
  if (try_cutree_hclust) {
    # if we succeed (tryCatch) in turning it into hclust - use it!
    # if not - go on with the function.
    if (!is.hclust(tree1)) {
      tree1_hc <- tryCatch(as.hclust(tree1), error = function(e) {
        FALSE
      })
    }
    if (!is.hclust(tree2)) {
      tree2_hc <- tryCatch(as.hclust(tree2), error = function(e) {
        FALSE
      })
    }

    # only if we got BOTH trees to be hclust - can be put them in...

    if ((exists("tree1_hc") & exists("tree2_hc")) &&
      !(is.logical(tree1_hc) | is.logical(tree2_hc))) {
      tree1 <- tree1_hc
      tree2 <- tree2_hc
    }
  }

  output <- list()
  if (missing(k)) k <- 2:(nleaves(tree1) - 1)
  the_Bks <- Bk(tree1, tree2,
    k = k,
    warn = warn
  )
  output[length(output) + 1] <- list(Bk = the_Bks)
  if (missing(xlim)) xlim <- c(2, c(nleaves(tree1) - 1))

  #    k and as.numeric(names(the_Bks)) should be THE SAME
  plot(unlist(the_Bks) ~ k,
    main = main, xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim,
    col = col_line_Bk,
    pch = 20, type = "b"
  )


  if (add_E) {
    the_Bks_E <- sapply(the_Bks, function(x) attr(x, "E_FM"))
    lines(the_Bks_E ~ k,
      col = col_line_Bk,
      type = "l", lty = 2, lwd = 2
    )
    # the points are added for the case we have NA's in the Bk
    points(the_Bks_E ~ k,
      col = col_line_Bk,
      pch = 19, cex = .01
    )
  }

  p.adjust.methods <- match.arg(p.adjust.methods)
  if (p.adjust.methods == "bonferroni") {
    alfa <- 1 - conf.level
    conf.level <- 1 - alfa / length(k)
  }

  if (rejection_line_asymptotic) {
    if (!exists("the_Bks_E")) the_Bks_E <- sapply(the_Bks, function(x) attr(x, "E_FM"))
    the_Bks_V <- sapply(the_Bks, function(x) attr(x, "V_FM"))

    the_Bks_SD <- sqrt(the_Bks_V)
    Z <- qnorm(conf.level)

    Bk_critical_value_asymptotic <- the_Bks_E + Z * the_Bks_SD

    lines(Bk_critical_value_asymptotic ~ k,
      col = col_line_asymptotic,
      pch = 20, type = "l", lty = 1, lwd = 2, cex = .5
    )
    # the points are added for the case we have NA's in the Bk
    points(Bk_critical_value_asymptotic ~ k,
      col = col_line_asymptotic,
      pch = 19, cex = .01
    )
    output[length(output) + 1] <- list(Bk_critical_value_asymptotic = Bk_critical_value_asymptotic)
  }

  if (rejection_line_permutation) {
    if (missing(k_permutation)) k_permutation <- k
    some_Bk_permu <- Bk_permutations(tree1, tree2, k = k_permutation, R = R, warn = warn)
    # we can see that the Bk is much higher than the permutation Bks:
    Bk_critical_value_permu <- unname(sapply(some_Bk_permu, quantile, probs = conf.level))
    lines(Bk_critical_value_permu ~ k_permutation,
      col = col_line_permutation,
      pch = 20, type = "l", lty = 1, lwd = 2, cex = .5
    )
    output[length(output) + 1] <- list(Bk_permutations = some_Bk_permu)
    output[length(output) + 1] <- list(Bk_critical_value_permu = Bk_critical_value_permu)
  }


  return(invisible(output))
}



# Bk_plot(tree1, tree2, k = 30:50)
# Bk_plot(tree1, tree2)

# The Bk function was previously also implemented by Matt in:
#   	 https://cran.r-project.org/package=profdpm
# See pages 9 and 10 here: https://CRAN.R-project.org/package=profdpm/vignettes/profdpm.pdf
# As of February 2020 the package went off CRAN, so I removed it also from dendextend.
# I came by this package thanks to chl: http://stats.stackexchange.com/questions/3672/a-measure-to-describe-the-distribution-of-a-dendrogram
# Also, there is a great overview of similarity measures on this here:
# http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.164.6189&rep=rep1&type=pdf
