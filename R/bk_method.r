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
#' @param warn logical (TRUE). Should a warning be issued in case of problems?
#' @param ... Ignored.
#' 
#' @seealso
#' \link{FM_index_profdpm}
#' @return 
#' A list with two elements, corrosponding two the two clustering vectors.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 4 )
#' hc1 <- hclust(dist(iris[ss,-5]), "com")
#' hc2 <- hclust(dist(iris[ss,-5]), "single")
#' # dend1 <- as.dendrogram(hc1)
#' # dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)   
#' 
#' A1_clusters <- cutree(hc1, k=3)
#' A2_clusters <- sample(cutree(hc1, k=3))
#' 
#' sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors =TRUE) # no sorting
#' sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors =FALSE) # Sorted
#' 
#' 
#' }
sort_2_clusters_vectors <- function(A1_clusters, A2_clusters, assume_sorted_vectors =FALSE, warn = TRUE, ...) {
   
   # sanity checks in case the names of the vectors do not make sense:
   if(!assume_sorted_vectors) {      
      names_A1_clusters <- names(A1_clusters)
      names_A2_clusters <- names(A2_clusters)
      
      if(is.null(names_A1_clusters) || is.null(names_A2_clusters)) {
         if(warn) warning("Names of the clusters are NULL, we will assume the vectors are sorted.")
         assume_sorted_vectors <- TRUE
      }
      if(length(names_A1_clusters) != length(names_A2_clusters)) {
         if(warn) warning("Names of the clusters are not of equal length, we will assume the vectors are sorted.")
         assume_sorted_vectors <- TRUE
      }
      if(length(names_A1_clusters) != length(A1_clusters)) {
         if(warn) warning("Names of the clusters are not of equal length as that of the clusters, we will assume the vectors are sorted.")
         assume_sorted_vectors <- TRUE
      }
   }         
   # if we are still NOT assuming the vectors are sorted - we can sort them...
   if(!assume_sorted_vectors) {
      A1_clusters <- A1_clusters[order(names_A1_clusters)]   # order the vec accourding to the names, so to allow a comparison
      A2_clusters <- A2_clusters[order(names_A2_clusters)]   # order the vec accourding to the names, so to allow a comparison
   }
   
   return(list(A1_clusters=A1_clusters, A2_clusters=A2_clusters))
}
   



#' @title Calculating Fowlkes-Mallows index using the profdpm R package
#' @export
#' @description
#' 
#' Calculating Fowlkes-Mallows index using the \code{pci} function from
#' the profdpm R package. This function uses C code (thanks to
#' Matthew Shotwell's work) and is a bit faster than the R code 
#' (from my simple tests - it is about 1.2-1.3 times faster - which is not much
#' but it might be useful).
#' 
#' As opposed to the \code{\link{FM_index_R}} function, the \code{FM_index_profdpm}
#' function does NOT calculate the expectancy or the variance of the FM Index
#' under the null hypothesis of no relation.
#' 
#' This function also allows us to compare our calculations with an independent
#' writing of a function calculating the same statistic.
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
#' @param warn logical (TRUE). Should a warning be issued in case of problems?
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
#' @references
#' 
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983).
#' "A Method for Comparing Two Hierarchical Clusterings".
#' Journal of the American Statistical Association 78 (383): 553.
#' 
#' Shotwell, Matthew S. "profdpm: An R Package for MAP Estimation in a Class
#' of Conjugate Product Partition Models."
#' Journal of Statistical Software 53: 1-18.
#' 
#' \url{http://en.wikipedia.org/wiki/Fowlkes-Mallows_index}
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' set.seed(23235)
#' ss <- TRUE # sample(1:150, 10 )
#' hc1 <- hclust(dist(iris[ss,-5]), "com")
#' hc2 <- hclust(dist(iris[ss,-5]), "single")
#' # dend1 <- as.dendrogram(hc1)
#' # dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)   
#' 
#' FM_index_profdpm(cutree(hc1, k=3), cutree(hc1, k=3)) # 1
#' set.seed(1341)
#' FM_index_profdpm(cutree(hc1, k=3), sample(cutree(hc1, k=3)), assume_sorted_vectors =TRUE) # 0.38037
#' FM_index_profdpm(cutree(hc1, k=3), sample(cutree(hc1, k=3)), assume_sorted_vectors =FALSE) # 1 again :)
#' FM_index_profdpm(cutree(hc1, k=3), cutree(hc2, k=3)) # 0.8059
#' FM_index_profdpm(cutree(hc1, k=30), cutree(hc2, k=30)) # 0.4529
#' 
#' fo <- function(k) FM_index_profdpm(cutree(hc1, k), cutree(hc2, k)) 
#' lapply(1:4, fo)
#' ks <- 1:150
#' plot(sapply(ks, fo)~ ks, type = "b", main = "Bk plot for the iris dataset")
#' 
#' }
FM_index_profdpm <- function(A1_clusters, A2_clusters, assume_sorted_vectors =FALSE, warn = TRUE, ...) {
   if(!require("profdpm")) {
      if(warn) warning("The 'profdpm' package is not installed. Reverting to using the 'FM_index_R' function.")
      return(FM_index_R(A1_clusters, A2_clusters,assume_sorted_vectors = assume_sorted_vectors, warn= warn,...))
   }
   # see page 9 and 10, here:
   # http://ftp.daum.net/CRAN/web/packages/profdpm/vignettes/profdpm.pdf
   
   if(!assume_sorted_vectors) {
      sorted_As <- sort_2_clusters_vectors(A1_clusters, A2_clusters,
                                           assume_sorted_vectors =assume_sorted_vectors,
                                           warn = warn)   
      A1_clusters <- sorted_As[[1]]
      A2_clusters <- sorted_As[[2]]
   }

   FM_index <- unname(pci(A1_clusters,A2_clusters)[2])
   attr(FM_index, "E_FM") <- NA
   attr(FM_index, "V_FM") <- NA
   
   return(FM_index)
}











#' @title Calculating Fowlkes-Mallows index in R
#' @export
#' @description
#' 
#' Calculating Fowlkes-Mallows index.
#' 
#' As opposed to the \code{\link{FM_index_profdpm}} function, the \code{FM_index_R}
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
#' @param warn logical (TRUE). Should a warning be issued in case of problems?
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
#' \link{cor_bakers_gamma}, \code{\link{FM_index_profdpm}}
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
#' hc1 <- hclust(dist(iris[ss,-5]), "com")
#' hc2 <- hclust(dist(iris[ss,-5]), "single")
#' # dend1 <- as.dendrogram(hc1)
#' # dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)   
#' 
#' FM_index_R(cutree(hc1, k=3), cutree(hc1, k=3)) # 1
#' set.seed(1341)
#' FM_index_R(cutree(hc1, k=3), sample(cutree(hc1, k=3)), assume_sorted_vectors =TRUE) # 0.38037
#' FM_index_R(cutree(hc1, k=3), sample(cutree(hc1, k=3)), assume_sorted_vectors =FALSE) # 1 again :)
#' FM_index_R(cutree(hc1, k=3), cutree(hc2, k=3)) # 0.8059
#' FM_index_R(cutree(hc1, k=30), cutree(hc2, k=30)) # 0.4529
#' 
#' fo <- function(k) FM_index_R(cutree(hc1, k), cutree(hc2, k)) 
#' lapply(1:4, fo)
#' ks <- 1:150
#' plot(sapply(ks, fo)~ ks, type = "b", main = "Bk plot for the iris dataset")
#' 
#' }
FM_index_R <- function(A1_clusters, A2_clusters, assume_sorted_vectors =FALSE, warn = TRUE, ...) {
   
   if(!assume_sorted_vectors) {
      sorted_As <- sort_2_clusters_vectors(A1_clusters, A2_clusters,
                                           assume_sorted_vectors =assume_sorted_vectors,
                                           warn = warn)   
      A1_clusters <- sorted_As[[1]]
      A2_clusters <- sorted_As[[2]]
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
   if(sum(M) != n ) stop("Why does M matrix doesn't sum up to n ??")
   Pk <- sum(m_i.^2) - n
   Qk <- sum(m_.j^2) - n
   
   FM <- Tk / sqrt(Pk*Qk)
   
   # Expectancy of the FM (according to H0)
   E_FM <- sqrt(Pk*Qk)/ (n*(n-1))
   
   
   Pk2 <- sum( m_i. * (m_i. -1) * (m_i. -2) )
   Qk2 <- sum( m_.j * (m_.j -1) * (m_.j -2) )
   # variance of the FM (according to H0)
   V_FM <- 2/ (n*(n-1))  +  
      4*Pk2 * Qk2 / ( (n*(n-1)*(n-2)) * Pk*Qk ) + 
      (Pk -2 - 4*Pk2/Pk) * (Qk -2 - 4*Qk2/Qk) / ( (n*(n-1)*(n-2)*(n-3))) -
      Pk * Qk / (n^2*(n-1)^2)
   
   
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
#' As opposed to the \code{\link{FM_index_profdpm}} function, the \code{FM_index_R}
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
#' @param include_EV logical (TRUE). Should we calculate expectancy and variance
#' of the FM Index under null hypothesis of no relation between the clusterings?
#' If TRUE (Default) - then the \link{FM_index_R} function, else (FALSE)
#' we use the (faster) \link{FM_index_profdpm} function.
#' @param assume_sorted_vectors logical (FALSE). Can we assume to two group 
#' vectors are sorter so that they have the same order of items?
#' IF FALSE (default), then the vectors will be sorted based on their
#' name attribute.
#' @param warn logical (TRUE). Should a warning be issued in case of problems?
#' @param ... Ignored (passed to FM_index_R/FM_index_profdpm).
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
#' \link{cor_bakers_gamma}, \code{\link{FM_index_profdpm}}
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
#' hc1 <- hclust(dist(iris[ss,-5]), "com")
#' hc2 <- hclust(dist(iris[ss,-5]), "single")
#' # dend1 <- as.dendrogram(hc1)
#' # dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)   
#' 
#' FM_index(cutree(hc1, k=3), cutree(hc1, k=3)) # 1 with EV
#' FM_index(cutree(hc1, k=3), cutree(hc1, k=3), include_EV= FALSE) # 1
#' 
#' # checking speed gains
#' require(microbenchmark)
#'  microbenchmark(FM_index(cutree(hc1, k=3), cutree(hc1, k=3)),
#'                  FM_index(cutree(hc1, k=3), cutree(hc1, k=3), include_EV= FALSE),
#'                   FM_index(cutree(hc1, k=3), cutree(hc1, k=3), include_EV= TRUE, assume_sorted_vectors=TRUE),
#'                   FM_index(cutree(hc1, k=3), cutree(hc1, k=3), include_EV= FALSE, assume_sorted_vectors=TRUE)
#'                                                   )
#' # C code is 1.2-1.3 times faster.                                                   
#'                                                   
#' set.seed(1341)
#' FM_index(cutree(hc1, k=3), sample(cutree(hc1, k=3)), assume_sorted_vectors =TRUE) # 0.38037
#' FM_index(cutree(hc1, k=3), sample(cutree(hc1, k=3)), assume_sorted_vectors =FALSE) # 1 again :)
#' FM_index(cutree(hc1, k=3), cutree(hc2, k=3)) # 0.8059
#' FM_index(cutree(hc1, k=30), cutree(hc2, k=30)) # 0.4529
#' 
#' fo <- function(k) FM_index(cutree(hc1, k), cutree(hc2, k)) 
#' lapply(1:4, fo)
#' ks <- 1:150
#' plot(sapply(ks, fo)~ ks, type = "b", main = "Bk plot for the iris dataset")
#' 
#' }
FM_index <- function(A1_clusters, A2_clusters, include_EV = TRUE, assume_sorted_vectors =FALSE, warn = TRUE, ...) {

   if(include_EV) {
      FM <- FM_index_R(A1_clusters, A2_clusters, 
                       assume_sorted_vectors =assume_sorted_vectors, warn = warn, ...)
   } else {
      FM <- FM_index_profdpm(A1_clusters, A2_clusters, 
                       assume_sorted_vectors =assume_sorted_vectors, warn = warn, ...)      
   }  
   
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
#' @param warn logical (TRUE). Should a warning be issued in case of problems?
#' @param ... Ignored (passed to FM_index_R/FM_index_profdpm).
#' 
#' 
#' @seealso
#' \link{cor_bakers_gamma}, \code{\link{FM_index_profdpm}}, 
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
#' hc1 <- hclust(dist(iris[ss,-5]), "com")
#' hc2 <- hclust(dist(iris[ss,-5]), "single")
#' # dend1 <- as.dendrogram(hc1)
#' # dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)   
#' 
#' A1_clusters <- cutree(hc1, k=3)
#' A2_clusters <- A1_clusters
#' 
#' R <- 10000
#' set.seed(414130)
#' FM_index_H0 <- replicate(R, FM_index_permutation(A1_clusters, A2_clusters)) # can take 10 sec
#' plot(density(FM_index_H0), main = "FM Index distribution under H0\n (10000 permutation)")
#' abline(v = mean(FM_index_H0), col = 1, lty = 2)
#' # The permutation distribution is with a heavy right tail:
#' require(psych)
#' skew(FM_index_H0) # 1.254
#' kurtosi(FM_index_H0) # 2.5427
#' 
#' mean(FM_index_H0); var(FM_index_H0)
#' the_FM_index <- FM_index(A1_clusters, A2_clusters)
#' the_FM_index
#' our_dnorm <- function(x) {
#'    dnorm(x, mean = attr(the_FM_index, "E_FM"), 
#'          sd = sqrt(attr(the_FM_index, "V_FM")))
#' }
#' # our_dnorm(0.35)
#' curve(our_dnorm,
#'       col = 4,
#'       from = -1,to=1,n=R,add = TRUE)
#' abline(v = attr(the_FM_index, "E_FM"), col = 4, lty = 2)
#' 
#' }
FM_index_permutation <- function(A1_clusters, A2_clusters, warn = TRUE, ...) {
   return(
      as.vector(FM_index(sample(A1_clusters), 
               sample(A2_clusters), 
               include_EV=FALSE, assume_sorted_vectors=TRUE, warn = warn,...))
   )
}





















Bk <- function(A1, A2, k = 3, print_output = F, 
               sanity_checks = FALSE, 
               include_E = TRUE,
               include_V = TRUE,               
               ...)
{
   if(sanity_checks) {	# the sanity checks are turned off by default since the "labels" function for dendrogram is one which takes some time to run...
      # notice that we must have labels.hclust and labels.dendrogram defined!
      A1_labels <- labels(A1)
      A2_labels <- labels(A2)
      length_A1_labels <- length(A1_labels)
      length_A2_labels <- length(A2_labels)	
      
      # Checking for common error options:
      if(length_A1_labels != length_A2_labels) stop("The two clusters don't have the same number of items!")	# If cluster sized are different - stop
      if(!all(sort(A1_labels) == sort(A2_labels))) stop("Your trees are having leaves with different names - please correct it inorder to use this function")
   }
   
   # calculate n
   n <- nleaves(A1) 
   
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
   
   A1_clusters <- cutree(A1, k = k)
   A2_clusters <- cutree(A2, k = k)
   
   # If one of the trees can not be cut, we should return the data.frame with missing values (so to not stop the entire function...)
   if(is.null(A1_clusters) || is.null(A2_clusters)) return(data.frame(Bk = NA, E_FM= NA, V_FM= NA) )
   
   # sort them to look the same (we assume the *same naming* for the leaves !
   A1_clusters <- A1_clusters[order(names(A1_clusters))]	# order the vec accourding to the names, so to allow a comparison
   A2_clusters <- A2_clusters[order(names(A2_clusters))]	# order the vec accourding to the names, so to allow a comparison
   if(print_output) print(list(A1_clusters, A2_clusters )	)
   M <- table(A1_clusters, A2_clusters)
   if(print_output) print(M)
   
   
   Tk <- sum(M^2) - n
   m_i. <- apply(M, 1, sum)
   m_.j <- apply(M, 2, sum)
   m_.. <- n # sum(M)
   if(sum(M) != n ) stop("Why does M matrix doesn't sum up to n ??")
   Pk <- sum(m_i.^2) - n
   Qk <- sum(m_.j^2) - n
   
   Bk <- Tk / sqrt(Pk*Qk)
   
   E_FM <- sqrt(Pk*Qk)/ (n*(n-1))
   
   
   Pk2 <- sum( m_i. * (m_i. -1) * (m_i. -2) )
   Qk2 <- sum( m_.j * (m_.j -1) * (m_.j -2) )
   V_FM <- 2/ (n*(n-1))  +  
      4*Pk2 * Qk2 / ( (n*(n-1)*(n-2)) * Pk*Qk ) + 
      (Pk -2 - 4*Pk2/Pk) * (Qk -2 - 4*Qk2/Qk) / ( (n*(n-1)*(n-2)*(n-3))) -
      Pk * Qk / (n^2*(n-1)^2)
   
   
   Bk_table <- data.frame(Bk, E_FM, V_FM)
   if(!include_E) Bk_table$E_FM <- NULL
   if(!include_V) Bk_table$V_FM <- NULL
         
   return(Bk_table)
}






# Bk(fit1, fit2, k = 2, T)
# Bk(as.dendrogram(fit1), as.dendrogram(fit2), k = 2, T)

# Bk(fit1, fit2, k = 2, F)
# Bk(as.dendrogram(fit1), as.dendrogram(fit2), k = 5, F)


if(F) {
   # The Bk function was previously also implemented by Matt in:
   #		 http://cran.r-project.org/web/packages/profdpm/index.html
   # See pages 9 and 10 here: http://cran.r-project.org/web/packages/profdpm/vignettes/profdpm.pdf
   # I came by this package thanks to chl: http://stats.stackexchange.com/questions/3672/a-measure-to-describe-the-distribution-of-a-dendrogram	
   # Also, there is a great overview of similarity measures on this here:
   # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.164.6189&rep=rep1&type=pdf
   
   our_dist <- dist(USArrests[1:10,])
   dend1 <- hclust(our_dist , "complete")
   dend2 <- hclust(our_dist , "single")
   plot(dend1)
   plot(as.dendrogram(dend1))
   our_dist
   
   
   # install.packages("profdpm")
   require("profdpm")
   # see page 9 and 10:
   pci(cutree(dend1 , k= 4),cutree(dend2 , k= 4))[2]
   Bk(dend1 , dend2, k = 4,F,F,F,F)
   # I get the same results - yay!
   
   # Matthew's C code is 100 times faster than our R code.
   require(microbenchmark)
   microbenchmark(
      pci(cutree(dend1 , k= 4),cutree(dend2 , k= 4)),
      system.time(Bk(dend1 , dend2, k = 4)))
   
   
   
}

Bk_range <- function(A1, A2, ks = NULL, to_print = F)
{
   #ks is the k range
   
   # n <- length(A1$order)	# this would ONLY work for hclust...
   n <- length(leaves.values(A1))	# this will also work for both hclust and denderogram (assuming the proper methods were defined...)
   if(is.null(ks)) ks <- 2:(n-1)
   Bks <- NULL # rep(NA, length(ks))
   
   # counter <- 1
   
   for(the.k in ks)
   {
      #		Bks[[counter]] <- Bk(A1, A2, k = the.k)
      Bks <- rbind(Bks, Bk(A1, A2, k = the.k))
      # counter <- 	counter + 1
      if(to_print) print(paste("We just cut using k:",the.k))
   }	
   return(data.frame(ks ,	Bks))
}

# Bk_range(fit1, fit2, c(2:6))

plot_Bks_with_E_FM <- function(Bk_data_frame, CI_sd_times = 2, plot_mean_Bk_under_H0 = TRUE, ...)	
{
   # CI_sd_times == how many times to multiply the Sd(Bk) - to get the cI
   # the three dots ... are for the plot
   if(sum(is.na(Bk_data_frame))>1) warning("NA's exists in the Bk table - rows with NA were ommited")
   
   aa <- na.omit(Bk_data_frame)	# the na.omit fixes issues with missing values (it deletes the entire row)
   with(aa, plot(Bk ~ ks, ylim = c(0,1), type = "b" ,...))
   if(plot_mean_Bk_under_H0 )
   {
      with(aa, points(E_FM ~ ks, type = "l" , lwd = 2))
      with(aa, points((E_FM +  CI_sd_times*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2))
      with(aa, points((E_FM -  CI_sd_times*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2))
   }
}










plot_Bks_with_E_FM.and.bakers.gamma <- function(thE_FM_object ,the_bakers_gamma, main = "")
{
   plot_Bks_with_E_FM(thE_FM_object)
   
   title(main = main)
   legend("topright", legend = paste("Baker's gamma (P_value):  ",
                                     round(the_bakers_gamma$estimate, 3)," (",
                                     round(the_bakers_gamma$p.value, 5),")", sep = "")
   )
}


# plot_Bks_with_E_FM(Bk_range(fit1, fit2))	


plot_Bks_with_E_FM.BH.adjusted <- function(Bk_data_frame, CI_sd_times = 2, ...)	
{
   # CI_sd_times == how many times to multiply the Sd(Bk) - to get the cI
   # the three dots ... are for the plot
   aa <- Bk_data_frame
   
   Bk.Ps <- with(aa, 1-pnorm(((Bk - E_FM)/sqrt(V_FM)) ))
   with(aa, 1-pnorm(((Bk - E_FM)/sqrt(V_FM)) ))
   with(aa, 1-pnorm(Bk , E_FM,sqrt(V_FM)))
   aa$Bk
   adjusted_Bk.Ps <- p.adjust(Bk.Ps, method = "BH")
   adjusted_Bk <- with(aa, (qnorm(1-adjusted_Bk.Ps) + E_FM)*sqrt(V_FM))	
   ss <- adjusted_Bk == Inf
   adjusted_Bk[ss] <- aa$Bk[ss] # fill in Inf values with original Bk 
   
   
   with(aa, plot(Bk ~ ks, ylim = c(0,1), type = "b" ,...))
   with(aa, points(adjusted_Bk ~ ks, col = "orange", pch = 19, type = "b"))
   with(aa, points(E_FM ~ ks, type = "l" , lwd = 2))
   with(aa, points((E_FM +  CI_sd_times*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2))
   with(aa, points((E_FM -  CI_sd_times*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2))
}


plot_Bks_with_E_FM.holm.adjusted.CI <- function(Bk_data_frame, CI_sd_times = 2, ...)	
{
   # CI_sd_times == how many times to multiply the Sd(Bk) - to get the cI
   # the three dots ... are for the plot
   aa <- Bk_data_frame
   
   # let's assume we wish to keep alfa = .05/2
   CI_sd_times 
   adj.Ps <- (.05/2) / length(aa$Bk):1
   ordered.adj.Ps <- adj.Ps[order(aa$Bk)]
   CI_sd_times <- qnorm(1-ordered.adj.Ps)	# the new  CI_sd_times - adjusted	
   
   with(aa, plot(Bk ~ ks, ylim = c(0,1), type = "b" ,...))	
   with(aa, points(E_FM ~ ks, type = "l" , lwd = 2))
   with(aa, points((E_FM +  CI_sd_times*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2, col = "green"))
   with(aa, points((E_FM -  CI_sd_times*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2, col = "green"))
   
   with(aa, points((E_FM +  qnorm(.975)*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2, col = 1))
   with(aa, points((E_FM -  qnorm(.975)*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2, col = 1))	
   
   bonf.CI_sd_times <- qnorm(1-((.05/2) / length(aa$Bk)))	
   with(aa, points((E_FM +  bonf.CI_sd_times*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2, col = "red"))
   with(aa, points((E_FM -  bonf.CI_sd_times*sqrt(V_FM))  ~ ks, type = "l" , lwd = 1, lty = 2, col = "red"))
   
   legend("topright", fill = c("black", "red","green"), legend = c("no correction - +-1.96", "Bonferroni correction CI", "Holm's adjusted CI"))
}





# calculate
# aa <- Bk_range(fit1, fit2)
# plot_Bks_with_E_FM(aa)

# str(as.dendrogram(fit1))
# aa <- Bk_range(as.dendrogram(fit1), as.dendrogram(fit2))
# Bk(as.dendrogram(fit1), as.dendrogram(fit2), k = 2,T)

# plot_Bks_with_E_FM(aa)

# unlist(as.dendrogram(fit1))
# labels(as.dendrogram(fit1))




###
#
# Bk-method.r

# require(R2PPT)



##example:
# mydata <- USArrests
##Ward Hierarchical Clustering
# d <- dist(mydata, method = "euclidean") # distance matrix
# fit1 <- hclust(d, method="ward") 
# fit2 <- hclust(d, method="single") 
##fit <- fit1
# par(mfrow = c(1,2))
# plot(fit1) # display dendogram
# plot(fit2) # display dendogram

if(F) {
   system.time(Bk(dendo1, dendo2)) 
   # Bk = 0.7832726 0.4284998 0.0007704066
   # times in sec after modifications:
   # 0.51
   # 0.68 
}


# x = vector("list", 5)
# x[1:2]=1:2
# attr(x[[1]], "fo") = "mi"
# attr(x[2], "fo") = "mi2"
# str(x)
