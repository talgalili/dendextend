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



#' @title Sorts a distance matrix by rows and columns names
#' @description Sorts a distance matrix by the names of the rows and columns.
#' @export
#' @param dist_mat a distance matrix.
#' @param by_rows logical (TRUE). Sort the distance matrix by rows?
#' @param by_cols logical (TRUE). Sort the distance matrix by columns?
#' @param ... Ignored.
#' @seealso
#' \link{dist}, \link{cor_cophenetic}
#' @return
#' A distance matrix (after sorting)
#'
sort_dist_mat <- function(dist_mat, by_rows = TRUE, by_cols = TRUE, ...) {
  dist1 <- as.matrix(dist_mat)
  if (by_rows) dist1 <- dist1[order(rownames(dist1)), ]
  if (by_cols) dist1 <- dist1[, order(colnames(dist1))]
  dist1 <- as.dist(dist1)
  attributes(dist1)[c("Diag", "Upper")] <- attributes(dist_mat)[c("Diag", "Upper")]
  dist1
}






#' @title Cophenetic correlation between two trees
#' @name cor_cophenetic
#' @export
#' @description
#' Cophenetic correlation coefficient for two trees.
#'
#' Assumes the labels in the two trees fully match. If they do not
#' please first use \link{intersect_trees} to have them matched.
#'
#'
#' @param dend1 a tree (dendrogram/hclust/phylo, or dendlist)
#' @param dend2 Either a tree (dendrogram/hclust/phylo), or a \link{dist} object (for example, from the original data matrix).
#' @param which an integer vector of length 2, indicating
#' which of the trees in a dendlist object should have
#' their cor_cophenetic calculated.
#' @param method_coef a character string indicating which correlation coefficient
#' is to be computed. One of "pearson" (default), "kendall", or "spearman",
#' can be abbreviated. Passed to \link{cor}.
#' @param ... Ignored.
#'
#' @details
#'
#' From \link{cophenetic}:
#' The cophenetic distance between two observations that have been clustered
#' is defined to be the intergroup dissimilarity at which the two observations
#' are first combined into a single cluster. Note that this distance has many
#' ties and restrictions.
#'
#' cor_cophenetic calculates the correlation between two cophenetic distance
#' matrices of the two trees.
#'
#' The value can range between -1 to 1. With near 0 values meaning that
#' the two trees are not statistically similar.
#' For exact p-value one should result to a permutation test. One such option
#' will be to permute over the labels of one tree many times, and calculating
#' the distriubtion under the null hypothesis (keeping the trees topologies
#' constant).
#'
#' Notice that this measure IS affected by the height of a branch.
#'
#' @seealso
#' \link{cophenetic}, \link{cor_bakers_gamma}
#' @return
#' The correlation between cophenetic
#'
#' @references
#'
#' Sokal, R. R. and F. J. Rohlf. 1962. The comparison of dendrograms by
#' objective methods. Taxon, 11:33-40
#'
#' Sneath, P.H.A. and Sokal, R.R. (1973) Numerical Taxonomy: The Principles
#' and Practice of Numerical Classification, p. 278 ff; Freeman, San Francisco.
#'
#' \url{https://en.wikipedia.org/wiki/Cophenetic_correlation}
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#' hc1 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("com")
#' hc2 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("single")
#' dend1 <- as.dendrogram(hc1)
#' dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)
#'
#' cophenetic(hc1)
#' cophenetic(hc2)
#' # notice how the dist matrix for the dendrograms have different orders:
#' cophenetic(dend1)
#' cophenetic(dend2)
#'
#' cor(cophenetic(hc1), cophenetic(hc2)) # 0.874
#' cor(cophenetic(dend1), cophenetic(dend2)) # 0.16
#' # the difference is becasue the order of the distance table in the case of
#' # stats:::cophenetic.dendrogram will change between dendrograms!
#'
#' # however, this is consistant (since I force-sort the rows/columns):
#' cor_cophenetic(hc1, hc2)
#' cor_cophenetic(dend1, dend2)
#'
#' cor_cophenetic(dendlist(dend1, dend2))
#'
#' # we can also use different cor methods (almost the same result though):
#' cor_cophenetic(hc1, hc2, method = "spearman") # 0.8456014
#' cor_cophenetic(dend1, dend2, method = "spearman") #
#'
#'
#' # cophenetic correlation is about 10 times (!) faster than bakers_gamma cor:
#' library(microbenchmark)
#' microbenchmark(
#'   cor_bakers_gamma = cor_bakers_gamma(dend1, dend2, try_cutree_hclust = FALSE),
#'   cor_cophenetic = cor_cophenetic(dend1, dend2),
#'   times = 10
#' )
#'
#' # but only because of the cutree for dendrogram. When allowing hclust cutree
#' # it is only about twice as fast:
#' microbenchmark(
#'   cor_bakers_gamma = cor_bakers_gamma(dend1, dend2, try_cutree_hclust = TRUE),
#'   cor_cophenetic = cor_cophenetic(dend1, dend2),
#'   times = 10
#' )
#' }
#'
cor_cophenetic <- function(dend1, ...) {
  UseMethod("cor_cophenetic")
}



#' @export
#' @rdname cor_cophenetic
cor_cophenetic.default <- function(dend1, dend2, method_coef = c("pearson", "kendall", "spearman"), ...) {
  dist_dend1 <- cophenetic(dend1)

  # if(!is.dist(dend2)) stop("dend2 must be either a dendrogram or a dist object")
  if (is.dist(dend2)) {
    dist_dend2 <- dend2
  } else { # either dendrogram or hclust or other
    dist_dend2 <- cophenetic(dend2)
  }
  # else - dend2 must be a distance matrix

  # hclust objects actually don't need the sorting...
  if (!is.hclust(dend1)) {
    dist_dend1 <- sort_dist_mat(dist_dend1)
    dist_dend2 <- sort_dist_mat(dist_dend2)
  }

  method_coef <- match.arg(method_coef)
  cor(dist_dend1, dist_dend2, method = method_coef)
}


#' @export
#' @rdname cor_cophenetic
cor_cophenetic.dendlist <- function(dend1, which = c(1L, 2L), method_coef = c("pearson", "kendall", "spearman"), ...) {
  method_coef <- match.arg(method_coef)
  cor_cophenetic(dend1[[which[1]]], dend1[[which[2]]], method = method_coef, ...)
}


#
#
# cor_cophenetic.hclust <- function(dend1, dend2, method = c("pearson", "kendall", "spearman"), ...) {
#    # hclust keeps things consistant
#    dist_dend1 <- cophenetic(dend1)
#    dist_dend2 <- cophenetic(dend2)
#
#    cor(dist_dend1, dist_dend2 , method=method)
# }
#
#
# # ' @S3method cor_bakers_gamma dendrogram
# cor_cophenetic.dendrogram <- function(dend1, dend2, use_labels_not_values = TRUE, to_plot = FALSE, warn = dendextend_options("warn"), ...)
# {
#     return(bakers_gamma)
# }
#
