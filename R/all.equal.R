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





#' @title Global Comparison of two (or more) dendrograms
#' @exportS3Method all.equal dendrogram
#' @method all.equal dendrogram
#' @aliases
#' all.equal.dendlist
#' @description
#' This function makes a global comparison of two or more dendrograms trees.
#'
#' The function can get two \link{dendlist} objects and compare
#' them using \link{all.equal.list}. If a dendlist is in only "target"
#' (and not "current"), it will go through the dendlist and
#' compare all of the dendrograms within it to one another.
#'
#' @param target an object of type \link{dendrogram} or \link{dendlist}
#' @param current an object of type \link{dendrogram}
#' @param use.edge.length logical (TRUE). If to check branches' heights.
#' @param use.tip.label.order logical (FALSE). If to check labels are in the same and in identical order
#' @param use.tip.label logical (TRUE). If to check that labels are the same (regardless of order)
#' @param use.topology logical (TRUE). If to check teh existence of distinct edges
#' @param tolerance the numeric tolerance used to compare the branch lengths.
#' @param scale a positive number (NULL as default), comparison of branch height is made after scaling (i.e., dividing) them by this number.
#' @param ... Ignored.
#'
#' @seealso
#' \link{all.equal}, \link[ape]{all.equal.phylo}, \link{identical}
#' @return
#' Either TRUE (NULL for attr.all.equal) or a vector of mode "character" describing the differences
#' between target and current.
#'
#' @usage \method{all.equal}{dendrogram}(target, current,
#'                                      use.edge.length = TRUE,
#'                                      use.tip.label.order = FALSE,
#'                                      use.tip.label = TRUE,
#'                                      use.topology = TRUE,
#'                                      tolerance = .Machine$double.eps^0.5,
#'                                      scale = NULL, ...)
#'
#' \method{all.equal}{dendlist}(target, current, ...)
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#' dend1 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("com") %>%
#'   as.dendrogram()
#' dend2 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("single") %>%
#'   as.dendrogram()
#' dend3 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("ave") %>%
#'   as.dendrogram()
#' dend4 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("centroid") %>%
#'   as.dendrogram()
#' #    cutree(dend1)
#'
#' all.equal(dend1, dend1)
#' all.equal(dend1, dend2)
#' all.equal(dend1, dend2, use.edge.length = FALSE)
#' all.equal(dend1, dend2, use.edge.length = FALSE, use.topology = FALSE)
#'
#' all.equal(dend2, dend4, use.edge.length = TRUE)
#' all.equal(dend2, dend4, use.edge.length = FALSE)
#'
#' all.equal(dendlist(dend1, dend2, dend3, dend4))
#' all.equal(dendlist(dend1, dend2, dend3, dend4), use.edge.length = FALSE)
#' all.equal(dendlist(dend1, dend1, dend1))
#' }
all.equal.dendrogram <- function(target, current,
                                 use.edge.length = TRUE,
                                 use.tip.label.order = FALSE,
                                 use.tip.label = TRUE,
                                 use.topology = TRUE,
                                 tolerance = .Machine$double.eps^0.5,
                                 scale = NULL, ...) {

  #    are_they_equal <- NULL

  # both should be dendrogram
  if (!(is.dendrogram(target))) stop("target must be a dendrogram")
  if (!(is.dendrogram(current))) {
    return("current is not a dendrogram")
  }


  if (use.edge.length) {

    #       suppressWarnings is in order to deal with the case
    # of a tree with just one item.
    # If Rcpp is on, we would get
    #  In Rcpp_get_dend_heights(tree, branches_heights = TRUE, labels_heights = FALSE) :
    #       'height' attr is missing from node, 0 is returned, please check your tree.

    target_branches_heights <- suppressWarnings(
      get_branches_heights(target)
    )
    current_branches_heights <- suppressWarnings(
      get_branches_heights(current)
    )

    result <- all.equal(target_branches_heights,
      current_branches_heights,
      tolerance = tolerance, scale = scale
    )
    if (!is.logical(result)) {
      return(paste("Difference in branch heights - ", result, collapse = ""))
    }
  }

  if (use.tip.label.order) {
    #       all.equal(c("a","b"),c("b","a"))
    #       all.equal(c("a","b"),c(1))
    result <- all.equal(labels(target), labels(current))
    if (!is.logical(result)) {
      return(paste("Difference in labels order - ", result, collapse = ""))
    }
  }

  if (use.tip.label) {
    #       all.equal(c("a","b"),c("b","a"))
    result <- all.equal(sort(labels(target)), sort(labels(current)))
    if (!is.logical(result)) {
      return(paste("Difference in labels - ", result, collapse = ""))
    }
  }

  # Check topology:
  if (use.topology) {
    target_de <- distinct_edges(target, current)
    current_de <- distinct_edges(current, target)
    if (!(is.null(target_de) & is.null(current_de))) {
      return(paste(
        "Dendrograms contain diffreent edges (i.e.: topology). Unique edges in target:",
        "|",
        paste(target_de, collapse = ", "),
        "|",
        "Unique edges in current:", paste(current_de, collapse = ", ")
      ))
    }
  }



  # It means we couldn't find any difference that matters.
  return(TRUE)
}










#' @export
#' @exportS3Method all.equal dendlist
#' @method all.equal dendlist
all.equal.dendlist <- function(target, current, ...) {
  if (!is.dendlist(target)) stop("target needs to be a dendlist object")

  # If we have only 1 item in the dendlist - then compare the internal items only
  if (!missing(current)) {
    return(base::all.equal.list(target, current))
  }
  # else - if we ONLY have targer, then we must want to
  # compare all of the dendrograms inside it:

  n_list <- length(target)

  if (n_list < 2) {
    warning("Did you really want to compare 'all.equal' with a dendlist which contains only 1 item? (with no 'current' parameter?!)")
    return(TRUE)
  }


  are_equal <- vector("list", n_list)
  pairwise_combn <- combn(n_list, 2)

  for (i in 1:ncol(pairwise_combn)) {
    l1 <- pairwise_combn[1, i]
    l2 <- pairwise_combn[2, i]
    are_equal[[i]] <- all.equal(target[[l1]], target[[l2]], ...)
  }




  if (all(sapply(are_equal, is.logical))) {
    return(TRUE)
  } else {
    names(are_equal) <- apply(pairwise_combn, 2, paste, collapse = "==")
    are_equal <- unlist(are_equal)
    return(are_equal)
  }
}
