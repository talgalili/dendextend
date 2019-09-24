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



#' @title Find which nodes satisfies a condition
#' @export
#' @description
#' Goes through a tree's nodes in order to return a vector
#' with whether (TRUE/FALSE) each node satisies some condition (function)
#'
#' @param dend a dendrogram dend
#' @param condition a function that gets a node and return TRUE or FALSE
#' (based on whether or not that node/tree fulfills the "condition")
#' @param include_leaves logical. Should leaves attributes be included as well?
#' @param include_branches logical. Should non-leaf (branch node)
#' attributes be included as well?
#' @param na.rm logical. Should NA attributes be REMOVED from the resulting vector?
#' @param ... passed to the condition function
#' @return
#' A logical vector with TRUE/FALSE, specifying for each of the dendrogram's nodes if it fulfills the condition or not.
#' @seealso \link{branches_attr_by_labels}, \link{get_leaves_attr}, \link{nnodes}, \link{nleaves}
#' @examples
#' \dontrun{
#'
#' library(dendextend)
#'
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#'
#' # Getting the dend dend
#' dend <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend %>% plot()
#'
#'
#'
#' # this is the basis for branches_attr_by_labels
#' has_any_labels <- function(sub_dend, the_labels) any(labels(sub_dend) %in% the_labels)
#' cols <- noded_with_condition(dend, has_any_labels,
#'   the_labels = c("126", "109", "59")
#' ) %>%
#'   ifelse(2, 1)
#' set(dend, "branches_col", cols) %>% plot()
#'
#' # Similar to branches_attr_by_labels - but for heights!
#' high_enough <- function(sub_dend, height) attr(sub_dend, "height") > height
#' cols <- noded_with_condition(dend, high_enough, height = 1) %>% ifelse(2, 1)
#' set(dend, "branches_col", cols) %>% plot()
#' }
#'
noded_with_condition <- function(dend, condition, include_leaves = TRUE,
                                 include_branches = TRUE,
                                 na.rm = FALSE, ...) {
  if (!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")
  if (missing(condition)) stop("'condition' parameter is missing.")


  dend_cond <- rep(NA, nnodes(dend))

  # this function is used to modify dend_cond. What it returns is not important.
  i_cond <- 0
  check_condition_per_node <- function(dend_node) {
    i_cond <<- i_cond + 1

    # if we should not include_leaves, then we skip when a leaf is encountered.
    if (!include_leaves && is.leaf(dend_node)) {
      return(NULL)
    }
    if (!include_branches && !is.leaf(dend_node)) {
      return(NULL)
    }

    dend_cond[i_cond] <<- condition(dend_node, ...)
    return(NULL)
  }
  dendrapply(dend, check_condition_per_node)

  # as.vector is to remove all classes of the na.omit
  # thank you Prof. Brian Ripley http://tolstoy.newcastle.edu.au/R/e2/devel/07/01/1965.html
  if (na.rm) dend_cond <- as.vector(na.omit(dend_cond))

  return(dend_cond)
}









#' @title Which node is a leaf?
#' @export
#' @description
#' Gives a vector as the number of nodes (\link{nnodes}),
#' which gives a TRUE when a node is a leaf.
#' @param dend a dendrogram dend
#' @param ... ignored.
#' @return
#' A logical vector with the length of \link{nnodes},
#' which gives a TRUE when a node is a leaf.
#' @seealso \link{noded_with_condition}, \link{is.leaf}, \link{nnodes}
#' @examples
#' \dontrun{
#'
#' library(dendextend)
#'
#' # Getting the dend dend
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#' dend <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend %>% plot()
#'
#' which_leaf(dend)
#' }
which_leaf <- function(dend, ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")
  noded_with_condition(dend, is.leaf)
}







#' @title Which node id is common to a group of labels
#' @export
#' @description
#' This function identifies which edge(s) in a tree has group of labels ("tips") in common.
#' By default it only returns the edge (node) with the heighest id.
#'
#' @param dend a dendrogram dend
#' @param labels a character vector of labels from the tree
#' @param max_id logical (TRUE) - if to return only the max id
#' @param ... ignored.
#'
#' @return
#' An integer with the id(s) of the nodes which includes all of the labels.
#'
#' @seealso \link{noded_with_condition}, \link{branches_attr_by_clusters},
#' \link{nnodes}, \link{branches_attr_by_labels}, \link{get_nodes_attr}
#' \link[ape]{which.edge}
#' @examples
#'
#' dend <- iris[1:10, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("labels", 1:10)
#' dend %>% plot()
#'
#' which_node(dend, c(1, 2), max_id = FALSE)
#' which_node(dend, c(2, 3), max_id = FALSE)
#' which_node(dend, c(2, 3))
#'
#' dend %>% plot()
#' the_h <- get_nodes_attr(dend, "height", which_node(dend, c(4, 6)))
#' the_h
#' abline(h = the_h, lty = 2, col = 2)
#' get_nodes_attr(dend, "height", which_node(dend, c(4, 6)))
#' get_nodes_attr(dend, "members", which_node(dend, c(4, 6)))
which_node <- function(dend, labels, max_id = TRUE, ...) {
  tree_has_all_labels <- function(sub_dend, the_labels) all(the_labels %in% labels(sub_dend))
  nodes_id <- which(noded_with_condition(dend, tree_has_all_labels, the_labels = labels))
  if (max_id) nodes_id <- max(nodes_id)
  nodes_id
}
