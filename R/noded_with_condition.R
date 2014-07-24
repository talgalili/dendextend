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
#' require(dendextend)
#' require(magrittr)
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 10 )
#' 
#' # Getting the dend dend
#' dend <- iris[ss,-5] %>% dist %>% hclust %>% as.dendrogram
#' dend %>% plot
#' 
#' 
#' 
#' this is the basis for branches_attr_by_labels
#' has_any_labels <- function(sub_dend, the_labels) any(labels(sub_dend) %in% the_labels)
#' cols <- noded_with_condition(dend, has_any_labels, the_labels = c("126","109", "59")) %>% ifelse(2,1)
#' set(dend, "branches_col", cols) %>% plot
#' 
#' # Similar to branches_attr_by_labels - but for heights!
#' high_enough <- function(sub_dend, height) attr(sub_dend, "height") > height
#' cols <- noded_with_condition(dend, high_enough, height = 1) %>% ifelse(2,1)
#' set(dend, "branches_col", cols) %>% plot
#' 
#' }
#' 
noded_with_condition <- function (dend, condition, include_leaves = TRUE,
                                  include_branches = TRUE,
                                  na.rm = FALSE, ...) {
   if(!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")   
   if(missing(condition)) stop("'condition' parameter is missing.")
   
   
   dend_cond <- rep(NA, nnodes(dend))
   
   # this function is used to modify dend_cond. What it returns is not important.
   i_cond <- 0
   check_condition_per_node <- function(dend_node) {
      i_cond <<- i_cond + 1
      
      # if we should not include_leaves, then we skip when a leaf is encountered.
      if(!include_leaves && is.leaf(dend_node)) return(NULL)
      if(!include_branches && !is.leaf(dend_node)) return(NULL)      
      
      dend_cond[i_cond] <<- condition(dend_node, ...)
      return(NULL)
   }   
   dendrapply(dend, check_condition_per_node)   
   
   # as.vector is to remove all classes of the na.omit
   # thank you Prof. Brian Ripley http://tolstoy.newcastle.edu.au/R/e2/devel/07/01/1965.html
   if(na.rm) dend_cond <- as.vector(na.omit(dend_cond)) 
   
   return(dend_cond)   
}




#' @title Change col/lwd/lty of branches matching labels condition
#' @export
#' @description
#' The user supplies a dend, labels, and type of condition (all/any), and TF_values
#' And the function returns a dendrogram with branches col/lwd/lty accordingly
#' @param dend a dendrogram dend 
#' @param labels a character vector of labels from the tree
#' @param type a character vector of either "all" or "any", indicating which of 
#' the branches should be painted: ones that all of their labels belong to the supplied labels,
#' or also ones that even some of their labels are included in the labels vector.
#' @param TF_values a two dimensional vector with the TF_values to use in case a branch fulfills the condition (TRUE)
#' and in the case that it does not (FALSE). Defaults are 2/1 for col, lwd and lty.
#' @param ... ignored.
#' @return 
#' A dendrogram with colored branches.
#' @seealso \link{noded_with_condition}, \link{get_leaves_attr}, \link{nnodes}, \link{nleaves}
#' @examples
#' \dontrun{
#' 
#' require(dendextend)
#' require(magrittr)
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 10 )
#' 
#' # Getting the dend dend
#' dend <- iris[ss,-5] %>% dist %>% hclust %>% as.dendrogram
#' dend %>% plot
#' 
#' dend %>% branches_attr_by_labels(c("123", "126", "23", "29")) %>% plot
#' dend %>% branches_attr_by_labels(c("123", "126", "23", "29"), "all") %>% plot # the same as above
#' dend %>% branches_attr_by_labels(c("123", "126", "23", "29"), "any") %>% plot
#' 
#' dend %>% branches_attr_by_labels(c("123", "126", "23", "29"), "any", "col", c("blue", "red")) %>% plot
#' dend %>% branches_attr_by_labels(c("123", "126", "23", "29"), "any", "lwd", c(4,1)) %>% plot
#' dend %>% branches_attr_by_labels(c("123", "126", "23", "29"), "any", "lty", c(2,1)) %>% plot
#' 
#' }
branches_attr_by_labels <- function(dend, labels, type = c("all", "any"), attr = c("col", "lwd", "lty"), TF_values = c(2,1), ...) {
   if(!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")   
   if(missing(labels)) stop("'labels' parameter is missing.")
   if(!is.character(labels)) {      
      warning("'labels' parameter was not a character vector, and was coerced into one.")
      labels <- as.character(labels)
   }
   
   
   type <- match.arg(type)
   attr <- match.arg(attr)
   
   
   # deal with the case we have labels not included in the tree dend:
   labels_in_dend <- labels %in% labels(dend)
   if(!all(labels_in_dend)) {
      warning("Not all of the labels you provided are included in the dendrogram.\n",
              "The following labels were omitted:", labels[!labels_in_dend])
      #       cat("\n")
      labels <- labels[labels_in_dend]
   }
   
   has_any_labels <- function(sub_dend, the_labels) any(labels(sub_dend) %in% the_labels)
   has_all_labels <- function(sub_dend, the_labels) all(labels(sub_dend) %in% the_labels)
   
   what_to_change <- switch(type, 
                            all = noded_with_condition(dend, has_all_labels, the_labels = labels),
                            any = noded_with_condition(dend, has_any_labels, the_labels = labels)
   )
   the_TF_values <- ifelse(what_to_change, TF_values[1], TF_values[2])
   # warnings()
   
   
   switch(attr, 
          col = set(dend, "branches_col", the_TF_values)      ,
          lwd = set(dend, "branches_lwd", the_TF_values)      ,
          lty = set(dend, "branches_lty", the_TF_values)      
   )   
}






