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




#' @title Duplicate a leaf X times
#' @export
#' @description
#' Duplicates a leaf in a tree. Useful for non-parametric bootstraping trees
#' since it emulates what would have happened if the tree was constructed based
#' on a row-sample with replacments from the original data matrix.
#' @param dend a dendrogram object 
#' @param leaf_label the label of the laef to replicate.
#' @param times the number of times we will have this leaf after replication
#' @param fix_members logical (TRUE). Fix the number of members in attr
#' using \link{fix_members_attr.dendrogram}
#' @param fix_order logical (TRUE). Fix the leaves order 
#' @param fix_midpoint logical (TRUE). Fix the midpoint value.
#' If TRUE, it overrides "fix_members" and turns it into TRUE (since it must
#' have a correct number of members in order to work).
#' values using \link{rank_order.dendrogram}
#' @param ... not used
#' @return 
#' A dendrogram, after duplicating one of its leaves.
#' @examples
#' 
#' \dontrun{
#' # define dendrogram object to play with:
#' dend <- USArrests[1:3,] %>% dist %>% hclust(method = "ave") %>% as.dendrogram
#' plot(dend)
#' duplicate_leaf(dend, "Alaska", 3)
#' duplicate_leaf(dend, "Arizona", 2, fix_members =FALSE, fix_order = FALSE)
#' plot(duplicate_leaf(dend, "Alaska", 2))
#' plot(duplicate_leaf(dend, "Alaska", 4))
#' plot(duplicate_leaf(dend, "Arizona", 2))
#' plot(duplicate_leaf(dend, "Arizona", 4))
#' }
#' 
duplicate_leaf <- function(dend, leaf_label, times, fix_members=TRUE, fix_order=TRUE,fix_midpoint=TRUE, ...) {
   
   # extreme case:
   if(length(leaf_label) > 1) {
      leaf_label <- leaf_label[1]
      warning("leaf_label had a length >1, only the first element was used.")
   }
   
   # dend_node <- dend
   duplicate_leaf_in_node <- function(dend_node) {
   
      # if this is a leaf - we can not replicate the leaf 
      # we have to do it in the parent node.
      if(is.leaf(dend_node)) return(unclass(dend_node))
      
      # if leaf_label is not in this node, return the dend_node
      # as there is no point to keep going done that node
      if(!(leaf_label %in% labels(dend_node))) return(unclass(dend_node))
      
      # since the leaf is in this node, is it one of its children?
      
      # let's first get the childrens labels
      return_label_or_NA <- function(x) {
         x_label <- attr(x, "label")
         if(is.null(x_label)) x_label <- NA
         x_label
      }
#       typeof(sapply(dend, return_label_or_NA))
      childrens_labels <- sapply(dend_node, return_label_or_NA)
      # if we can't find the leaf in one of these node's children, 
      # then Recall the function on all children nodes and then return the node:
      if(!(leaf_label %in% childrens_labels)) {         
         for(i in seq(dend_node)) {
            dend_node[[i]] <- Recall(dend_node[[i]])
         }
         return(unclass(dend_node))
      }
      
      # but if it is here(!), let us duplicate it:
      n_childrens <- length(childrens_labels)
      leaf_label_location <- which(childrens_labels == leaf_label)
      the_leaf <- unclass(dend_node[[leaf_label_location]])
      leaf_order_value <- as.vector(the_leaf)
      
      for(i in 2:times) {
         temp_new_leaf <- the_leaf
         attr(temp_new_leaf, "label") <- paste(leaf_label,i, sep = "_")
         temp_new_leaf[1] <- leaf_order_value + i/(n_childrens+1)
         dend_node[[n_childrens+i-1]] <- temp_new_leaf         
      }
      
      return(unclass(dend_node))
      
   }
   
   if(!(leaf_label %in% labels(dend))) {
      warning(paste("The label", leaf_label,"Is not available in this tree. Original tree returned." ))
      return(dend)
   }
   
   if(times == 1) {
      # no point in replicating this if it needs to be only once, now is there?
      return(dend)
   }
   
   if(!is.leaf(dend)) {
      dend <- duplicate_leaf_in_node(dend)
   } else {
      # extreme case:
      # in this case, we need to do something special
      # if we got this far - we already know this tree has the only leaf we want.
      attr(dend, "height") <- 0 # just to be sure the leaf has height 0.
      new_dend <- list()
#       dend <- rep(dend,times) # FAILS!
      for(i in seq_len(times)) new_dend[[i]] <- dend
      dend <- new_dend      
      attr(dend, "members") <- times
      attr(dend, "height") <- 1
      labels(dend) <- paste(leaf_label,seq_len(times), sep = "_")      
   }
   
   
   class(dend) <- "dendrogram"

   if(fix_midpoint) fix_members <- TRUE # for midpoint to work, we MUST have a correct number of members.
   if(fix_members) dend <- fix_members_attr.dendrogram(dend)   
   if(fix_order) dend <- rank_order.dendrogram(dend)
   if(fix_midpoint) dend <- suppressWarnings(stats_midcache.dendrogram(dend)) # fixing the middle point thing   

   return(dend)
}












#' @title Sample a tree
#' @export
#' @description
#' Samples a tree, either by permuting the labels (which is usefull for 
#' a permutation test), or by repeated sampling of the same labels (essential
#' for bootstraping when we don't have access to the original data which 
#' produced the tree).
#' 
#' Duplicates a leaf in a tree. Useful for non-parametric bootstraping trees
#' since it emulates what would have happened if the tree was constructed based
#' on a row-sample with replacments from the original data matrix.
#' @param dend a dendrogram object 
#' @param replace logical (FALSE). Should we shuffle the labels (if FALSE),
#' or should we replicate the same leaf over and over, while omitting other
#' leaves? (this is when set to TRUE).
#' @param dend_labels a character vector of the tree's labels.
#' This can save the time it takes for getting the tree labels (in case we run
#' a simulating, computing this once might save some running time).
#' If missing, it uses \link{labels} in order to get the labels.
#' @param sampled_labels a character vector of the tree's sampled labels.
#' This can help us if we wish to compare two trees. In such a case we'd like
#' to be able to have the same sample of labels used on both trees.
#' If missing, it uses \link{sample} in order to get the sampled labels.
#' 
#' Only works when replace=TRUE!
#' @param fix_members logical (TRUE). Fix the number of members in attr
#' using \link{fix_members_attr.dendrogram}
#' @param fix_order logical (TRUE). Fix the leaves order 
#' @param fix_midpoint logical (TRUE). Fix the midpoint value.
#' If TRUE, it overrides "fix_members" and turns it into TRUE (since it must
#' have a correct number of members in order to work).
#' values using \link{rank_order.dendrogram}
#' @param ... not used
#' @return 
#' A dendrogram, after "sampling" its leaves.
#' @seealso
#' \link{sample}, \link{duplicate_leaf}
#' @examples
#' 
#' \dontrun{
#' # define dendrogram object to play with:
#' dend <- USArrests[1:5,] %>% dist %>% hclust(method = "ave") %>% as.dendrogram
#' plot(dend)
#' 
#' # # same tree, with different order of labels 
#' plot(sample.dendrogram(dend, replace = FALSE)) 
#' 
#' # # A different tree (!), with some labels duplicated, 
#'   # while others are pruned
#' plot(sample.dendrogram(dend, replace = TRUE)) 
#' }
#' 
sample.dendrogram <- function(dend, replace = FALSE, 
                              dend_labels, sampled_labels, 
                              fix_members=TRUE, fix_order=TRUE, fix_midpoint=TRUE,...) {
   if(missing(dend_labels)) dend_labels <- labels(dend)
   
   if(replace) {
      if(missing(sampled_labels)) sampled_labels <- sample(dend_labels, replace=TRUE)
      
      # 1) prune redundent leaves
      ss_kept_labels <- dend_labels %in% sampled_labels
      ss_removed_labels <- !ss_kept_labels
      removed_labels <- dend_labels[ss_removed_labels]
      dend <- prune(dend, leaves=removed_labels)
      

      # 2) add new leaves
      #
      # table of the new sampled labels
      t_sampled_labels <- table(sampled_labels)
      names_t_sampled_labels <- names(t_sampled_labels) 
      for(i in seq_along(t_sampled_labels)){
         dend <- duplicate_leaf(dend,
                        leaf_label=names_t_sampled_labels[i],
                        times = unname(t_sampled_labels[i]),
                        fix_members=FALSE, 
                        fix_order=FALSE, 
                        fix_midpoint=FALSE
                        )
      }
         

      # it is better to do it here than over at duplicate_leaf
      # since we are fine with doing this fix only once after we finished
      # adding all of the new labels.
#       # 3) Fix members
#       # 4) Fix leaves order values
      if(fix_midpoint) fix_members <- TRUE # for midpoint to work, we MUST have a correct number of members.
      if(fix_members) dend <- fix_members_attr.dendrogram(dend)   
      if(fix_order) dend <- rank_order.dendrogram(dend)
      if(fix_midpoint) dend <- suppressWarnings(stats_midcache.dendrogram(dend)) # fixing the middle point thing   
      
      
   } else { 
      # don't replace
      # this is MUCH simpler, and useful for permutation tests.
      
      n_dend <- nleaves(dend)
      new_order <- sample(n_dend)
      labels(dend) <- dend_labels[new_order]      
      if(fix_order) order.dendrogram(dend) <- order.dendrogram(dend)[new_order]      
   }
   
   
   class(dend) <- "dendrogram"
   
   return(dend)
}

