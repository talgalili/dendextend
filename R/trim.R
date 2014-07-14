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






#' @title Trimms one leaf from a dendrogram
#' @export
#' @param x dendrogram object
#' @param leaf_name a character string as the label of the tip we wish to prune
#' @param ... passed on
#' @details 
#' Used through \link{prune}
#' @return A dendrogram with a leaf pruned
#' @examples
#' hc <- hclust(dist(USArrests[1:5,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' par(mfrow = c(1,2))
#' plot(dend, main = "original tree")
#' plot(prune_leaf(dend , "Alaska"), main = "tree without Alaska")
#' 
#' 
prune_leaf <- function(x, leaf_name,...)
{
   labels_x <- labels(x)
   
   if(length(labels_x) != length(unique(labels_x)))	warning("Found dubplicate labels in the tree (this might indicate a problem in the tree you supplied)")

   if(!(leaf_name %in% labels_x)) {	# what to do if there is no such leaf inside the tree
      warning(paste("There is no leaf with the label", leaf_name , "in the tree you supplied", "\n" , "Returning original tree", "\n" ))
      return(x)
   }
   
   if(sum(labels_x %in% leaf_name) > 1) {	# what to do if there is no such leaf inside the tree      
      warning(paste("There are multiple leaves by the name of '", leaf_name , "' in the tree you supplied.  Their locations is:",
                    paste(which(labels_x %in% leaf_name), collapse = ","),"\n" , "Returning original tree", "\n" ))
      return(x)
   }
   
   is.father.of.leaf.to.remove <- function(x, leaf_name)
   {
      # this function checks if the leaf we wish to remove is the direct child of the current branch (x) we entered the function
      is.father <- F
      for(i in seq_len(length(x)))
      {
         if(is.leaf(x[[i]]) == TRUE  &&  labels(x[[i]]) == leaf_name) is.father <- TRUE
      }
      return(is.father)
   }
   
   
   remove_leaf_if_child <- function(x, leaf_name)
   {
      # print(labels(x))
      if(all(labels(x) != leaf_name))
      {	# if the leaf we want to remove is not in this branch, simply return the branch without going deeper intoit.
         return(x)
      } else {	# but if the leaf we want to remove is here somewhere, go on searching
         attr(x, "members") <- attr(x, "members") - 1 
         
         if(!is.father.of.leaf.to.remove(x, leaf_name))	# if you are not the father, then go on and make this function work on each child
         {
            for(i in seq_len(length(x)))
            {
               x[[i]] <- remove_leaf_if_child(x[[i]], leaf_name)
            }
         } else { # we'll merge 
            if(length(x) != 2) stop("This function doesn't work for non binary branches where the leaf to remove is located")	# this should be fixed in the future...				
            # if leaf location is 1, then move branch in leaf 2 to be the new x
            leaf_location <- 1 			
            if(is.leaf(x[[leaf_location]]) == T  &&  labels(x[[leaf_location]]) == leaf_name) {
               branch_to_bumpup <- 2
               x <- x[[branch_to_bumpup]]
            } else { # else - the leaf location must be located in position "2"
               branch_to_bumpup <- 1
               x <- x[[branch_to_bumpup]]
            }				
         }
      }		
      return(x)
   }
   
   
   new_x <- remove_leaf_if_child(x, leaf_name)
   new_x <- suppressWarnings(stats_midcache.dendrogram(new_x)) # fixes the attributes
#   new_x <- fix_members_attr.dendrogram(new_x) # fix the number of memebers attr for each node
   return(new_x)
}






#' @title Prunes a tree (using leaves' labels)
#' @export
#' @aliases 
#' prune.default
#' prune.dendrogram
#' prune.hclust
#' prune.phylo
#' @description  Trimms a tree (dendrogram, hclust) from a set of leaves based on their labels.
#' @usage
#' prune(x, ...)
#' 
#' \method{prune}{dendrogram}(x, leaves,...)
#' 
#' \method{prune}{hclust}(x, leaves,...)
#' 
#' \method{prune}{phylo}(x, ...)
#' @param x tree object (dendrogram/hclust/phylo)
#' @param leaves a character vector of the label(S) of the tip(s) (leaves) we wish to prune off the tree.
#' @param ... passed on
#' @details 
#' I was not sure if to call this function drop.tip (from ape), snip/prune (from rpart) or just remove.leaves.  I ended up deciding on prune.
#' 
#' @return A pruned tree
#' @seealso \link{prune_leaf}, \link[ape]{drop.tip} {ape}
#' @examples
#' hc <- hclust(dist(USArrests[1:5,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' par(mfrow = c(1,2))
#' plot(dend, main = "original tree")
#' plot(prune(dend , c("Alaska", "California")), main = "tree without Alaska and California")
prune <- function(x, ...) {UseMethod("prune")}

#' @export
prune.default <- function(x,...) {
   stop("object x must be a dendrogram/hclust/phylo object")
}

# ' @S3method prune dendrogram
#' @export
prune.dendrogram <- function(x, leaves,...) {
   leaves <- as.character(leaves)
      
   for(i in seq_along(leaves))
   {
      # this function is probably not the fastest - but it works...
      x <- prune_leaf(x, leaves[i])	# move step by stem to remove all of these leaves...
   }
   return(x)
}


# ' @S3method prune hclust
#' @export
prune.hclust <- function(x, leaves,...) {
   x_dend <- as.dendrogram(x)
   x_dend_pruned <- prune(x_dend, leaves,...)
   x_pruned <- as_hclust_fixed(x_dend_pruned, x)  
   
   return(x_pruned)
}

# ' @S3method prune phylo
#' @export
prune.phylo <- function(x,...) {
	# require(ape)
	ape::drop.tip(phy=x, ...)
}









#' @title Intersect trees
#' @description 
#' Return two trees after pruning them so that the only leaves left are the intersection of their labels.
#' @export
#' @param x1 tree object (dendrogram/hclust/phylo)
#' @param x2 tree object (dendrogram/hclust/phylo)
#' @param warn logical (FALSE). Should a warning be issued if there
#' was a need to perform intersaction.
#' @param ... passed on
#' @return A list with two pruned trees
#' @seealso \link{prune}, \link{intersect}, \link{labels}
#' @examples
#' hc <- hclust(dist(USArrests[1:5,]), "ave")
#' dend <- as.dendrogram(hc)
#' labels(dend) <- 1:5
#' dend1 <- prune(dend, 1)
#' dend2 <- prune(dend, 5)
#' intersect_dend <- intersect_trees(dend1, dend2)
#' 
#' layout(matrix(c(1,1,2,3,4,5), 3,2, byrow=TRUE))
#' plot(dend, main = "Original tree")
#' plot(dend1, main = "Tree 1:\n original with label 1 pruned");
#'    plot(dend2, main = "Tree 2:\n original with label 2 pruned")
#' plot(intersect_dend[[1]], 
#'       main = "Tree 1 pruned
#'       with the labels that intersected with those of Tree 2")
#'    plot(intersect_dend[[2]],
#'       main = "Tree 2 pruned
#'       with the labels that intersected with those of Tree 1")
#' 
intersect_trees <- function(x1, x2, warn= FALSE, ...){
   labels_x1 <- labels(x1)
   labels_x2 <- labels(x2)
   intersected_labels <- intersect(labels_x1, labels_x2)
   
   # prune tree 1
   ss_labels_to_keep  <- labels_x1 %in% intersected_labels
   ss_labels_to_prune_1 <- !ss_labels_to_keep
   pruned_x1 <- prune(x1, labels_x1[ss_labels_to_prune_1])
      
   # prune tree 2
   ss_labels_to_keep  <- labels_x2 %in% intersected_labels
   ss_labels_to_prune_2 <- !ss_labels_to_keep
   pruned_x2 <- prune(x2, labels_x2[ss_labels_to_prune_2])
   
   if(warn && any(c(ss_labels_to_prune_1, ss_labels_to_prune_2)))  {
      warning("The labels in both tree had different values - trees were pruned.")
   }
   
   return(list(pruned_x1, pruned_x2))   
}






# methods(prune)
# example(rotate)
# example(prune)
# example(intersect_trees)

