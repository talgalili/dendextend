#' @title Trimms one leaf from a dendrogram
#' @param x dendrogram object
#' @param leaf_name a character string as the label of the tip we wish to trim
#' @param ... passed on
#' @details 
#' Used through \link{trim}
#' @return A dendrogram with a leaf trimmed
#' @examples
#' hc <- hclust(dist(USArrests[1:5,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' par(mfrow = c(1,2))
#' plot(dend, main = "original tree")
#' plot(dendextend:::trim_leaf(dend , "Alaska"), main = "tree without Alaska")
trim_leaf <- function(x, leaf_name,...)
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
         if(is.leaf(x[[i]]) == T  &&  labels(x[[i]]) == leaf_name) is.father <- T
      }
      return(is.father)
   }
   
   
   remove.leaf.if.child <- function(x, leaf_name)
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
               x[[i]] <- remove.leaf.if.child(x[[i]], leaf_name)
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
   
   new_x <- remove.leaf.if.child(x, leaf_name)
   new_x <- stats:::midcache.dendrogram(new_x )
   return(new_x)
}






#' @title Trim a tree (using leaves' labels)
#' @aliases 
#' trim.default
#' trim.dendrogram
#' trim.hclust
#' trim.phylo
#' @description  Trimms a tree (dendrogram, hclust) from a set of leaves based on their labels.
#' @usage
#' trim(x, ...)
#' 
#' \method{trim}{dendrogram}(x, leaves,...)
#' 
#' \method{trim}{hclust}(x, leaves,...)
#' 
#' \method{trim}{phylo}(x, ...)
#' @export
#' @param x tree object (dendrogram/hclust/phylo)
#' @param leaves a character vector of the label(S) of the tip(s) (leaves) we wish to trim off the tree.
#' @param ... passed on
#' @details 
#' I was not sure if to call this function drop.tip (from ape), snip/prune (from rpart) or just remove.leaves.  I ended up deciding on trim.
#' 
#' @return A trimmed tree
#' @seealso \link{trim_leaf}, \link[ape]{drop.tip} {ape}
#' @examples
#' hc <- hclust(dist(USArrests[1:5,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' par(mfrow = c(1,2))
#' plot(dend, main = "original tree")
#' plot(trim(dend , c("Alaska", "California")), main = "tree without Alaska and California")
trim <- function(x, ...) UseMethod("trim")

#' @export
trim.default <- function(x,...) stop("object x must be a dendrogram/hclust/phylo object")

#' @S3method trim dendrogram
trim.dendrogram <- function(x, leaves,...) {
   leaves <- as.character(leaves)
      
   for(i in seq_along(leaves))
   {
      x <- trim_leaf(x, leaves[i])	# move step by stem to remove all of these leaves...
   }
   return(x)
}


#' @S3method trim hclust
trim.hclust <- function(x, leaves,...) {
   x_dend <- as.dendrogram(x)
   x_dend_trimmed <- trim(x_dend, leaves,...)
   x_trimmed <- as_hclust_fixed(x_dend_trimmed, x)  
   
   return(x_trimmed)
}

#' @S3method trim phylo
trim.phylo <- function(x,...) ape:::drop.tip(phy=x, ...)









