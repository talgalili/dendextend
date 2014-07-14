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




#' @title get attributes from the dendrogram's root branches
#' @export
#' @param dend dendrogram object
#' @param the_attr the attribute to get from the branches (for example "height")
#' @param warning default is TRUE. Should a warning be printed when 
#' the function is used on an object which is NOT a dendrogram.
#' @param ... passed on to attr
#' @return The attributes of the branches (often two) of the dendrogram's root
#' @seealso \link{attr}
#' @examples
#' hc <- hclust(dist(USArrests[2:9,]), "com")
#' dend <- as.dendrogram(hc)
#' 
#' get_branches_attr(dend, "height") # 0.00000 71.96247
#' # plot(dend)
#' str(dend, 2)
get_branches_attr <- function(dend, the_attr, warning = TRUE,...) {
   if(class(dend) != "dendrogram")
   {
      if(warning) warning("dend wasn't of class dendrogram - class was set to dendrogram by
              the function - be sure it makes sense...")
      # I might make this S3 at some point, but I don't see a need for it...
      class(dend) <- "dendrogram"
      # dend <- as.dendrogram(dend)
   }
   sapply(dend, function(x) {attr(x, the_attr,...)})
}






#' @title Raise the height of a dendrogram tree
#' @export
#' @param dend dendrogram object
#' @param heiget_to_add how much height to add to all the branches (not leaves) in the dendrogram
#' @param ... passed on (not used)
#' @return A raised dendrogram
#' @examples
#' hc <- hclust(dist(USArrests[2:9,]), "com")
#' dend <- as.dendrogram(hc)
#' 
#' par(mfrow = c(1,2))
#' plot(dend, main = "original tree")
#' plot(raise.dendrogram(dend , 100), main = "Raised tree")
raise.dendrogram <- function(dend, heiget_to_add,...)
{
   # Only if you are not a leaf - then do things
   if(!is.leaf(dend)) 
   {
      # first - move through all the branches and implement this function on them (they will return higher...
      num_of_branches <- length(dend)
      for(i in seq_len(num_of_branches))
      {
         dend[[i]] <- raise.dendrogram(dend[[i]], heiget_to_add)   # go into the tree with recursion...
      }
      # change the height of our current branch
      attr(dend, "height")   <- attr(dend, "height") + heiget_to_add
   }
   return(dend)
}







#' @title unbranch trees
#' @export
#' @aliases 
#' unbranch.default
#' unbranch.dendrogram
#' unbranch.hclust
#' unbranch.phylo
#' @usage
#' unbranch(x, ...)
#' 
#' \method{unbranch}{dendrogram}(x, branch_becoming_root = 1, new_root_height, ...)
#' 
#' \method{unbranch}{hclust}(x, branch_becoming_root = 1, new_root_height, ...)
#' 
#' \method{unbranch}{phylo}(x, ...)
#' @param x tree (dendrogram/hclust) object
#' @param branch_becoming_root a numeric choosing the branch of the root which will become the new root (from left to right)
#' @param new_root_height the new height of the branch which will become the new root.
#' If the parameter is not given - the height of the original root is used.
#' @param ... passed on
#' @return An unbranched dendrogram
#' @seealso \link[ape]{unroot} {ape}
#' @examples
#' hc <- hclust(dist(USArrests[2:9,]), "com")
#' dend <- as.dendrogram(hc)
#' 
#' par(mfrow = c(1,3))
#' plot(dend, main = "original tree")
#' plot(unbranch(dend , 1), main = "unbranched tree (left branch)")
#' plot(unbranch(dend , 2), main = "tree without  (right branch)")
unbranch <- function(x, ...) UseMethod("unbranch")

#' @export
unbranch.default <- function(x,...) stop("object x must be a dendrogram/hclust/phylo object")


# ' @S3method unbranch dendrogram
#' @export
unbranch.dendrogram <- function(x, branch_becoming_root = 1, new_root_height,...)
{
   
   dend <- x # (since this function is based on dendrograms)
   if(is.leaf(dend[[branch_becoming_root]])) {
      warning("unbranch.dendrogram can't have the new root being a leaf.
The original dendrogram is returned.
Please choose another branch to be the root.")
      return(dend)
   }
   new_dend <- list()
   i_new_dend_branch <- 1
   
   # add branches from the new root branch to the new tree
   number_of_branches_in_root <- length(dend[[branch_becoming_root]])
   for(i in seq_len(number_of_branches_in_root))
   {
      new_dend[[i_new_dend_branch]] <- dend[[branch_becoming_root]][[i]]   # add the branches of the branc_becoming_root to the new tree
      i_new_dend_branch <- i_new_dend_branch + 1
   }
   
   # add all other branches of the old tree to the root of the new tree
   number_of_branches_in_dend <- length(dend)
   number_of_branches_in_dend_minus_root <-  number_of_branches_in_dend- 1
   branches_to_add_to_root <- seq_len(number_of_branches_in_dend)[-branch_becoming_root]   
      # id of branches to add to the root of the new tree
   for(i in seq_len(number_of_branches_in_dend_minus_root))
   {
      new_dend[[i_new_dend_branch]] <- dend[[branches_to_add_to_root[i] ]]	# add the branches of the branc_becoming_root to the new tree
      i_new_dend_branch <- i_new_dend_branch + 1
   }
   
   # set the proper attributes of the root of the new tree 
   if(!missing(new_root_height)){			
      attr(new_dend, "height") <- new_root_height
   } else {
      attr(new_dend, "height") <- attr(dend, "height") # + attr(dend[[branch_becoming_root]], "height") 
   }
   class(new_dend) <- 'dendrogram'
   attr(new_dend, "members") <- sum(get_branches_attr(new_dend, "members")) # the new members of the root is the sum of the members in all of his branches
   attr(new_dend, "midpoint") <- 
      suppressWarnings(mean(get_branches_attr(new_dend, "midpoint"), na.rm = TRUE) )      
   # the new midpoint of the root is the mean of the midpoint in all of his branches
   # if some are NA, they are ignored

   # Bad idea: we only use labels for the leafs...
   #    attr(new_dend, "label") <- "merged root" # might cause problems in the future?
   new_dend <- suppressWarnings(stats_midcache.dendrogram(new_dend)) # might through warnings if we have 3 branches (but it will keep the "midpoints" in check 
   
   return(new_dend)
}



# ' @S3method unbranch hclust
#' @export
unbranch.hclust <- function(x, branch_becoming_root = 1, new_root_height, ...) {
   x_dend <- as.dendrogram(x)
   x_dend_unbranch <- unbranch(x_dend, branch_becoming_root , new_root_height, ...)
   x_unbranch <- as_hclust_fixed(x_dend_unbranch, x)  
   
   return(x_unbranch)
}


# ' @S3method unbranch phylo
#' @export
unbranch.phylo <- function(x, ...) {
		# ape::unbranch(phy = x)
   # require(ape)
   x_dend <- as.dendrogram(x)
   x_dend_unbranch <- unbranch(x_dend, ...) # branch_becoming_root , new_root_height,
   x_unbranch <- ape::as.phylo(x_dend_unbranch)  

   return(x_unbranch)
}

















#' @title Flatten the branches of a dendrogram's root
#' @description
#' The function makes sure the two branches of the root of a dendrogram will have the same height.  
#' The user can choose how to decide which height to use.
#' @export
#' @param dend dendrogram object
#' @param FUN how to choose the new height of both branches (defaults to taking the max between the two)
#' @param new_height overrides FUN, and sets the new height of the two branches manually
#' @param ... passed on (not used)
#' @return A dendrogram with both of the root's branches of the same height
#' @examples
#' hc <- hclust(dist(USArrests[2:9,]), "com")
#' dend <- as.dendrogram(hc)
#' attr(dend[[1]], "height") <- 150 # make the height un-equal
#' 
#' par(mfrow = c(1,2))
#' plot(dend, main = "original tree")
#' plot(flatten.dendrogram(dend), main = "Raised tree")
flatten.dendrogram <- function(dend, FUN = max, new_height,...)
{
   if(missing(new_height)) {
      dend_branches_height <- get_branches_attr(dend, "height")
      new_height <- FUN(dend_branches_height)      
   }
   for(i in seq_len(length(dend))) attr(dend[[i]], "height") <- new_height
   return(dend)
}

