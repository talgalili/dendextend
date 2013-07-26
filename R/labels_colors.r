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
# You should have received a copy of the GNU General Public License
# along with dendextend.  If not, see <http://www.gnu.org/licenses/>.





#' @title Retrieve/assign colors to the labels of a dendrogram
#' @aliases 
#' labels_colors<-
#' @usage
#' labels_colors(object, ...)
#' 
#' labels_colors(object, ...) <- value
#' @export
#' @param object a dendrogram object 
#' @param ... not used
#' @param value a vector of colors to be used as new label's colors for the dendrogram
#' @source Inspired by the code in the example of \link{dendrapply}, so credit should also go to Martin Maechler.
#' @return A vector with the dendrogram's labels colors (or a colored dendrogram, in case assignment is used)
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' # Defaults:
#' labels_colors(dend)
#' plot(dend)
#' 
#' # let's add some color:
#' labels_colors(dend) <- 2:4
#' labels_colors(dend)
#' plot(dend)
#' 
#' # changing color to black
#' labels_colors(dend) <- 1
#' labels_colors(dend)
#' plot(dend)
#' 
#' # removing color (and the nodePar completely - if it has no other attributed but lab.col)
#' labels_colors(dend) <- NULL
#' labels_colors(dend)
#' plot(dend)
labels_colors <- function (object,...) {
   col <- NULL
   
   get.col.from.leaf <- function(dend_node)
   {
      if(is.leaf(dend_node))
      {   		
         i_leaf_number <<- i_leaf_number + 1
         col[i_leaf_number] <<- attr(dend_node, "nodePar")[["lab.col"]]
      }
      return(dend_node)
   }
   # mtrace(".change.label.by.mat")
   i_leaf_number <- 0
   dendrapply(object, get.col.from.leaf)
   return(col)
}

#' @export
"labels_colors<-" <- function (object, ..., value) {
   col <- value
   leaves_length <- length(order.dendrogram(object)) # length(labels(object)) # it will be faster to use order.dendrogram than labels...   
   if(leaves_length > length(col)) {
      warning("Length of color vector was shorter then the number of leaves - vector color recycled")
      col <- rep(col, length.out = leaves_length)
   }	 	
   
   set.col.to.leaf <- function(dend_node)
   {
      if(is.leaf(dend_node))
      {			
         i_leaf_number <<- i_leaf_number + 1
         if(is.null(attr(dend_node, "nodePar"))) {
            attr(dend_node, "nodePar") <- list(lab.col= col[i_leaf_number] )
         } else {            
            attr(dend_node, "nodePar") <- within(attr(dend_node, "nodePar"), {lab.col <- col[i_leaf_number]}) # this way it doesn't erase other nodePar values (if they exist)
         }
         
         if(length(attr(dend_node, "nodePar")) == 0) attr(dend_node, "nodePar") <- NULL # remove nodePar if it is empty
      }
      return(dend_node)
   }   
   i_leaf_number <- 0
   new_dend_object <- dendrapply(object, set.col.to.leaf)
   return(new_dend_object)
}


