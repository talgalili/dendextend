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
   
   get.col.to.leaf <- function(dend_node)
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
   dendrapply(object, get.col.to.leaf)
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
   
   add.col.to.leaf <- function(dend_node)
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
   new_dend_object <- dendrapply(object, add.col.to.leaf)
   return(new_dend_object)
}


# # TODO - what happens if nodePar has other values?
# attr(dend[[1]], "nodePar") <- list(lab.col= 2, aaa=3)          
# attr(dend[[1]], "nodePar") <- within(attr(dend[[1]], "nodePar"), {lab.col <- 3})
# attr(dend[[1]], "nodePar")
# plot(dend)



#' @title Counts the actual number of terminal nodes
#' @export
#' @description This function counts the number of "practical" terminal nodes.  
#' If the tree is standard, that would simply be the number of leaves (only the leaves will have height 0).
#' However, in cases where the tree has several nodes (before the leaves) with 0 height, 
#' the count_terminal_nodes counts such nodes as terminal nodes
#' 
#' The function is recursive in that it either returns 1 if it reached a terminal node (either a leaf or a 0 height node),
#' else: it will count the number of terminal nodes in each of its sub-nodes, sum them up, and return them.
#' 
#' @param dend_node a dendrogram object for which to count its number of terminal nodes (leaves or 0 height nodes).
#' @param ... not used
#' @return The number of terminal nodes (excluding the leaves of nodes of height 0)
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' ###
#' # Trivial case
#' count_terminal_nodes(dend) # 3 terminal nodes
#' length(labels(dend)) # 3 - the same number
#' plot(dend, main = "This is considered a tree \n with THREE terminal nodes (leaves)") # while we have 3 leaves, in practice we have only 2 terminal nodes (this is a feature, not a bug.)
#' 
#' ###
#' # NON-Trivial case
#' str(dend)
#' attr(dend[[2]], "height") <- 0
#' count_terminal_nodes(dend) # 2 terminal nodes, why? see this plot:
#' plot(dend, main = "This is considered a tree \n with TWO terminal nodes only") # while we have 3 leaves, in practice we have only 2 terminal nodes (this is a feature, not a bug.)
count_terminal_nodes <- function(dend_node,...) {
   
   # a recursive function for counting the number of first encountered zero nodes
   # if you are a leaf, or a node with height 0, return 1
   if(is.leaf(dend_node) | attr(dend_node, "height") == 0) return(1L)
   
   # else : # go through all of the childrens and apply count.first.zero.nodes		
   terminal_nodes_counter <- 0
   for(i in seq_len(length(dend_node))){
      terminal_nodes_counter <- terminal_nodes_counter + # let each sub-node add its counts
         count_terminal_nodes(dend_node[[i]],...)
   }		
   return(terminal_nodes_counter)
}




#' @title unclass an entire dendrogram tree
#' @export
#' @param dend a dendrogram object 
#' @param ... not used
#' @return The list which was the dendrogram (but without a class)
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' 
#' unclass(dend) # this only returns a list with two dendrogram objects inside it.
#' str(dend) # this is a great way to show a dendrogram, but it doesn't help us understand how the R object is built.
#' unclass_dend(dend) # this only returns a list with two dendrogram objects inside it.
#' str(unclass_dend(dend)) # NOW we can more easily understand how the dendrogram object is structured...
unclass_dend <- function(dend,...)
{
   # I could have also made this into a method
   # unclass_dendrogram
   # But decided not to, so to allow unclass to work as usual in case someone wants to use it only on one branch.
   if(!is.leaf(dend))
   {
      for(i in seq_len(length(dend)))
      {
         dend[[i]] <- unclass_dend(dend[[i]])
      }
   }
   dend <- unclass(dend)
   return(dend)
}



# example(count_terminal_nodes)
# example(labels_colors)

