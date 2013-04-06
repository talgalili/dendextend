
#' @title Counts the number of leaves in a tree
#' @aliases 
#' nleaves.default
#' nleaves.dendrogram
#' nleaves.hclust
#' @description Counts the number of leaves in a tree (dendrogram or hclust).
#' @usage
#' nleaves(x, ...)
#' 
#' \method{nleaves}{dendrogram}(x, ...)
#' 
#' \method{nleaves}{hclust}(x, ...)
#' @export
#' @param x tree object (dendrogram or hclust)
#' @param ... not used
#' @details 
#' The idea for the name is from functions like ncol, and nrow.
#' 
#' Also, it is worth noting that the nleaves.dendrogram is based on order.dendrogram instead of labels.dendrogram since the first is MUCH faster than the later.
#' @return The number of leaves in the tree
#' @seealso \link{nrow}, \link{count_terminal_nodes}
#' @examples
#' hc <- hclust(dist(USArrests[1:5,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' nleaves(dend) # 5
#' nleaves(hc) # 5
nleaves <- function(x, ...) UseMethod("nleaves")

#' @export
nleaves.default <- function(x,...) stop("object x must be a dendrogram or hclust object")

#' @S3method nleaves dendrogram
nleaves.dendrogram <- function(x,...) length(order.dendrogram(x))

#' @S3method nleaves hclust
nleaves.hclust <- function(x,...) length(x$order)








#' @title Counts the number of terminal nodes (merging 0 nodes!)
#' @export
#' @description This function counts the number of "practical" terminal nodes (nodes which are not leaves, but has 0 height to them are considered "terminal" nodes).
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
#' @seealso \link{nleaves}
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




#' @S3method head dendrogram
#' @import utils
head.dendrogram <- function(x, n = 3L, ...) {
   str(x, max.leve = n,...)
   cat( "etc...","\n")
}
# hc <- hclust(dist(USArrests), "ave")
# dend <- as.dendrogram(hc)
# str(dend)
# head(dend)
# Some thoughts on imports: http://obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/
# and: http://stackoverflow.com/questions/8637993/better-explanation-of-when-to-use-imports-depends

# methods(nleaves)
