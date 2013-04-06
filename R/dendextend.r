


#' @title Counts the actual number of terminal nodes
#' @export
#' @description This function counts the number of "practical" terminal nodes.  
#' If the tree is standard, that would simply be the number of leaves (only the leaves will have height 0).
#' However, in cases where the tree has several nodes (before the leaves) with 0 height, 
#' the count.terminal.nodes counts such nodes as terminal nodes
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
#' count.terminal.nodes(dend) # 3 terminal nodes
#' length(labels(dend)) # 3 - the same number
#' plot(dend, main = "This is considered a tree \n with THREE terminal nodes (leaves)") # while we have 3 leaves, in practice we have only 2 terminal nodes (this is a feature, not a bug.)
#' 
#' ###
#' # NON-Trivial case
#' str(dend)
#' attr(dend[[2]], "height") <- 0
#' count.terminal.nodes(dend) # 2 terminal nodes, why? see this plot:
#' plot(dend, main = "This is considered a tree \n with TWO terminal nodes only") # while we have 3 leaves, in practice we have only 2 terminal nodes (this is a feature, not a bug.)
count.terminal.nodes <- function(dend_node,...) {
   
   # a recursive function for counting the number of first encountered zero nodes
   # if you are a leaf, or a node with height 0, return 1
   if(is.leaf(dend_node) | attr(dend_node, "height") == 0) return(1L)
   
   # else : # go through all of the childrens and apply count.first.zero.nodes		
   terminal_nodes_counter <- 0
   for(i in seq_len(length(dend_node))){
      terminal_nodes_counter <- terminal_nodes_counter + # let each sub-node add its counts
         count.terminal.nodes(dend_node[[i]],...)
   }		
   return(terminal_nodes_counter)
}

# example(count.terminal.nodes)
