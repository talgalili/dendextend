## I'm not sure if I'll use it yet. But just in case...
#######
#######


### # ' @aliases 
### # ' set_leaves_attr
### # ' @usage
### # ' get_leaves_attr(object, labels = TRUE,...)
### # ' 
### # ' set_leaves_attr(object, ...) <- value
#' @title Get/set attributes of dendrogram's leaves
#' @export
#' @param object a dendrogram object 
#' @param attribute character scaler of the attribute (\code{attr})
#' we wish to get/set from the leaves
#' @param ... not used
#' @source Heavily inspired by the code in the 
#' function \code{labels.dendrogram}, 
#' so credit should go to Martin Maechler.
#' @return 
#' A vector with the dendrogram's leaves attribute
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' # get_leaves_attr(dend) # error :)
#' get_leaves_attr(dend, "label")
#' labels(dend, "label")
#' get_leaves_attr(dend, "height") # should be 0's
#' get_leaves_attr(dend, "leaf") # should be TRUE's
#' get_leaves_attr(dend, "members") # should be TRUE's
get_leaves_attr <- function (object, attribute, ...) {
   if(!inherits(object,'dendrogram')) warning("'object' should be a dendrogram.")   
   if(missing(attribute)) stop("'attribute' parameter is missing.")
   
   get_attr_from_leaf <- function(dend_node) {
      if(is.leaf(dend_node)) attr(dend_node, attribute)
   }   
   
   return(unlist(dendrapply(object, get_attr_from_leaf)))   
}
