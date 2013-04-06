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




#' @title Convert dendrogram Objects to Class hclust 
#' @description Convert dendrogram Objects to Class hclust while preserving the call/method/dist.method values of the original hclust object (hc)
#' @export
#' @param x a dendrogram object to turn into hclust
#' @param hc an old hclust object from which to re-use the call/method/dist.method values
#' @param ... passed to as.hclust
#' @return An hclust object (from a dendrogram) with the original hclust call/method/dist.method values
#' @seealso \link{as.hclust}
#' @examples
#' hc <- hclust(dist(USArrests[1:3,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' as.hclust(dend)
#' as_hclust_fixed(dend, hc)
as_hclust_fixed <- function(x, hc, ...) {
   x <- as.hclust(x) 
   
   # these elements are removed after using as.hclust - so they have to be manually re-introduced into the object.
   x$call <- hc$call
   x$method <- hc$method
   x$dist.method <- hc$dist.method   
   
   return(x)
}
