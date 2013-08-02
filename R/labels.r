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










####################
## Adding labels assignment as an S3 method (and extending it to include hclust)
## Also order labels
####################


### Includes the functions:
# labels<-.default 
# labels<-.dendrogram 
# labels.hclust 
# labels<-.hclust
# labels.matrix 
# labels<-.matrix
#
# "order.dendrogram<-" 
   




# turning label insertion function into a method:

#' @title "label" assignment operator
#' @description "label" assignment operator for vectors, dendrogram, and hclust classes.
#' @rdname labels-assign
#' @aliases 
#' labels<-.default 
#' labels<-.dendrogram 
#' labels.hclust 
#' labels<-.hclust
#' labels.matrix 
#' labels<-.matrix
#' @usage
#' labels(object, ...) <- value
#' 
#' \method{labels}{matrix}(object, which = c("colnames", "rownames"), ...)
#' 
#' \method{labels}{matrix}(object, which = c("colnames", "rownames"), ...) <- value
#' 
#' \method{labels}{dendrogram}(object, ...) <- value
#' 
#' \method{labels}{hclust}(object, order = TRUE,...)
#' 
#' \method{labels}{hclust}(object, ...) <- value
#' @param object a variable name (possibly quoted) who's label are to be updated
#' @param which "colnames" or "rownames", to which of the two should labels refer to.
#' @param ... parameters passed (not currently in use)
#' @param value a value to be assigned to object's label
#' @param order default is FALSE. Only relevant for extracting labels from an
#'  \link{hclust} object (with labels.hclust). Setting order=TRUE will return
#'  labels in their order in the dendrogram, instead of the riginal labels order
#'  retained from object$labels - which ususally corresponding to 
#'  the row or column names of the \link{dist} object provided to
#'  the \link{hclust} function.
#' @return The updated object
#' @author Gavin Simpson, Tal Galili 
#' (with some ideas from Gregory Jefferis's dendroextras package)
#' @export
#' @source 
#' The functions here are based on code by Gavin and kohske from 
#' (adopted to dendrogram by Tal Galili):
#' \url{http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question}
#' Also with some ideas from Gregory Jefferis's dendroextras package.
#' @seealso \code{\link{labels}}, \code{\link{labels.matrix}}
#' @examples
#' x <- 1:3 
#' labels(x)
#' labels(x) <- letters[1:3]
#' labels(x) # [1] "a" "b" "c"
#' x
#' # a b c 
#' # 1 2 3 
#' 
#' 
#' # get("labels<-")
#' 
#' ################
#' # Example for matrix objects
#' 
#' x <- matrix(1:9, 3,3)
#' labels(x)
#' x
#' labels(x) <- letters[1:3]
#' x
#' labels(x, which = "rownames") <- LETTERS[24:26]
#' x
#' #  a b c
#' #X 1 4 7
#' #Y 2 5 8
#' #Z 3 6 9
#' 
#' ################
#' # Example for using the assignment with dendrogram and hclust objects:
#' hc <- hclust(dist(USArrests[1:3,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' labels(hc) # "Arizona" "Alabama" "Alaska" 
#' labels(hc)  <- letters[1:3]
#' labels(hc)# "a" "b" "c"
#' labels(dend) # "Arizona" "Alabama" "Alaska" 
#' labels(dend) <- letters[1:3]
#' labels(dend) # "a" "b" "c"
#' labels(dend) <- LETTERS[1:2] # will produce a warning
#' labels(dend) # "A" "B" "A"
#' labels(dend) <- LETTERS[4:6] # will replace the labels correctly 
#' # (the fact the tree had duplicate labels will not cause a problem)
#' labels(dend) # "D" "E" "F"
"labels<-" <- function(object,..., value) UseMethod("labels<-")

# example("labels<-")
# ?"labels<-"


# ' @title "label" assignment operator - default
#' @export
"labels<-.default" <- function(object,..., value) {
   if(length(value) < length(object)) {
      warning("The lengths of the new labels is shorter than the length of the object - labels are recycled.")
      names(object) <- rep(value, length.out = length(object)) # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"      
   } else {
      names(object) <- value # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"      
   }
   object
}



# ' @title "label" assignment operator - dendrogram
#' @export
#' @S3method labels<- dendrogram
"labels<-.dendrogram" <- function(object,..., value) {
   # credit for the help on how to write this type of function goes to:
   # Gavin Simpson and also kohske, see here:
   # http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question

   # deals with wrong length of new labels VS tree size
   new_labels <- as.character(value)
   new_labels_length <- length(new_labels)
   leaves_length <- nleaves(object) # labels(object) # it will be faster to use order.dendrogram than labels...   
   if(new_labels_length < leaves_length) {
      warning("The lengths of the new labels is shorter than the number of leaves in the dendrogram - labels are recycled.")
      new_labels <- rep(new_labels, length.out = leaves_length)
   }
   
   .change.label.LTR <- function(dend_node)
   {
      if(is.leaf(dend_node))
      {			
         attr(dend_node, "label") <- new_labels[i_leaf_number]
         i_leaf_number <<- i_leaf_number + 1 # this saves us from cases of duplicate enteries...
      }
      return(unclass(dend_node)) # the "unclass" is important since dendrapply adds the dendrogram class to each node (which is irrelevent)
   }
   
   i_leaf_number <- 1
   new_dend_object <- dendrapply(object, .change.label.LTR)
   class(new_dend_object) <- "dendrogram"
   
   return(new_dend_object)
}


# ' @title Find Labels from hclust Object
#' @export
#' @S3method labels hclust
labels.hclust <- function(object, order = TRUE, ...)  {
   if(order) {
      labels_obj <- as.character(object$labels[object$order])      
   } else {
      labels_obj <- as.character(object$labels)
   }   
   
   return(labels_obj)
}



# ' @title "label" assignment operator - hclust
#' @export
#' @S3method labels<- hclust
"labels<-.hclust" <- function(object,..., value) {
   if(length(value) < length(object$labels)) {
      warning("The lengths of the new labels is shorter than the number of leaves in the hclust - labels are recycled.")
   }
   
   object$labels[object$order] <- value  # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
   return(object)
}






# ' @title "label" assignment operator for matrix class
#' @export
#' @S3method labels matrix
labels.matrix <- function(object, which = c("colnames","rownames"), ...) {
   if(missing(which))
      which <- "colnames"
   which <- match.arg(which)
   if(which == "colnames") {
      out <- colnames(object)
   } else {
      out <- rownames(object)
   }
   out
}
# example("labels.matrix")
# ?"labels.matrix"

# ' @title "label" assignment operator - matrix
#' @export
#' @S3method labels<- matrix
#' @keywords internal
'labels<-.matrix' <- function(object, which = c("colnames","rownames"), ..., value) {
   if(missing(which))
      which <- "colnames"
   which <- match.arg(which)
   
   # I'm using ncol and nrow instead of length(colnames(object))
   # since if the object has no colnames, their length will be 0
   
   if(which == "colnames") {
      
      if(length(value) < ncol(object)) {
         warning("The lengths of the new labels is shorter than the length of the object's colnames - labels are recycled.")
         colnames(object) <- rep(value, length.out = ncol(object)) # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"      
      } else {
         colnames(object) <- value # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"      
      }     
      
   } else {

      if(length(value) < nrow(object)) {
         warning("The lengths of the new labels is shorter than the length of the object's rownames - labels are recycled.")
         rownames(object) <- rep(value, length.out = nrow(object)) # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"      
      } else {
         rownames(object) <- value # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"      
      }     
      
   }
   object
}







#' @title order.dendrogram<- assignment operator
#' @export
#' @rdname order.dendrogram-assign
#' @description order.dendrogram<- assignment operator.  This is useful in cases where some object is turned into a dendrogram but its leaves values (the order) are all mixed up.
#' @param object a variable name (possibly quoted) who's label are to be updated
#' @param ... parameters passed (not currently in use)
#' @param value a value to be assigned to object's leaves value (their "order")
#' @usage
#' order.dendrogram(object, ...) <- value
#' @return dendrogram with updated order leaves values
#' @seealso \code{\link{order.dendrogram}}, \code{\link{labels<-}} 
#' @examples
#' ################
#' # Example for using the assignment with dendrogram and hclust objects:
#' hc <- hclust(dist(USArrests[1:4,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' str(dend)
#' order.dendrogram(dend) # 4 3 1 2
#' order.dendrogram(dend) <- 1:4
#' order.dendrogram(dend) # 1 2 3 4
#' str(dend) # the structure is still fine.
"order.dendrogram<-" <- function(object,..., value) {
   new_labels <- as.numeric(value)
   new_labels_length <- length(new_labels)
   leaves_length <- nleaves(object) # labels(object) # it will be faster to use order.dendrogram than labels...   
   if(new_labels_length < leaves_length) {
      warning("The lengths of the new labels is shorter than the number of leaves in the dendrogram - labels are recycled.")
      new_labels <- rep(new_labels, length.out = leaves_length)
   }
   
   .change.order.LTR <- function(dend_node)
   {
      if(is.leaf(dend_node))
      {   	
         attr_backup <- attributes(dend_node)
         i_leaf_number <<- i_leaf_number + 1 # this saves us from cases of duplicate enteries...
         dend_node <- new_labels[i_leaf_number]
         attributes(dend_node) <- attr_backup # fix attributes         
      }
      return(dend_node)
   }
   
   i_leaf_number <- 0
   new_dend_object <- dendrapply(object, .change.order.LTR)
   
   return(new_dend_object)
}



# methods("labels")
# methods("labels<-")
# example("labels.matrix")
