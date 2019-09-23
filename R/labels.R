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










#' ###################
## Adding labels assignment as an S3 method (and extending it to include hclust)
## Also order labels


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
#' @export
#' @rdname labels-assign
#' @description "label" assignment operator for vectors, dendrogram, and hclust classes.
#'
#' @param object a variable name (possibly quoted) who's label are to be updated
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
#' @source
#' The functions here are based on code by Gavin and kohske from
#' (adopted to dendrogram by Tal Galili):
#' \url{http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question}
#' Also with some ideas from Gregory Jefferis's dendroextras package.
#' @seealso \code{\link{labels}}
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
#' # Example for using the assignment with dendrogram and hclust objects:
#' hc <- hclust(dist(USArrests[1:3, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' labels(hc) # "Arizona" "Alabama" "Alaska"
#' labels(hc) <- letters[1:3]
#' labels(hc) # "a" "b" "c"
#' labels(dend) # "Arizona" "Alabama" "Alaska"
#' labels(dend) <- letters[1:3]
#' labels(dend) # "a" "b" "c"
#' labels(dend) <- LETTERS[1:2] # will produce a warning
#' labels(dend) # "A" "B" "A"
#' labels(dend) <- LETTERS[4:6] # will replace the labels correctly
#' # (the fact the tree had duplicate labels will not cause a problem)
#' labels(dend) # "D" "E" "F"
`labels<-` <- function(object, ..., value) {
  UseMethod("labels<-")
}

# example("labels<-")
# ?"labels<-"


#' @export
#' @rdname labels-assign
`labels<-.default` <- function(object, ..., value) {
  if (length(value) < length(object)) {
    warning("The lengths of the new labels is shorter than the length of the object - labels are recycled.")
    names(object) <- rep(value, length.out = length(object)) # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
  } else {
    names(object) <- value # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
  }
  object
}



#' @export
#' @rdname labels-assign
`labels<-.dendrogram` <- function(object, ..., value) {
  # credit for the help on how to write this type of function goes to:
  # Gavin Simpson and also kohske, see here:
  # http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question

  if (missing(value)) {
    if (dendextend_options("warn")) warning("value is missing, returning the dendrogram as is")
    return(object)
  }


  # deals with wrong length of new labels VS tree size
  # new_labels <- as.character(value) # I shouldn't force this to a character
  new_labels <- value
  new_labels_length <- length(new_labels)
  leaves_length <- nleaves(object) # labels(object) # it will be faster to use order.dendrogram than labels...
  if (new_labels_length < leaves_length) {
    warning("The lengths of the new labels is shorter than the number of leaves in the dendrogram - labels are recycled.")
    new_labels <- rep(new_labels, length.out = leaves_length)
  }

  .change.label.LTR <- function(dend_node) {
    if (is.leaf(dend_node)) {
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



#' @export
#' @rdname labels-assign
labels.hclust <- function(object, order = TRUE, ...) {
  if (order) {
    labels_obj <- as.character(object$labels[object$order])
  } else {
    labels_obj <- as.character(object$labels)
  }

  return(labels_obj)
}


#' @export
#' @rdname labels-assign
`labels<-.hclust` <- function(object, ..., value) {
  if (length(value) < length(object$labels)) {
    warning("The lengths of the new labels is shorter than the number of leaves in the hclust - labels are recycled.")
  }

  object$labels[object$order] <- value # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
  return(object)
}



#' @export
#' @rdname labels-assign
labels.phylo <- function(object, ...) {
  object$tip.label
}




#' @export
#' @rdname labels-assign
`labels<-.phylo` <- function(object, ..., value) {
  if (missing(value)) {
    if (dendextend_options("warn")) warning("value is missing, returning the dendrogram as is")
    return(object)
  }


  # deals with wrong length of new labels VS tree size
  new_labels <- as.character(value)
  new_labels_length <- length(new_labels)
  leaves_length <- nleaves(object) # labels(object) # it will be faster to use order.dendrogram than labels...
  if (new_labels_length < leaves_length) {
    warning("The lengths of the new labels is shorter than the number of leaves in the dendrogram - labels are recycled.")
    new_labels <- rep(new_labels, length.out = leaves_length)
  }

  object$tip.label <- new_labels

  return(object)
}




#
# # ' @title "label" assignment operator for matrix class
# # ' @S3method labels matrix
# # ' @export
# labels.matrix <- function(object, which = c("colnames","rownames"), ...) {
#    if(missing(which))
#       which <- "colnames"
#    which <- match.arg(which)
#    if(which == "colnames") {
#       out <- colnames(object)
#    } else {
#       out <- rownames(object)
#    }
#    out
# }
# # example("labels.matrix")
# # ?"labels.matrix"
#
# # ' @title "label" assignment operator - matrix
# # ' @S3method labels<- matrix
# # ' @export
# # ' @keywords internal
# 'labels<-.matrix' <- function(object, which = c("colnames","rownames"), ..., value) {
#    if(missing(which))
#       which <- "colnames"
#    which <- match.arg(which)
#
#    # I'm using ncol and nrow instead of length(colnames(object))
#    # since if the object has no colnames, their length will be 0
#
#    if(which == "colnames") {
#
#       if(length(value) < ncol(object)) {
#          warning("The lengths of the new labels is shorter than the length of the object's colnames - labels are recycled.")
#          colnames(object) <- rep(value, length.out = ncol(object)) # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
#       } else {
#          colnames(object) <- value # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
#       }
#
#    } else {
#
#       if(length(value) < nrow(object)) {
#          warning("The lengths of the new labels is shorter than the length of the object's rownames - labels are recycled.")
#          rownames(object) <- rep(value, length.out = nrow(object)) # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
#       } else {
#          rownames(object) <- value # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
#       }
#
#    }
#    object
# }
#
#





#' @title order.dendrogram<- assignment operator
#' @export
#' @rdname order.dendrogram-assign
#' @description order.dendrogram<- assignment operator.  This is useful in cases where some object is turned into a dendrogram but its leaves values (the order) are all mixed up.
#' @param object a variable name (possibly quoted) who's label are to be updated
#' @param ... parameters passed (not currently in use)
#' @param value a value to be assigned to object's leaves value (their "order")
#' @return dendrogram with updated order leaves values
#' @seealso \code{\link{order.dendrogram}}, \code{\link{labels<-}}
#' @examples
#' ################
#' # Example for using the assignment with dendrogram and hclust objects:
#' hc <- hclust(dist(USArrests[1:4, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' str(dend)
#' order.dendrogram(dend) # 4 3 1 2
#' order.dendrogram(dend) <- 1:4
#' order.dendrogram(dend) # 1 2 3 4
#' str(dend) # the structure is still fine.
#'
#' # This function is very useful if we try playing with subtrees
#' # For example:
#' hc <- hclust(dist(USArrests[1:6, ]), "ave")
#' dend <- as.dendrogram(hc)
#' sub_dend <- dend[[1]]
#' order.dendrogram(sub_dend) # 4 6
#' # now using as.hclust(sub_dend) will cause trouble:
#' # labels(as.hclust(sub_dend)) # As of R 3.1.1-patched - this will produce an Error (as it should) :)
#' # let's fix it:
#'
#' order.dendrogram(sub_dend) <- rank(order.dendrogram(sub_dend), ties.method = "first")
#' labels(as.hclust(sub_dend)) # We now have labels :)
`order.dendrogram<-` <- function(object, ..., value) {

  # notice that:  is.integer(as.numeric(1L)) == FALSE
  # is.numeric(1L) == TRUE
  # but the next line will not be triggered if value is an integer!
  if (!is.numeric(value)) {
    warning("'value' is not numeric - coerced using as.numeric")
    value <- as.numeric(value)
  }
  if (!is.integer(value)) {
    warning("'value' is not integer - coerced using as.integer")
    value <- as.integer(value)
  }
  new_order_values <- value

  new_labels_length <- length(new_order_values)
  leaves_length <- nleaves(object) # labels(object) # it will be faster to use order.dendrogram than labels...

  if (new_labels_length < leaves_length) {
    warning("The lengths of the new labels is shorter than the number of leaves in the dendrogram - labels are recycled.")
    new_order_values <- rep(new_order_values, length.out = leaves_length)
  }


  # I can't think of a case were someone would like to use non unique values.  But just in case - I will warn them that they are doing so...
  if (any(duplicated(new_order_values))) {
    warning("The value you wish to insert into the dendrogram 'objects' has duplicate values.  Often you are likely to prefer unique values in your vector.  So be sure to check that you've used the correct vector here...")
  }
  #    any(duplicated(c(1:4, 2)))


  .change_order_LTR <- function(dend_node) {
    if (is.leaf(dend_node)) {
      attr_backup <- attributes(dend_node)
      dend_node <- new_order_values[i_leaf_number]
      attributes(dend_node) <- attr_backup # fix attributes
      i_leaf_number <<- i_leaf_number + 1 # this saves us from cases of duplicate enteries...
    }
    return(unclass(dend_node))
  }

  i_leaf_number <- 1
  new_dend_object <- dendrapply(object, .change_order_LTR)
  class(new_dend_object) <- "dendrogram"

  return(new_dend_object)
}



# methods("labels")
# methods("labels<-")
# example("labels.matrix")



#' @title Ordering of the Leaves in a hclust Dendrogram
#' @description Ordering of the Leaves in a hclust Dendrogram. Like \link{order.dendrogram}.
#' @export
#' @param x ab hclust object a distance matrix.
#' @param ... Ignored.
#' @seealso
#' \link{order.dendrogram}
#' @return
#' A vector with length equal to the number of
#' leaves in the hclust dendrogram is returned.
#' From r <- order.hclust(), each element is
#' the index into the original data
#' (from which the hclust was computed).
#'
#' @examples
#'
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#' hc <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust()
#' # dend <- hc %>% as.dendrogram
#' order.hclust(hc)
order.hclust <- function(x, ...) {
  if (!is.hclust(x)) stop("x is not a hclust object.")
  x$order
}























#' @title Set/place new labels in a dendrogram
#' @export
#' @description
#' Convenience functions for updating the labels of a dendrogram.
#' set_labels and place_labels differs in their assumption about the order of the labels.
#' * set_labels assumes the labels are in the same order as that of the labels in the dendrogram.
#' * place_labels assumes the labels has the same order as that of the items in the original data matrix.
#'    This is useful for renaming labels based on some other columns in the data matrix.
#'
#' @aliases
#' place_labels
#' @param dend a \link{dendrogram} object
#' @param labels A vector of values to insert in the labels of a dendrogram.
#' @param ... Currently ignored.
#' @return The updated \link{dendrogram} object
#' @author Tal Galili, Garrett Grolemund
#' @seealso \code{\link{labels}}, \code{\link{set}}
#' @examples
#'
#' ss <- c(
#'   50, 114, 17, 102, 76, 10, 107, 84, 31, 37, 49, 106, 44, 119,
#'   104, 145, 67, 85, 12, 77, 22, 136, 38, 135, 70
#' )
#'
#' small_iris <- iris[ss, ]
#'
#' small_iris[, -5] %>%
#'   dist() %>%
#'   hclust(method = "complete") %>%
#'   as.dendrogram() %>%
#'   color_branches(k = 3) %>%
#'   color_labels(k = 3) %>%
#'   plot()
#'
#' # example for using place_labels
#' small_iris[, -5] %>%
#'   dist() %>%
#'   hclust(method = "complete") %>%
#'   as.dendrogram() %>%
#'   color_branches(k = 3) %>%
#'   color_labels(k = 3) %>%
#'   place_labels(paste(small_iris$Species, 1:25, sep = "_")) %>%
#'   plot()
#'
#' # example for using set_labels
#' small_iris[, -5] %>%
#'   dist() %>%
#'   hclust(method = "complete") %>%
#'   as.dendrogram() %>%
#'   color_branches(k = 3) %>%
#'   color_labels(k = 3) %>%
#'   set_labels(1:25) %>%
#'   plot()
set_labels <- function(dend, labels, ...) {
  labels(dend) <- labels
  dend
}


#' @export
place_labels <- function(dend, labels, ...) {
  set_labels(dend, labels[order.dendrogram(dend)])
}
