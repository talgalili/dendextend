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





#' @title Retrieve/assign colors to the labels of a dendrogram
#' @export
#' @rdname labels_colors
#' @description Retrieve/assign colors to the labels of a dendrogram. Note that usually dend objects come without any color assignment (and the output will be NULL, until colors are assigned).
#' @param dend a dendrogram object
#' @param labels Boolean (default is TRUE), should the returned vector of colors
#' return with the leaves labels as names.
#' @param ... not used
#' @param value a vector of colors to be used as new label's colors for the dendrogram
#' @source Heavily inspired by the code in the example of \link{dendrapply},
#' so credit should go to Martin Maechler.
#' I also implemented some ideas from Gregory Jefferis's dendroextras package
#' (having the "names" of the returned vector be the labels).
#' @return
#' A vector with the dendrogram's labels colors (or a colored dendrogram,
#' in case assignment is used). The colors are labeled.
#' @seealso \code{\link[dendextend]{cutree}},\code{\link{dendrogram}},
#' \code{\link{hclust}}, \code{\link{color_labels}}, \code{\link{color_branches}},
#' \link{assign_values_to_leaves_edgePar}, \link{get_leaves_branches_col}
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3, ]), "ave")
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
#'
#' # doesn't work...
#' #  get_nodes_attr(dend, "nodePar", include_branches = FALSE)
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
labels_colors <- function(dend, labels = TRUE, ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  col <- NULL

  get_col_from_leaf <- function(dend_node) {
    if (is.leaf(dend_node)) {
      i_leaf_number <<- i_leaf_number + 1
      col[i_leaf_number] <<- attr(dend_node, "nodePar")[["lab.col"]]
      if (!is.null(col) & labels) names(col)[i_leaf_number] <<- attr(dend_node, "label")
    }
    return(dend_node)
  }
  # mtrace(".change.label.by.mat")
  i_leaf_number <- 0
  # here I don't care about the classes of the branches of the dend.
  dendrapply(dend, get_col_from_leaf)
  return(col)
}


#' @export
#' @rdname labels_colors
labels_col <- labels_colors



#' @export
#' @rdname labels_colors
"labels_colors<-" <- function(dend, ..., value) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  if (missing(value)) {
    if (dendextend_options("warn")) warning("Color values are missing, using default (different) colors")
    tree_size <- nleaves(dend)
    value <- rainbow_fun(tree_size)
  }


  col <- value
  leaves_length <- length(order.dendrogram(dend)) # length(labels(dend)) # it will be faster to use order.dendrogram than labels...
  if (leaves_length > length(col)) {
    if (dendextend_options("warn")) warning("Length of color vector was shorter than the number of leaves - vector color recycled")
    col <- rep(col, length.out = leaves_length)
  }

  set.col.to.leaf <- function(dend_node) {
    if (is.leaf(dend_node)) {
      i_leaf_number <<- i_leaf_number + 1
      if (is.null(attr(dend_node, "nodePar"))) {
        attr(dend_node, "nodePar") <-
          list(
            lab.col = col[i_leaf_number],
            pch = NA
          )
      } else {
        #             attr(dend_node, "nodePar") <- within(attr(dend_node, "nodePar"), {lab.col <- col[i_leaf_number]}) # this way it doesn't erase other nodePar values (if they exist)
        # Using as.list in order to make the code more robust
        # attr(dend_node, "nodePar") <- within(as.list(attr(dend_node, "nodePar")), {lab.col <- col[i_leaf_number]}) # this way it doesn't erase other nodePar values (if they exist)
        attr(dend_node, "nodePar")[["lab.col"]] <- col[i_leaf_number] # this way it doesn't erase other nodePar values (if they exist)
      }

      if (length(attr(dend_node, "nodePar")) == 0) attr(dend_node, "nodePar") <- NULL # remove nodePar if it is empty
    }
    return(unclass(dend_node))
  }
  i_leaf_number <- 0
  new_dend <- dendrapply(dend, set.col.to.leaf)
  class(new_dend) <- "dendrogram"
  return(new_dend)
}


`labels_colors<-.dendrogram` <- `labels_colors<-`



#


#' @title Retrieve/assign cex to the labels of a dendrogram
#' @rdname labels_cex
#' @description Retrieve/assign cex to the labels of a dendrogram
#' @export
#' @param dend a dendrogram object
#' @param ... not used
#' @param value a vector of cex to be used as new label's size for the dendrogram
#' @return
#' A vector with the dendrogram's labels sizes (NULL if none are supplied).
#' @examples
#' # define dendrogram object to play with:
#' dend <- as.dendrogram(hclust(dist(USArrests[1:3, ]), "ave"))
#'
#' # Defaults:
#' labels_cex(dend)
#' plot(dend)
#'
#' # let's add some color:
#' labels_cex(dend) <- 1:3
#' labels_cex(dend)
#' plot(dend)
#'
#' labels_cex(dend) <- 1
#' labels_cex(dend)
#' plot(dend)
labels_cex <- function(dend, ...) {
  # get_leaves_attr(dend, attribute = "nodePar", simplify = FALSE)
  unlist(dendrapply(dend, function(node) {
    attr(node, "nodePar")$`lab.cex`
  }))
}

#' @export
#' @rdname labels_cex
"labels_cex<-" <- function(dend, ..., value) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")
  assign_values_to_leaves_nodePar(dend, value, "lab.cex", ...)
}

`labels_cex<-.dendrogram` <- `labels_cex<-`













#' Color unique labels in a dendrogram
#'
#' @param dend a dend object
#' @param ... NOT USED
#'
#' @return
#' A dendrogram after the colors of its labels have been updated (a different color for each unique label).
#' @export
#'
#' @examples
#'
#' x <- c(2011, 2011, 2012, 2012, 2015, 2015, 2015)
#' names(x) <- x
#' dend <- as.dendrogram(hclust(dist(x)))
#'
#' par(mfrow = c(1, 2))
#' plot(dend)
#' dend2 <- color_unique_labels(dend)
#' plot(dend2)
color_unique_labels <- function(dend, ...) {
  #    if(!require(dendextend)) install.packages("dendextend")
  #    if(!require(colorspace)) install.packages("colorspace")
  #    library("dendextend")
  # original request: http://stackoverflow.com/questions/33567508/how-to-color-the-same-labels-on-dendorgram-in-one-colour-in-r/

  n_unique_labels <- length(unique(labels(dend)))
  colors <- colorspace::rainbow_hcl(n_unique_labels)
  labels_number <- as.numeric(factor(labels(dend)))
  labels_colors(dend) <- colors[labels_number]
  dend
}








#
# #### Tests:
# # define dendrogram object to play with:
# hc <- hclust(dist(USArrests[1:3,]), "ave")
# dend <- as.dendrogram(hc)
#
# # Defaults:
# labels_colors(dend)
# plot(dend)
#
# # let's add some color:
# labels_colors(dend) <- 2:4
# plot(dend)
#

# dend_node <- structure(3L, members = 1L, height = 0, label = "123", leaf = TRUE, nodePar = structure(19, .Names = "pch"))
# as.list(attr(dend_node, "nodePar"))
# str(attr(dend_node, "nodePar"))
# within
# within(as.list(attr(dend_node, "nodePar")), {print(ls())})
# within(as.list(attr(dend_node, "nodePar")), {bbb = 4})
# within(attr(dend_node, "nodePar"), {lab.col <- col[i_leaf_number]}) # this way it doesn't erase other nodePar values (if they exist)
#
