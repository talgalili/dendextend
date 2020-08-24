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


# like is.infinite but acts differently for character
is.infinite2 <- function(x) {
  if (is.character(x)) {
    return(x == "Inf")
  }
  # else { return(
  is.infinite(x)
}






### # ' @aliases
### # ' set_leaves_attr
### # ' @usage
### # ' get_leaves_attr(dend, labels = TRUE,...)
### # '
### # ' set_leaves_attr(dend, ...) <- value

#' @title Get/set attributes of dendrogram's leaves
#' @description Get/set attributes of dendrogram's leaves
#' @export
#' @param dend a dendrogram object
#' @param attribute character scalar of the attribute (\code{attr})
#' we wish to get/set from the leaves
#' @param simplify logical. If TRUE (default), then the return vector is
#' after using \code{unlist} on it.
#' @param ... not used
#' @source Heavily inspired by the code in the
#' function \code{labels.dendrogram},
#' so credit should go to Martin Maechler.
#' @return
#' A vector (or a list) with the dendrogram's leaves attribute
#' @seealso \link{get_nodes_attr}, \link{nnodes},
#' \link{nleaves}, \link{assign_values_to_leaves_nodePar}
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' # get_leaves_attr(dend) # error :)
#' get_leaves_attr(dend, "label")
#' labels(dend, "label")
#' get_leaves_attr(dend, "height") # should be 0's
#' get_nodes_attr(dend, "height")
#'
#' get_leaves_attr(dend, "nodePar")
#'
#'
#' get_leaves_attr(dend, "leaf") # should be TRUE's
#' get_nodes_attr(dend, "leaf") # conatins NA's
#'
#'
#' get_leaves_attr(dend, "members") # should be 1's
#' get_nodes_attr(dend, "members") #
#'
#'
#' get_leaves_attr(dend, "members", simplify = FALSE) # should be 1's
get_leaves_attr <- function(dend, attribute, simplify = TRUE, ...) {
  if (!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")
  if (missing(attribute)) stop("'attribute' parameter is missing.")

  get_attr_from_leaf <- function(dend_node) {
    if (is.leaf(dend_node)) attr(dend_node, attribute)
  }

  ret <- dendrapply(dend, get_attr_from_leaf)
  if (simplify) ret <- unlist(ret)

  return(ret)
}









#' @title Get nodePar of dendrogram's leaves
#' @description Get the nodePar attributes of dendrogram's leaves (includes pch, color, and cex)
#' @export
#' @param dend a dendrogram object
#' @param simplify logical (default is FALSE). If TRUE, then the return vector is
#' after using \code{unlist} on it.
#' @param ... not used
#' @return
#' A list (or a vector) with the dendrogram's leaves nodePar attribute
#' @seealso \link{get_nodes_attr}, \link{assign_values_to_leaves_nodePar}, \link{labels_colors}
#' \link{get_leaves_edgePar}
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' # get_leaves_attr(dend) # error :)
#' get_leaves_nodePar(dend)
#' labels_colors(dend) <- 1:3
#' get_leaves_nodePar(dend)
#'
#' dend <- assign_values_to_leaves_nodePar(dend, 2, "lab.cex")
#' get_leaves_nodePar(dend)
#'
#' plot(dend)
get_leaves_nodePar <- function(dend, simplify = FALSE, ...) {
  if (!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")

  is_node_leaf <- get_nodes_attr(dend, "leaf")
  is_node_leaf[is.na(is_node_leaf)] <- FALSE

  ret <- get_nodes_attr(dend, "nodePar", simplify = FALSE)[is_node_leaf]
  if (simplify) ret <- unlist(ret)

  return(ret)
}








#' @title Get edgePar of dendrogram's leaves
#' @export
#' @description
#' This is helpful to get the attributes of branches of the leaves.
#' For example, after we use \link{color_branches}, to get the colors
#' of the labels to match (since getting the colors of branches to match
#' those of the labels can be tricky).
#' @param dend a dendrogram object
#' @param simplify logical (default is FALSE). If TRUE, then the return vector is
#' after using \code{unlist} on it.
#' @param ... not used
#' @return
#' A list (or a vector) with the dendrogram's leaves edgePar attribute
#' @seealso \link{get_nodes_attr}, \link{assign_values_to_leaves_nodePar}, \link{labels_colors}
#' \link{get_leaves_nodePar}
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' # get_leaves_edgePar(dend) # error :)
#' get_leaves_edgePar(dend)
#' dend <- color_branches(dend, k = 3)
#' get_leaves_edgePar(dend)
#' get_leaves_edgePar(dend, TRUE)
#'
#' dend <- dend %>% set("branches_lwd", c(2, 1, 2))
#' get_leaves_edgePar(dend)
#'
#' plot(dend)
get_leaves_edgePar <- function(dend, simplify = FALSE, ...) {
  if (!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")

  is_node_leaf <- get_nodes_attr(dend, "leaf")
  is_node_leaf[is.na(is_node_leaf)] <- FALSE

  ret <- get_nodes_attr(dend, "edgePar", simplify = FALSE)[is_node_leaf]
  if (simplify) ret <- unlist(ret)

  return(ret)
}






#' @title Get an attribute of the branches of a dendrogram's leaves
#' @export
#' @description
#' This is helpful to get the attributes of branches of the leaves.
#' For example, after we use \link{color_branches}, to get the colors
#' of the labels to match (since getting the colors of branches to match
#' those of the labels can be tricky).
#' This is based on \link{get_leaves_edgePar}.
#'
#' @param dend a dendrogram object
#' @param attr character, the attr to get. Can be either "col", "lwd", or "lty".
#' @param ... not used
#' @return
#' A vector with the dendrogram's leaves nodePar attribute
#' @seealso \link{get_nodes_attr}, \link{assign_values_to_leaves_nodePar}, \link{labels_colors}
#' \link{get_leaves_nodePar}, \link{get_leaves_edgePar}
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' dend <- dend %>%
#'   color_branches(k = 3) %>%
#'   set("branches_lwd", c(2, 1, 2)) %>%
#'   set("branches_lty", c(1, 2, 1))
#'
#' plot(dend)
#'
#' get_leaves_branches_attr(dend, "col")
#' get_leaves_branches_attr(dend, "lwd")
#' get_leaves_branches_attr(dend, "lty")
#'
#' labels_colors(dend) <- get_leaves_branches_attr(dend, "col")
#' plot(dend)
get_leaves_branches_attr <- function(dend, attr = c("col", "lwd", "lty"), ...) {
  if (!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")
  attr <- match.arg(attr)

  dend_leaves_edgePar <- get_leaves_edgePar(dend)

  get_attr <- function(element) element[attr]

  unname(unlist(lapply(dend_leaves_edgePar, get_attr)))
}





#' @title Get the colors of the branches of a dendrogram's leaves
#' @export
#' @description
#' It is useful to get the colors of branches of the leaves,
#' after we use \link{color_branches}, so to then match the colors of the labels
#' to that of the branches (since getting the colors of branches to match
#' those of the labels can be tricky).
#' This is based on \link{get_leaves_branches_attr} which is based on
#' \link{get_leaves_edgePar}.
#'
#' TODO: The function get_leaves_branches_col may behave oddly when extracting
#' colors with missing col attributes when the lwd attribute is available.
#' This may resolt in a vector with the wrong length (with omitted NA values).
#' This might need to be fixed in the future, and attention should be given to this case.
#'
#' @param dend a dendrogram object
#' @param ... not used
#' @return
#' A vector with the dendrogram's leaves' branches' colors
#' @seealso \link{get_nodes_attr}, \link{assign_values_to_leaves_nodePar}, \link{labels_colors}
#' \link{get_leaves_nodePar}, \link{get_leaves_edgePar}, \link{get_leaves_branches_attr}
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' par(mfrow = c(1, 2), mar = c(5, 2, 1, 0))
#' dend <- dend %>%
#'   color_branches(k = 3) %>%
#'   set("branches_lwd", c(2, 1, 2)) %>%
#'   set("branches_lty", c(1, 2, 1))
#'
#' plot(dend)
#'
#' labels_colors(dend) <- get_leaves_branches_col(dend)
#' plot(dend)
get_leaves_branches_col <- function(dend, ...) {
  get_leaves_branches_attr(dend, attr = "col", ...)
}



#' @title Get attributes of dendrogram's nodes
#' @export
#' @description
#' Allows easy access to attributes of branches and/or leaves, with option
#' of returning a vector with/withough NA's (for marking the missing attr value)
#'
#' @param dend a dendrogram object
#' @param attribute character scalar of the attribute (\code{attr})
#' we wish to get from the nodes
#' @param id integer vector. If given - only the attr of these nodes id will be returned (via depth first search)
#' @param include_leaves logical. Should leaves attributes be included as well?
#' @param include_branches logical. Should non-leaf (branch node)
#' attributes be included as well?
#' @param simplify logical (default is TRUE). should the result be simplified
#' to a vector (using \link{simplify2array} ) if possible? If it is not possible
#' it will return a matrix. When FALSE, a list is returned.
#' @param na.rm logical. Should NA attributes be REMOVED from the resulting vector?
#' @param ... not used
#' @source Heavily inspired by the code in the
#' function \code{labels.dendrogram},
#' so credit should go to Martin Maechler.
#' @return
#' A vector with the dendrogram's nodes attribute. If an attribute is missing
#' from some nodes, it will return NA in that vector.
#' @seealso \link{get_leaves_attr}, \link{nnodes}, \link{nleaves}
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' # get_leaves_attr(dend) # error :)
#' get_leaves_attr(dend, "label")
#' labels(dend, "label")
#' get_leaves_attr(dend, "height") # should be 0's
#' get_nodes_attr(dend, "height")
#'
#'
#' get_leaves_attr(dend, "leaf") # should be TRUE's
#' get_nodes_attr(dend, "leaf") # conatins NA's
#'
#'
#' get_leaves_attr(dend, "members") # should be 1's
#' get_nodes_attr(dend, "members", include_branches = FALSE, na.rm = TRUE) #
#' get_nodes_attr(dend, "members") #
#' get_nodes_attr(dend, "members", simplify = FALSE)
#' get_nodes_attr(dend, "members", include_leaves = FALSE, na.rm = TRUE) #
#'
#' get_nodes_attr(dend, "members", id = c(1, 3), simplify = FALSE)
#' get_nodes_attr(dend, "members", id = c(1, 3)) #
#'
#'
#' hang_dend <- hang.dendrogram(dend)
#' get_leaves_attr(hang_dend, "height") # no longer 0!
#' get_nodes_attr(hang_dend, "height") # does not include any 0s!
#'
#' # does not include leaves values:
#' get_nodes_attr(hang_dend, "height", include_leaves = FALSE)
#' # remove leaves values all together:
#' get_nodes_attr(hang_dend, "height", include_leaves = FALSE, na.rm = TRUE)
#' \dontrun{
#' library(microbenchmark)
#' # get_leaves_attr is twice faster than get_nodes_attr
#' microbenchmark(
#'   get_leaves_attr(dend, "members"), # should be 1's
#'   get_nodes_attr(dend, "members", include_branches = FALSE, na.rm = TRUE)
#' )
#' }
#'
get_nodes_attr <- function(dend, attribute,
                           id,
                           include_leaves = TRUE,
                           include_branches = TRUE,
                           simplify = TRUE,
                           na.rm = FALSE, ...) {
  if (!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")
  if (missing(attribute)) stop("'attribute' parameter is missing.")

  #### for some reason, this doesn't work:
  #    get_attr_from_node <- function(dend_node) {
  #       i_attr <- attr(dend_node, attribute)
  #       ifelse(is.null(i_attr), NA, i_attr)
  #    }
  #
  #    return((dendrapply(dend, get_attr_from_node)))


  #    dend_attr <- rep(NA, nnodes(dend))
  #   empty_list <- vector("list", nnodes(dend))
  empty_list <- as.list(rep(NA, nnodes(dend)))
  dend_attr <- empty_list
  missing_id <- missing(id)


  # this function is used to modify dend_attr. What it returns is not important.
  i_node <- 0
  get_attr_from_node <- function(dend_node) {
    i_node <<- i_node + 1

    # if we have id's and this is not it - we can skip it...
    # FALSE & NULL # fails
    # FALSE && NULL # works...
    if (!missing_id && !(i_node %in% id)) {
      return(invisible())
    }


    # if we should not include_leaves, then we skip when a leaf is encountered.
    if (!include_leaves && is.leaf(dend_node)) {
      return(NULL)
    }
    if (!include_branches && !is.leaf(dend_node)) {
      return(NULL)
    }

    i_attr <- attr(dend_node, attribute)
    if (!is.null(i_attr)) dend_attr[[i_node]] <<- i_attr
    return(invisible())
  }
  dendrapply(dend, get_attr_from_node)

  # as.vector is to remove all classes of the na.omit
  # thank you Prof. Brian Ripley http://tolstoy.newcastle.edu.au/R/e2/devel/07/01/1965.html
  if (simplify) dend_attr <- simplify2array(dend_attr)

  if (na.rm) dend_attr <- as.vector(na.omit(dend_attr))


  if (dendextend_options("warn") && identical(dend_attr, simplify2array(empty_list))) warning("It seems that the attribute '", attribute, "' does not exist - returning NA.")

  # TODO: this could probably be more optimized - say, by looking only at the above mentioned id's
  # and not create all of the vector and only then take a subset.
  # But for now, I think this is more maintainable...
  if (!missing_id) {
    dend_attr <- dend_attr[id]
  }

  return(dend_attr)
}











#   ' @export
#' @title recursivly apply a function on a list
#' @description
#' recursivly apply a function on a list - and returns the output as a list,
#' following the naming convention in the {plyr} package
#' the big difference between this and rapply is that this will also apply
#' the function on EACH element of the list, even if it's not a "terminal node"
#' inside the list tree.
#' An attribute is added to indicate if the value returned is
#' from a branch or a leaf.
#' @param x a list.
#' @param FUN a function to apply on each element of the list
#' @param add_notation logical. Should each node be
#' added a "position_type" attribute, stating if it is a "Branch" or a "Leaf".
#' @param ... not used.
#' @return a list with ALL of the nodes (from the original "x" list),
#' that FUN was applied on.
#'
#' @examples
#' \dontrun{
#' x <- list(1)
#' x
#' rllply(x, function(x) {
#'   x
#' }, add_notation = TRUE)
#'
#' x <- list(1, 2, list(31))
#' x
#' rllply(x, function(x) {
#'   x
#' }, add_notation = TRUE)
#' # the first element is the entire tree
#' # after FUN was applied to its root element.
#'
#' hc <- hclust(dist(USArrests[1:4, ]), "ave")
#' dend <- as.dendrogram(hc)
#' rllply(dend, function(x) {
#'   attr(x, "height")
#' })
#' rllply(dend, function(x) {
#'   attr(x, "members")
#' })
#' }
rllply <- function(x, FUN, add_notation = FALSE, ...) {
  if (is.list(x)) {
    output <- list()
    for (i in seq_len(length(x)))
    {
      output[[i]] <- list(rllply(x[[i]], FUN, ...))
      if (add_notation) attr(output[[i]][[1]], "position_type") <- "Branch"
    }
    output <- list(FUN(x, ...), output)
  } else {
    output <- FUN(x, ...)
    if (add_notation) attr(output, "position_type") <- "Leaf"
  }
  return(output)
}




# ' @aliases
# ' old_get_branches_heights


#' @title Get height attributes from a dendrogram
#' @description Get height attributes of a dendrogram's branches
#' @export
#' @param dend a dendrogram.
#' @param sort logical. Should the heights be sorted?
#' @param decreasing logical. Should the sort be increasing or decreasing? Not available for partial sorting.
#' @param include_leaves logical (FALSE). Should the output include the leaves value (0's).
#' @param ... not used.
#' @return
#' a vector of the dendrogram's nodes heights (excluding leaves).
#'
#' @examples
#'
#' hc <- hclust(dist(USArrests[1:4, ]), "ave")
#' dend <- as.dendrogram(hc)
#' get_branches_heights(dend)
get_branches_heights <- function(dend, sort = TRUE, decreasing = FALSE, include_leaves = FALSE, ...) {
  #    height <- unlist(rllply(dend, function(x){attr(x, "height")}))
  #    height <- get_nodes_attr(dend, "height")
  #    height <- height[height != 0] # include only the non zero values

  height <- get_nodes_attr(dend, "height", include_leaves = include_leaves, na.rm = TRUE)
  if (sort) height <- sort(height, decreasing = decreasing) # sort the height

  return(height)
}












#' @title Hang dendrogram leaves
#' @export
#' @description
#' Adjust the height attr in all of the dendrogram leaves so that
#'  the tree will hang. This is similar to as.dendrogram(hclust, hang=0.1)
#'  Only that it now works on other object than hclust turned into a dendrogram.
#'  For example, this allows us to hang non-binary trees.
#'
#' @param dend a dendrogram object
#' @param hang The fraction of the plot height by which labels should hang below
#' the rest of the plot. A negative value will cause the labels to
#' hang down from 0.
#' @param hang_height is missing, then using "hang". If a number is given,
#' it overrides "hang" (except if "hang" is negative)
#' @param ... not used
#' @return
#' A dendrogram, after adjusting the height attr in all of its leaves,
#' so that the tree will hang.
#' @source
#' Noticing that as.dendrogram has a "hang" parameter was thanks to Enrique Ramos's answer here::
#' \url{https://stackoverflow.com/questions/17088136/plot-horizontal-dendrogram-with-hanging-leaves-r}
#' @examples
#'
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' par(mfrow = c(1, 2))
#' plot(hang.dendrogram(dend))
#' plot(hc)
#' # identical(as.dendrogram(hc, hang = 0.1), hang.dendrogram(dend, hang = 0.1))
#' # TRUE!!
#'
#'
#' par(mfrow = c(1, 4))
#'
#' plot(dend)
#' plot(hang.dendrogram(dend, hang = 0.1))
#' plot(hang.dendrogram(dend, hang = 0))
#' plot(hang.dendrogram(dend, hang = -0.1))
#'
#' par(mfrow = c(1, 1))
#' plot(hang.dendrogram(dend), horiz = TRUE)
hang.dendrogram <- function(dend, hang = 0.1, hang_height, ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")


  #    get_heights.dendrogram
  if (missing(hang_height)) hang_height <- attr(dend, "height") * hang

  fix_height_attr_per_leaf <- function(dend_node) {
    if (!is.leaf(dend_node)) {
      dend_node_height <- attr(dend_node, "height")

      for (i_nodes in seq_len(length(dend_node))) {
        if (is.leaf(dend_node[[i_nodes]])) {
          if (hang < 0) {
            attr(dend_node[[i_nodes]], "height") <- 0
          } else {
            attr(dend_node[[i_nodes]], "height") <- dend_node_height - hang_height
          }

          dend_node[[i_nodes]] <- unclass(dend_node[[i_nodes]]) # makes sure we don't inherent any classes...
        } else {
          dend_node[[i_nodes]] <-
            fix_height_attr_per_leaf(dend_node[[i_nodes]])
        }
      }
    }
    return(unclass(dend_node))
  }

  dend <- fix_height_attr_per_leaf(dend)
  class(dend) <- "dendrogram"
  return(dend)
}





#' @title Get height attributes from a dendrogram's children
#' @description Get height attributes from a dendrogram's children nodes
#' @export
#' @param dend a dendrogram.
#' @param ... not used.
#' @return
#' a vector of the heights of a dendrogram's current node's
#' (first level) children.
#'
#' @seealso
#' \link{get_branches_heights}
#' @examples
#'
#' hc <- hclust(dist(USArrests[1:4, ]), "ave")
#' dend <- as.dendrogram(hc)
#' get_childrens_heights(dend)
get_childrens_heights <- function(dend, ...) {
  sapply(dend, function(x) {
    attr(x, "height")
  })
}



#' @title Rank branches' heights
#' @export
#' @description
#' Adjust the height attr in all of the dendrogram nodes so that
#'  the tree will have a distance of 1 unit between each parent/child nodes.
#'  It can be thought of as ranking the branches between themselves.
#'
#'  This is intended for easier comparison of the topology of two trees.
#'
#'  Notice that this function changes the height of all the leaves into 0,
#'  thus erasing the effect of \link{hang.dendrogram} (which should be run
#'  again, if that is the visualization you are intereted in).
#'
#' @param dend a dendrogram object
#' @param diff_height Numeric scalar (1). Affects the difference in height
#' between two branches.
#' @param ... not used
#' @seealso
#' \link{get_branches_heights}, \link{get_childrens_heights},
#' \link{hang.dendrogram}, \link{tanglegram}
#'
#' @return
#' A dendrogram, after adjusting the height attr in all of its branches.
#'
#' @examples
#'
#' # define dendrogram object to play with:
#' dend <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#'
#' par(mfrow = c(1, 3))
#'
#' plot(dend)
#' plot(rank_branches(dend))
#' plot(hang.dendrogram(rank_branches(dend)))
rank_branches <- function(dend, diff_height = 1, ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")


  fix_height_diff_per_branch <- function(dend_node) {
    if (is.leaf(dend_node)) {
      attr(dend_node, "height") <- 0
      return(unclass(dend_node))
    }
    # else:

    # go through and fix all of the nodes beneath this one:
    for (i_nodes in seq_len(length(dend_node))) {
      dend_node[[i_nodes]] <- Recall(dend_node[[i_nodes]])
    }

    # now we can fix the current height, by finding the height of
    # all son-branches and increasing the height of the maximum of them.
    dend_children_heights <- get_childrens_heights(dend_node)
    attr(dend_node, "height") <- max(dend_children_heights) + diff_height


    return(unclass(dend_node))
  }

  dend <- fix_height_diff_per_branch(dend)
  class(dend) <- "dendrogram"
  return(dend)
}
















#' @title Assign values to nodePar of dendrogram's leaves
#' @export
#' @description
#' Go through the dendrogram leaves and updates the values inside its nodePar
#'
#' If the value has Inf then the value in edgePar will not be changed.
#' @param dend a dendrogram object
#' @param value a new value vector for the nodePar attribute. It should be
#' the same length as the number of leaves in the tree. If not, it will recycle
#' the value and issue a warning.
#' @param nodePar the value inside nodePar to adjust.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... not used
#' @return
#' A dendrogram, after adjusting the nodePar attribute in all of its leaves,
#' @seealso \link{get_leaves_attr}
#' @examples
#'
#' \dontrun{
#'
#' dend <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust("ave") %>%
#'   as.dendrogram()
#'
#' # reproduces "labels_colors<-"
#' # although it does force us to run through the tree twice,
#' # hence "labels_colors<-" is better...
#' plot(dend)
#' dend <- assign_values_to_leaves_nodePar(dend = dend, value = c(3, 2), nodePar = "lab.col")
#' plot(dend)
#'
#' dend <- assign_values_to_leaves_nodePar(dend, 1, "pch")
#' plot(dend)
#' # fix the annoying pch=1:
#' dend <- assign_values_to_leaves_nodePar(dend, NA, "pch")
#' plot(dend)
#' # adjust the cex:
#' dend <- assign_values_to_leaves_nodePar(dend, 19, "pch")
#' dend <- assign_values_to_leaves_nodePar(dend, 2, "lab.cex")
#' plot(dend)
#'
#' str(unclass(dend))
#'
#' get_leaves_attr(dend, "nodePar", simplify = FALSE)
#' }
#'
assign_values_to_leaves_nodePar <- function(dend, value, nodePar, warn = dendextend_options("warn"), ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  if (missing(value)) {
    if (warn) warning("value is missing, returning the dendrogram as is.")
    return(dend)
  }

  leaves_length <- nleaves(dend) # length(labels(dend)) # it will be faster to use order.dendrogram than labels...
  if (leaves_length > length(value)) {
    if (warn) warning("Length of value vector was shorter than the number of leaves - vector value recycled")
    value <- rep(value, length.out = leaves_length)
  }

  set_value_to_leaf <- function(dend_node) {
    if (is.leaf(dend_node)) {
      i_leaf_number <<- i_leaf_number + 1

      to_update_attr <- !is.infinite2(value[i_leaf_number])
      if (to_update_attr) {
        # if(!is.infinite2(value[i_leaf_number])) {
        attr(dend_node, "nodePar")[nodePar] <- list(value[i_leaf_number]) # this way it doesn't erase other nodePar values (if they exist)
      }


      if (length(attr(dend_node, "nodePar")) == 0) {
        attr(dend_node, "nodePar") <- NULL # remove nodePar if it is empty
      } else {
        # if we have some nodePar, and we don't have pch - let's make
        # sure it is NA - so that we don't see that annoying dot.
        if (!("pch" %in% names(attr(dend_node, "nodePar")))) {
          attr(dend_node, "nodePar")["pch"] <- list(NA)
        }
      }
    }
    return(unclass(dend_node))
  }
  i_leaf_number <- 0
  new_dend <- dendrapply(dend, set_value_to_leaf)
  class(new_dend) <- "dendrogram"
  return(new_dend)
}


#
#  tanglegram(dend1 , dend2, lab.cex = 2, edge.lwd = 3,margin_inner= 5, type = "t", center = TRUE)
# attr(dend[[1]], "nodePar")[["pch"]]















#' @title Assign values to edgePar of dendrogram's leaves
#' @export
#' @description
#' Go through the dendrogram leaves and updates the values inside its edgePar
#'
#' If the value has Inf then the value in edgePar will not be changed.
#' @param dend a dendrogram object
#' @param value a new value vector for the edgePar attribute. It should be
#' the same length as the number of leaves in the tree. If not, it will recycle
#' the value and issue a warning.
#' @param edgePar the value inside edgePar to adjust.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... not used
#' @return
#' A dendrogram, after adjusting the edgePar attribute in all of its leaves,
#' @seealso \link{get_leaves_attr}, link{assign_values_to_leaves_nodePar}
#' @examples
#'
#' \dontrun{
#'
#' dend <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust("ave") %>%
#'   as.dendrogram()
#'
#' plot(dend)
#' dend <- assign_values_to_leaves_edgePar(dend = dend, value = c(3, 2), edgePar = "col")
#' plot(dend)
#' dend <- assign_values_to_leaves_edgePar(dend = dend, value = c(3, 2), edgePar = "lwd")
#' plot(dend)
#' dend <- assign_values_to_leaves_edgePar(dend = dend, value = c(3, 2), edgePar = "lty")
#' plot(dend)
#'
#' get_leaves_attr(dend, "edgePar", simplify = FALSE)
#' }
#'
assign_values_to_leaves_edgePar <- function(dend, value, edgePar, warn = dendextend_options("warn"), ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  if (missing(value)) {
    if (warn) warning("value is missing, returning the dendrogram as is.")
    return(dend)
  }

  leaves_length <- nleaves(dend) # length(labels(dend)) # it will be faster to use order.dendrogram than labels...
  if (leaves_length > length(value)) {
    if (warn) warning("Length of value vector was shorter than the number of leaves - vector value recycled")
    value <- rep(value, length.out = leaves_length)
  }

  set_value_to_leaf <- function(dend_node) {
    if (is.leaf(dend_node)) {
      i_leaf_number <<- i_leaf_number + 1

      to_update_attr <- !is.infinite2(value[i_leaf_number])
      if (to_update_attr) {
        # if(!is.infinite2(value[i_leaf_number])) {
        attr(dend_node, "edgePar")[edgePar] <- list(value[i_leaf_number]) # this way it doesn't erase other edgePar values (if they exist)
      }


      if (length(attr(dend_node, "edgePar")) == 0) {
        attr(dend_node, "edgePar") <- NULL # remove edgePar if it is empty
      } else {
        # if we have some edgePar, and we don't have pch - let's make
        # sure it is NA - so that we don't see that annoying dot.
        if (!("pch" %in% names(attr(dend_node, "edgePar")))) {
          attr(dend_node, "edgePar")["pch"] <- list(NA)
        }
      }
    }
    return(unclass(dend_node))
  }
  i_leaf_number <- 0
  new_dend <- dendrapply(dend, set_value_to_leaf)
  class(new_dend) <- "dendrogram"
  return(new_dend)
}








#' @title Assign values to nodePar of dendrogram's nodes
#' @export
#' @description
#' Go through the dendrogram nodes and updates the values inside its nodePar
#'
#' If the value has Inf then the value in edgePar will not be changed.
#'
#' @param dend a dendrogram object
#' @param value a new value vector for the nodePar attribute. It should be
#' the same length as the number of nodes in the tree. If not, it will recycle
#' the value and issue a warning.
#' @param nodePar the value inside nodePar to adjust.
#' This may contain components named pch, cex, col, xpd, and/or bg.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... not used
#' @return
#' A dendrogram, after adjusting the nodePar attribute in all of its nodes,
#' @seealso \link{get_leaves_attr}, \link{assign_values_to_leaves_nodePar}
#' @examples
#'
#' \dontrun{
#'
#' dend <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust("ave") %>%
#'   as.dendrogram()
#'
#' # reproduces "labels_colors<-"
#' # although it does force us to run through the tree twice,
#' # hence "labels_colors<-" is better...
#' plot(dend)
#' dend2 <- dend %>%
#'   assign_values_to_nodes_nodePar(value = 19, nodePar = "pch") %>%
#'   assign_values_to_nodes_nodePar(value = c(1, 2), nodePar = "cex") %>%
#'   assign_values_to_nodes_nodePar(value = c(2, 1), nodePar = "col")
#' plot(dend2)
#'
#'
#' ### Making sure this works for NA with character.
#' dend %>%
#'   assign_values_to_nodes_nodePar(value = 19, nodePar = "pch") %>%
#'   assign_values_to_nodes_nodePar(value = c("red", NA), nodePar = "col") -> dend2
#' plot(dend2)
#' }
#'
assign_values_to_nodes_nodePar <- function(dend, value, nodePar = c("pch", "cex", "col", "xpd", "bg"), warn = dendextend_options("warn"), ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  if (missing(value)) {
    warning("value is missing, returning the dendrogram as is.")
    return(dend)
  }

  nodePar <- match.arg(nodePar)


  nodes_length <- nnodes(dend) # length(labels(dend)) # it will be faster to use order.dendrogram than labels...
  if (nodes_length > length(value)) {
    if (warn) warning("Length of value vector was shorter than the number of nodes - vector value recycled")
    value <- rep(value, length.out = nodes_length)
  }

  set_value_to_node <- function(dend_node) {
    i_node_number <<- i_node_number + 1

    to_update_attr <- !is.infinite2(value[i_node_number]) | is.na(value[i_node_number])
    if (to_update_attr) {
      # if(!is.infinite2(value[i_node_number])) {
      attr(dend_node, "nodePar")[nodePar] <- list(value[i_node_number]) # this way it doesn't erase other nodePar values (if they exist)
    }

    if (length(attr(dend_node, "nodePar")) == 0) {
      attr(dend_node, "nodePar") <- NULL # remove nodePar if it is empty
    } else {
      # if we have some nodePar, and we don't have pch - let's make
      # sure it is NA - so that we don't see that annoying dot.
      if (!("pch" %in% names(attr(dend_node, "nodePar")))) {
        attr(dend_node, "nodePar")["pch"] <- list(NA)
      }
    }
    return(unclass(dend_node))
  }
  i_node_number <- 0
  new_dend <- dendrapply(dend, set_value_to_node)
  class(new_dend) <- "dendrogram"
  return(new_dend)
}

















#' @title Assign values to edgePar of dendrogram's branches
#' @export
#' @description
#' Go through the dendrogram branches and updates the values inside its edgePar
#'
#' If the value has Inf then the value in edgePar will not be changed.
#'
#' @param dend a dendrogram object
#' @param value a new value scalar for the edgePar attribute.
#' @param edgePar a character indicating the value inside edgePar to adjust.
#' Can be either "col", "lty", or "lwd".
#' @param skip_leaves logical (FALSE) - should the leaves be skipped/ignored?
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... not used
#' @return
#' A dendrogram, after adjusting the edgePar attribute in all of its branches,
#' @seealso \link{get_root_branches_attr}
#' @examples
#'
#' # This failed before - now it works fine. (thanks to Martin Maechler)
#' dend <- 1:2 %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend %>%
#'   set("branches_lty", 1:2) %>%
#'   set("branches_col", c("topbranch_never_plots", "black", "orange")) %>%
#'   plot()
#' \dontrun{
#'
#' dend <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' plot(dend)
#' dend <- assign_values_to_branches_edgePar(dend = dend, value = 2, edgePar = "lwd")
#' plot(dend)
#' dend <- assign_values_to_branches_edgePar(dend = dend, value = 2, edgePar = "col")
#' plot(dend)
#' dend <- assign_values_to_branches_edgePar(dend = dend, value = "orange", edgePar = "col")
#' plot(dend)
#' dend2 <- assign_values_to_branches_edgePar(dend = dend, value = 2, edgePar = "lty")
#' plot(dend2)
#'
#' dend2 %>%
#'   unclass() %>%
#'   str()
#' }
#'
assign_values_to_branches_edgePar <- function(dend, value, edgePar, skip_leaves = FALSE, warn = dendextend_options("warn"), ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  if (missing(value)) {
    warning("value is missing, returning the dendrogram as is.")
    return(dend)
  }

  # if we are skipping leaves, than the number of branches should not include the terminal nodes/leaves!
  n_branches <- nnodes(dend) - ifelse(skip_leaves, nleaves(dend), 0) # length(labels(dend)) # it will be faster to use order.dendrogram than labels...
  if (n_branches > length(value)) {
    if (warn) warning("Length of value vector was shorter than the number of leaves - vector value recycled")
    value <- rep(value, length.out = n_branches)
  }

  set_value_to_branch <- function(dend_node) {
    # if we ignore leaves, jump the function for leaves:
    if (skip_leaves & is.leaf(dend_node)) {
      return(unclass(dend_node))
    }
    # else - keep as usual.

    i_node <<- i_node + 1
    to_update_attr <- !is.infinite2(value[i_node])
    if (to_update_attr) {
      attr(dend_node, "edgePar")[edgePar] <- list(value[i_node]) # [i_leaf_number] # this way it doesn't erase other edgePar values (if they exist)
    }
    if (length(attr(dend_node, "edgePar")) == 0) attr(dend_node, "edgePar") <- NULL # remove edgePar if it is empty
    return(unclass(dend_node))
  }

  i_node <- 0
  new_dend <- dendrapply(dend, set_value_to_branch)
  class(new_dend) <- "dendrogram"
  return(new_dend)
}






#' @title Remove all edgePar values from a dendrogram's branches
#' @export
#' @description
#' Go through the dendrogram branches and remove its edgePar.
#' @param dend a dendrogram object
#' @param ... not used
#' @return
#' A dendrogram, after removing the edgePar attribute in all of its branches,
#' @seealso \link{get_root_branches_attr}, \link{assign_values_to_branches_edgePar}
#' @examples
#'
#' \dontrun{
#'
#' dend <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend <- color_branches(dend, 3)
#' par(mfrow = c(1, 2))
#' plot(dend)
#' plot(remove_branches_edgePar(dend))
#' }
#'
remove_branches_edgePar <- function(dend, ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  remove_edgePar_from_branch <- function(dend_node) {
    attr(dend_node, "edgePar") <- NULL # remove edgePar
    return(unclass(dend_node))
  }
  new_dend <- dendrapply(dend, remove_edgePar_from_branch)
  class(new_dend) <- "dendrogram"
  return(new_dend)
}




#' @title Remove all nodePar values from a dendrogram's nodes
#' @export
#' @description
#' Go through the dendrogram nodes and remove its nodePar
#' @param dend a dendrogram object
#' @param ... not used
#' @return
#' A dendrogram, after removing the nodePar attribute in all of its nodes,
#' @seealso \link{get_root_branches_attr}, \link{assign_values_to_branches_edgePar}
#' @examples
#'
#' \dontrun{
#'
#' dend <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend <- color_branches(dend, 3)
#' par(mfrow = c(1, 2))
#' plot(dend)
#' plot(remove_branches_edgePar(dend))
#' }
#'
remove_nodes_nodePar <- function(dend, ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  remove_nodePar_from_node <- function(dend_node) {
    attr(dend_node, "nodePar") <- NULL # remove nodePar
    return(unclass(dend_node))
  }
  new_dend <- dendrapply(dend, remove_nodePar_from_node)
  class(new_dend) <- "dendrogram"
  return(new_dend)
}




#' @title Remove all nodePar values from a dendrogram's leaves
#' @export
#' @description
#' Go through the dendrogram leaves and remove its nodePar.
#' @param dend a dendrogram object
#' @param ... not used
#' @return
#' A dendrogram, after removing the nodePar attribute in all of its leaves,
#' @seealso \link{get_leaves_attr}, \link{assign_values_to_leaves_nodePar}
#' @examples
#'
#' \dontrun{
#'
#' dend <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#'
#' dend <- color_labels(dend, 3)
#' par(mfrow = c(1, 2))
#' plot(dend)
#' plot(remove_leaves_nodePar(dend))
#'
#'
#' get_leaves_attr(dend, "nodePar")
#' get_leaves_attr(remove_leaves_nodePar(dend), "nodePar")
#' }
#'
remove_leaves_nodePar <- function(dend, ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  remove_nodePar_from_leaf <- function(dend_node) {
    if (is.leaf(dend_node)) {
      attr(dend_node, "nodePar") <- NULL # remove nodePar if it is empty
    }
    return(unclass(dend_node))
  }
  new_dend <- dendrapply(dend, remove_nodePar_from_leaf)
  class(new_dend) <- "dendrogram"
  return(new_dend)
}












# nleaves(dend)
# nleaves(dend[[1]])
# nleaves(dend[[2]])


#' @title Fix members attr in a dendrogram
#' @export
#' @description
#' Fix members attr in a dendrogram after (for example), the tree was pruned
#' or manipulated.
#' @param dend a dendrogram object
#' @param ... not used
#' @return
#' A dendrogram, after adjusting the members attr in all of its nodes.
#' @examples
#'
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3, ]), "ave")
#' dend <- as.dendrogram(hc)
#' # plot(dend)
#' # prune one leaf
#' dend[[2]] <- dend[[2]][[1]]
#' # plot(dend)
#' dend # but it is NO LONGER true that it has 3 members total!
#' fix_members_attr.dendrogram(dend) # it now knows it has only 2 members :)
#'
#' hc <- hclust(dist(USArrests[1:3, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' identical(prune_leaf(dend, "Alaska"), fix_members_attr.dendrogram(prune_leaf(dend, "Alaska")))
#' str(unclass(prune_leaf(dend, "Alaska")))
#' str(unclass(fix_members_attr.dendrogram(prune_leaf(dend, "Alaska"))))
fix_members_attr.dendrogram <- function(dend, ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")


  fix_members_attr_per_node <- function(dend_node) {
    if (!is.leaf(dend_node)) attr(dend_node, "members") <- nleaves(dend_node, method = "order")
    return(unclass(dend_node))
  }
  # mtrace(".change.label.by.mat")
  dend <- dendrapply(dend, fix_members_attr_per_node)
  class(dend) <- "dendrogram"
  return(dend)
}
#



#' @title Fix rank of leaves order values in a dendrogram
#' @export
#' @description
#' Generally, leaves order value should be a sequence of integer values.
#' From 1 to nleaves(dend).
#' This function fixes trees by using \link{rank} on existing leaves order
#' values.
#' @param dend a dendrogram object
#' @param ... not used
#' @return
#' A dendrogram, after fixing its leaves order values.
#' @seealso
#' \link{prune}
#' @examples
#'
#' # define dendrogram object to play with:
#' dend <- USArrests[1:4, ] %>%
#'   dist() %>%
#'   hclust(method = "ave") %>%
#'   as.dendrogram()
#' # plot(dend)
#' order.dendrogram(dend)
#' dend2 <- prune(dend, "Alaska")
#' order.dendrogram(dend2)
#' order.dendrogram(rank_order.dendrogram(dend2))
rank_order.dendrogram <- function(dend, ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")

  order.dendrogram(dend) <- rank(order.dendrogram(dend), ties.method = "first")

  return(dend)
}
#
