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



#' @title Rotate a tree object
#' @export
#' @rdname rotate
#' @description
#' Rotates, rev and sort the branches of a tree object (dendrogram, hclust)
#' based on a vector - eithor of labels order (numbers) or the labels in their
#' new order (character).
#'
#' @param x a tree object (either a \code{dendrogram} or \code{hclust})
#' @param order Either numeric or character vector.
#' Is numeric: it is a numeric vector with the order of the value to be
#' assigned to object's label. The numbers say are just like when you use \link{order}:
#' which of the items on the tree-plot should be "first" (e.g: most left),
#' second etc. (this is relevant only to \code{rotate})
#' Is character: it must be a vector with the content of labels(x), in the
#' order we'd like to have the new tree.
#' @param decreasing logical. Should the sort be increasing or decreasing? Not available for partial sorting. (relevant only to \code{sort})
#' @param ... parameters passed (for example, in case of sort)
#' @param type a character indicating how to sort. If "labels" then by lexicographic
#' order of the labels. If "nodes", then by using \link{ladderize} (order so that
#' recursively, the leftmost branch will be the smallest)
#' @param phy a placeholder in case the user uses "phy ="
#' @details
#' The motivation for this function came from the function
#' \code{\link{order.dendrogram}} NOT being very intuitive.
#' What \code{rotate} aims to do is give a simple tree rotation function which
#' is based on the order which the user would like to see the tree rotated by
#' (just as \code{\link{order}} works for numeric vectors).
#'
#' \code{\link{rev.dendrogram}} is part of base R, and returns the tree object
#' after rotating it so that the order of the labels is reversed.
#' Here we added an S3 method for hclust objects.
#'
#' The \code{sort} methods sort the labels of the tree (using \code{order})
#' and then attempts to rotate the tree to fit that order.
#'
#' The hclust method of "\code{rotate}" works by first changing the object into
#' dendrogram, performing the rotation, and then changing it back to hclust.
#' Special care is taken in preserving some of the properties of the hclust
#' object.
#'
#' The {ape} package has its own \code{\link[ape]{rotate}}({ape}) function
#' (Which is sadly not S3, so cannot be easily connected with the
#' current implementation).  Still, there is an S3 plug that makes sure people
#' loading first ape and then dendextend will still be able to
#' use \code{rotate} without a problem.
#' Notice that if you will first load {ape} and only then {dendextend},
#' using "rotate" will fail with the error: "Error in rotate(dend, ____) :
#'  object "phy" is not of class "phylo"" - this is because rotate in ape
#'  is not S3 and will fail to find the rotate.dendrogram function.
#'  In such a case simply run \code{unloadNamespace(ape)}. Or, you can run:
#'  \code{unloadNamespace("dendextend"); attachNamespace("dendextend")}
#'  The solution for this is that if you have {ape} installed on your machine,
#'  It will be loaded when you load {dendextend} (but after it).
#'  This way, \code{rotate} will work fine for both dendrogram AND phylo
#'  objects.
#'
#' @return A rotated tree object
#' @seealso \code{\link{order.dendrogram}},  \code{\link{order}},
#' \code{\link{rev.dendrogram}}, \code{\link[ape]{rotate}} ({ape}), \link{ladderize}
#' @examples
#' hc <- hclust(dist(USArrests[c(1, 6, 13, 20, 23), ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' # For dendrogram objects:
#' labels_colors(dend) <- rainbow(nleaves(dend))
#' # let's color the labels to make the followup of the rotation easier
#' par(mfrow = c(1, 2))
#' plot(dend, main = "Original tree")
#' plot(rotate(dend, c(2:5, 1)),
#'   main =
#'     "Rotates the left most leaf \n into the right side of the tree"
#' )
#' par(mfrow = c(1, 2))
#' plot(dend, main = "Original tree")
#' plot(sort(dend), main = "Sorts the labels by alphabetical order \n
#' and rotates the tree to give the best fit possible")
#' par(mfrow = c(1, 2))
#' plot(dend, main = "Original tree")
#' plot(rev(dend), main = "Reverses the order of the tree labels")
#'
#' # For hclust objects:
#' plot(hc)
#' plot(rotate(hc, c(2:5, 1)), main = "Rotates the left most leaf \n
#' into the right side of the tree")
#'
#' par(mfrow = c(1, 3))
#' dend %>% plot(main = "Original tree")
#' dend %>%
#'   sort() %>%
#'   plot(main = "labels sort")
#' dend %>%
#'   sort(type = "nodes") %>%
#'   plot(main = "nodes (ladderize) sort")
rotate <- function(x, ...) {
  UseMethod("rotate")
}

### TODO: maybe add k, h parameters.
# ' @param k    numeric scalar, with the number of clusters
# ' the tree should be cut into. The tree is rotate based on the cluster groups.
# ' @param h    numeric scalar, with a height where the tree
# ' should be cut. The tree is rotate based on the cluster groups.


#' @export
#' @rdname rotate
rotate.default <- function(x, order, ...) {
  stop("object x must be a dendrogram/hclust/phylo object")
}

#' @export
#' @rdname rotate
rotate.dendrogram <- function(x, order, ...) {
  if (missing(order)) { # if order is missing - return the same tree.
    warning("'order' parameter is missing, returning the tree as it was.")
    return(x)
  }

  labels_x <- labels(x)
  order_x <- order.dendrogram(x)
  number_of_leaves <- length(order_x)


  if (!is.numeric(order)) {
    order <- as.character(order)
    if (length(intersect(order, labels_x)) != number_of_leaves) {
      stop("'order' is neither numeric nor a vector with ALL of the labels (in the order you want them to be)")
    }
    # order has all the labels, now, let's match:
    # match(c("c", "a", "b"), c("c","b", "a")) # order for making 2 into 1!
    # c("c", "b", "a", "d")[match(c("c", "d", "b", "a"), c("c","b","a", "d"))] # WORKS
    # c("c", "d", "b", "a")[match(c("c", "d", "b", "a"), c("c","b","a", "d"))] # FAIL
    order <- match(order, labels_x)
  }

  weights <- seq_len(number_of_leaves)
  weights_for_order <- numeric(number_of_leaves)
  weights_for_order[order_x[order]] <- weights
  reorder(x, weights_for_order, mean, ...)
}


#' @export
#' @rdname rotate
rotate.hclust <- function(x, order, ...) {
  x_dend <- as.dendrogram(x)
  x_dend_rotated <- rotate(x_dend, order, ...)
  x_rotated <- as_hclust_fixed(x_dend_rotated, x)

  return(x_rotated)
}


#' @export
#' @rdname rotate
rotate.phylo <- function(x, ..., phy) {
  if (!missing(phy)) x <- phy
  # library(ape)
  ape::rotate(phy = x, ...)
}


#' @export
#' @rdname rotate
sort.dendrogram <- function(x, decreasing = FALSE, type = c("labels", "nodes"), ...) {
  type <- match.arg(type)
  switch(type,
    "labels" = rotate(x, order(labels(x), decreasing = decreasing, ...)),
    "nodes" = ladderize(x, right = decreasing, ...)
  )
}

#' @export
#' @rdname rotate
sort.hclust <- function(x, decreasing = FALSE, ...) {
  rotate(x, order(labels(x), decreasing = decreasing, ...))
}


#' @export
#' @rdname rotate
sort.dendlist <- function(x, ...) {
  for (i in seq_len(length(x))) {
    x[[i]] <- sort(x[[i]], ...)
  }
  x
}



# flip <- function(x, ...) {
#    rev_order <- rev(seq_len(nleaves(x)))
#    rotate(x, rev_order)
# }

#' @export
#' @rdname rotate
rev.hclust <- function(x, ...) {
  rev_order <- rev(seq_len(nleaves(x)))
  rotate(x, rev_order, ...)
}


# methods(rotate)
# methods(sort)
# help(flip)
# example(rotate)


###### Some debugging of "rotate" with ape vs dendextend
# library(ape)
# library(dendextend)
# "package:ape" %in% search() # TRUE
# ### to write: package_in_search ???
# tre <- rtree(25)
# detach("package:ape")
# hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
# rotate(hc)
# loadedNamespaces()
# unloadNamespace(ape)
# search()
# unloadNamespace("dendextend"); attachNamespace("dendextend")
# some thoughts: http://www.ats.ucla.edu/stat/r/faq/referencing_objects.htm




#
#
# rotate_group_right <- function(x, ...) {
#    fac_x <- factor(x) # , levels=unique(x))
#    levels_fac_x <- levels(fac_x)
#    levels(fac_x) <- c(tail(levels_fac_x, -1), head(levels_fac_x, 1))
#    return(fac2num(fac_x))
# }
#
#
# hc <- hclust(dist(USArrests[1:5,]), "ave")
# dend <- as.dendrogram(hc)
# x = cutree(dend, k = 3)
# rotate_group_right(rotate_group_right(x))
#

#







#' @title Interactively rotate a tree object
#' @export
#' @rdname click_rotate
#' @description
#' Lets te user click a plot of dendrogram
#' and rotates the tree based on the location of the click.
#'
#' @description
#' Code for mouse selection of (sub-)cluster to be rotated
#'
#' @param x a tree object (either a \code{dendrogram} or \code{hclust})
#' @param plot (logical) should the dendrogram first be plotted.
#' @param plot_after (logical) should the dendrogram be plotted after
#' the rotation?
#' @param horiz logical. Should the plot be normal or horizontal?
#' @param continue logical. If TRUE, allows the user to keep
#' clicking the plot until a click is made on the labels.
#' @param ... parameters passed to the plot
#'
#' @author Andrej-Nikolai Spiess, Tal Galili
#' @return A rotated tree object
#' @seealso \code{\link{rotate.dendrogram}}
#' @examples
#' # create the dend:
#' dend <- USArrests %>%
#'   dist() %>%
#'   hclust("ave") %>%
#'   as.dendrogram() %>%
#'   color_labels()
#' \dontrun{
#' # play with the rotation once
#' dend <- click_rotate(dend)
#' dend <- click_rotate(dend, horiz = TRUE)
#' # keep playing with the rotation:
#' while (TRUE) dend <- click_rotate(dend)
#' # the same as
#' dend <- click_rotate(dend, continue = TRUE)
#' }
#'
click_rotate <- function(x, ...) {
  UseMethod("click_rotate")
}

#' @export
#' @rdname click_rotate
click_rotate.default <- function(x, ...) {
  stop("object x must be a dendrogram/hclust/phylo object")
}

#' @export
#' @rdname click_rotate
click_rotate.dendrogram <- function(x, plot = TRUE, plot_after = plot, horiz = FALSE, continue = FALSE, ...) {
  if (plot) plot(x, horiz = horiz, ...)

  labels_x <- labels(x)
  order_x <- order.dendrogram(x)
  number_of_leaves <- length(order_x)

  only_once <- !continue
  continue <- TRUE

  while (isTRUE(continue)) {
    cat("Please click on top branch of cluster to be rotated...\n")
    if (!only_once) cat("Clicking on the leaf labels will exit...\n\n")
    LOC <- locator(1)
    X <- ifelse(horiz, round(LOC$y), round(LOC$x))
    Y <- ifelse(horiz, LOC$x, LOC$y)
    CLUSTERS <- cutree(x, h = Y, order_clusters_as_data = FALSE)
    order <- 1:length(CLUSTERS)
    CLUSNUM <- CLUSTERS[X]
    SEL <- which(CLUSTERS == CLUSNUM)
    order[SEL] <- rev(order[SEL])

    x <- rotate(x, order)
    if (plot_after) plot(x, horiz = horiz, ...)

    if (Y < 0 | only_once) continue <- FALSE
  }

  cat("Done rotating.\n")

  return(x)
}





















#' @title Ladderize a Tree
#' @export
#' @rdname ladderize
#' @description
#' This function reorganizes the internal structure of the tree to get the ladderized effect when plotted.
#'
#' @param x a tree object (either a \link{dendrogram}, \link{dendlist}, or \link[ape]{phylo})
#' @param right a logical (TRUE) specifying whether the smallest clade is on the right-hand side (when the tree is plotted upwards), or the opposite (if FALSE).
#' @param which an integer (can have any number of elements).
#' It indicates the elements in the \link{dendlist} to ladderize.
#' If missing, it will ladderize all the dendrograms in the dendlist.
#' @param ... Currently ignored.
#' @param phy a placeholder in case the user uses "phy ="
#'
#' @return A rotated tree object
#' @seealso \code{\link[ape]{ladderize}},
#' \code{\link{rev.dendrogram}}, \code{\link{rotate}} ({dendextend}), \code{\link[ape]{rotate}} ({ape})
#' @examples
#'
#' dend <- USArrests[1:8, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("labels_colors") %>%
#'   set("branches_k_color", k = 5)
#' set.seed(123)
#' dend <- shuffle(dend)
#'
#' par(mfrow = c(1, 3))
#' dend %>% plot(main = "Original")
#' dend %>%
#'   ladderize(TRUE) %>%
#'   plot(main = "Right (default)")
#' dend %>%
#'   ladderize(FALSE) %>%
#'   plot(main = "Left (rev of right)")
ladderize <- function(x, right = TRUE, ...) {
  UseMethod("ladderize")
}


#' @export
#' @rdname ladderize
ladderize.dendrogram <- function(x, right = TRUE, ...) {
  if (!is.dendrogram(x)) stop("'object' should be a dendrogram.")

  ladderize_node <- function(node) {
    all_children_leaves <- all(sapply(x, is.leaf))
    if (is.leaf(node) | all_children_leaves) {
      return(unclass(node))
    }
    # else
    attr_backup <- attributes(node)
    new_order <- order(sapply(node, nleaves), decreasing = TRUE)
    node <- node[new_order]
    attributes(node) <- attr_backup # fix attributes

    for (i in 1:length(node)) {
      node[[i]] <- ladderize_node(node[[i]])
    }

    return(unclass(node))
  }

  new_x <- ladderize_node(x)
  class(new_x) <- "dendrogram"
  new_x <- suppressWarnings(stats_midcache.dendrogram(new_x)) # fixes the attributes

  if (!right) new_x <- rev(new_x)

  return(new_x)
}



#' @export
#' @rdname ladderize
ladderize.phylo <- function(x, right = TRUE, phy, ...) {
  if (!missing(phy)) x <- phy
  # library(ape)
  ape::ladderize(phy = x, right = right)
}

#' @export
#' @rdname ladderize
ladderize.dendlist <- function(x, right = TRUE, which, ...) {
  seq_trees <- seq_len(length(x))
  if (missing(which)) which <- seq_trees

  for (i in seq_trees) {
    if (i %in% which) x[[i]] <- ladderize(x[[i]], right = right, ...)
  }
  x
}

# TODO: add tests to ladderize!
# TODO: this is probably a nice starting point for untangle (better than sort). Maybe play with it.
