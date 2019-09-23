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





#' @title Counts the number of leaves in a tree
#' @rdname nleaves
#' @export
#' @description
#' Counts the number of leaves in a tree (dendrogram or hclust).
#'
#' @param x tree object (dendrogram/hclust/phylo,\link{dendlist})
#' @param method a character scalar (default is "members"). If "order"
#' than nleaves is based on length of \link{order.dendrogram}.
#' If "members", than length is trusting what is written in the
#' dendrogram's root \link{attr}.
#' "members" is about 4 times faster than "order".
#'
#' @param ... not used
#' @details
#' The idea for the name is from functions like ncol, and nrow.
#'
#' Also, it is worth noting that the nleaves.dendrogram is based on
#' order.dendrogram instead of labels.dendrogram since the first is
#' MUCH faster than the later.
#'
#' The phylo method is based on turning the phylo to hclust and than to
#' dendrogram. It may not work for complex phylo trees.
#'
#' @return The number of leaves in the tree
#' @seealso \link{nrow}, \link{count_terminal_nodes}
#' @examples
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' nleaves(dend) # 5
#' nleaves(hc) # 5
nleaves <- function(x, ...) {
  UseMethod("nleaves")
}

#' @export
#' @rdname nleaves
nleaves.default <- function(x, ...) {
  stop("object x must be a dendrogram/hclust/phylo object")
}

#' @export
#' @rdname nleaves
nleaves.dendrogram <- function(x, method = c("members", "order"), ...) {
  if (method[1] == "members") {
    return(as.integer(attr(x, "members")))
  } else {
    # "order"
    return(length(order.dendrogram(x)))
  }
}


#' @export
#' @rdname nleaves
nleaves.dendlist <- function(x, ...) {
  sapply(x, nleaves, ...)
}



# library(microbenchmark)
# big_dend <- USArrests %>% dist %>% hclust %>% as.dendrogram
# microbenchmark(
#    nleaves.dendrogram(big_dend, "order"),
#    nleaves.dendrogram(big_dend, "members")
# )


# TODO: add count by number of labels/ or by the number of is.leaf==TRUE


#' @export
#' @rdname nleaves
nleaves.hclust <- function(x, ...) {
  length(x$order)
}


#' @export
#' @rdname nleaves
nleaves.phylo <- function(x, ...) {
  # nleaves(as.dendrogram(x))
  length(labels(x))
}

# TODO: there is probably a better way for getting the tree size for a phylo object.




#' @title Counts the number of nodes (Vertices) in a tree
#' @export
#' @rdname nnodes
#' @description Counts the number of nodes in a tree (dendrogram, hclust, phylo).
#'
#' @param x tree object (dendrogram or hclust)
#' @param ... not used
#' @details
#' The idea for the name is from functions like ncol, and nrow.
#'
#' The phylo method is based on turning the phylo to hclust and than to
#' dendrogram. It may not work for complex phylo trees.
#'
#' @return The number of leaves in the tree
#' @seealso \link{nrow}, \link{count_terminal_nodes}, \link{nleaves}
#' @examples
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' nnodes(dend) # 9
#' nnodes(hc) # 9
nnodes <- function(x, ...) {
  UseMethod("nnodes")
}

#' @export
#' @rdname nnodes
nnodes.default <- function(x, ...) {
  stop("object x must be a dendrogram/hclust/phylo object")
}

#' @export
#' @rdname nnodes
nnodes.dendrogram <- function(x, ...) {
  if (!inherits(x, "dendrogram")) warning("'object' should be a dendrogram.")

  i_node_counter <- 0

  go_through_nodes <- function(dend_node) {
    i_node_counter <<- i_node_counter + 1
  }

  dendrapply(x, go_through_nodes)

  return(i_node_counter)
}
# nnodes(dend)


#' @export
#' @rdname nnodes
nnodes.hclust <- function(x, ...) {
  nnodes(as.dendrogram(x))
}

#' @export
#' @rdname nnodes
nnodes.phylo <- function(x, ...) {
  nnodes(as.dendrogram(x))
}





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
#' hc <- hclust(dist(USArrests[1:3, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' ###
#' # Trivial case
#' count_terminal_nodes(dend) # 3 terminal nodes
#' length(labels(dend)) # 3 - the same number
#' plot(dend,
#'   main = "This is considered a tree \n with THREE terminal nodes (leaves)"
#' )
#'
#' ###
#' # NON-Trivial case
#' str(dend)
#' attr(dend[[2]], "height") <- 0
#' count_terminal_nodes(dend) # 2 terminal nodes, why? see this plot:
#' plot(dend,
#'   main = "This is considered a tree \n with TWO terminal nodes only"
#' )
#' # while we have 3 leaves, in practice we have only 2 terminal nodes
#' # (this is a feature, not a bug.)
count_terminal_nodes <- function(dend_node, ...) {

  # a recursive function for counting the number of first encountered zero nodes
  # if you are a leaf, or a node with height 0, return 1
  if (is.leaf(dend_node) | attr(dend_node, "height") == 0) {
    return(1L)
  }

  # else : # go through all of the childrens and apply count.first.zero.nodes
  terminal_nodes_counter <- 0
  for (i in seq_len(length(dend_node))) {
    terminal_nodes_counter <- terminal_nodes_counter + # let each sub-node add its counts
      count_terminal_nodes(dend_node[[i]], ...)
  }
  return(terminal_nodes_counter)
}




#' @title unclass an entire dendrogram tree
#' @description unclass all the nodes in a dendrogram tree. (Helps in cases when a dendrapply function was used wrongly)
#' @export
#' @param dend a dendrogram object
#' @param ... not used
#' @return The list which was the dendrogram (but without a class)
#' @seealso \link{nleaves}
#' @examples
#' # define dendrogram object to play with:
#' hc <- hclust(dist(USArrests[1:3, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' itself <- function(x) x
#' dend <- dendrapply(dend, itself)
#' unclass(dend) # this only returns a list with
#' # two dendrogram objects inside it.
#' str(dend) # this is a great way to show a dendrogram,
#' # but it doesn't help us understand how the R object is built.
#' str(unclass(dend)) # this is a great way to show a dendrogram,
#' # but it doesn't help us understand how the R object is built.
#' unclass_dend(dend) # this only returns a list
#' #  with two dendrogram objects inside it.
#' str(unclass_dend(dend)) # NOW we can more easily understand
#' # how the dendrogram object is structured...
unclass_dend <- function(dend, ...) {
  # I could have also made this into a method
  # unclass_dendrogram
  # But decided not to, so to allow unclass to work as usual in case someone wants to use it only on one branch.
  #    if(!is.leaf(dend))
  #    {
  #       for(i in seq_len(length(dend)))
  #       {
  #          dend[[i]] <- unclass_dend(dend[[i]])
  #       }
  #    }
  #    dend <- unclass(dend)
  dend <- dendrapply(dend, unclass)
  return(dend)
}



# example(count_terminal_nodes)
# example(labels_colors)




#' @export
head.dendrogram <- function(x, n = 3L, ...) {
  # library(utils)
  utils::str(x, max.leve = n, ...)
  cat("etc...", "\n")
}
# hc <- hclust(dist(USArrests), "ave")
# dend <- as.dendrogram(hc)
# str(dend)
# head(dend)
# Some thoughts on imports: http://obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/
# and: http://stackoverflow.com/questions/8637993/better-explanation-of-when-to-use-imports-depends

# methods(nleaves)




# This is what should have been done for dendrograms
# order.default <- order
# order <- function(...) UseMethod("order")
