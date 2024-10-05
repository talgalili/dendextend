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






#' @title Trims one leaf from a dendrogram
#' @description Trims (prunes) one leaf from a dendrogram.
#' @export
#' @param dend dendrogram object
#' @param leaf_name a character string as the label of the tip we wish to prune
#' @param ... passed on
#' @details
#' Used through \link{prune}
#' @return A dendrogram with a leaf pruned
#' @examples
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' par(mfrow = c(1, 2))
#' plot(dend, main = "original tree")
#' plot(prune_leaf(dend, "Alaska"), main = "tree without Alaska")
prune_leaf <- function(dend, leaf_name, ...) {
  labels_dend <- labels(dend)

  if (length(labels_dend) != length(unique(labels_dend))) warning("Found duplicate labels in the tree (this might indicate a problem in the tree you supplied)")

  if (!(leaf_name %in% labels_dend)) { # what to do if there is no such leaf inside the tree
    warning(paste("There is no leaf with the label", leaf_name, "in the tree you supplied", "\n", "Returning original tree", "\n"))
    return(dend)
  }

  if (sum(labels_dend %in% leaf_name) > 1) { # what to do if there is no such leaf inside the tree
    warning(paste(
      "There are multiple leaves by the name of '", leaf_name, "' in the tree you supplied.  Their locations is:",
      paste(which(labels_dend %in% leaf_name), collapse = ","), "\n", "Returning original tree", "\n"
    ))
    return(dend)
  }

  is.father.of.leaf.to.remove <- function(dend, leaf_name) {
    # this function checks if the leaf we wish to remove is the direct child of the current branch (dend) we entered the function
    is.father <- FALSE
    for (i in seq_len(length(dend)))
    {
      if (is.leaf(dend[[i]]) == TRUE && labels(dend[[i]]) == leaf_name) is.father <- TRUE
    }
    return(is.father)
  }


  remove_leaf_if_child <- function(dend, leaf_name) {
    # print(labels(dend))
    if (all(labels(dend) != leaf_name)) { # if the leaf we want to remove is not in this branch, simply return the branch without going deeper into it.
      return(dend)
    } else { # but if the leaf we want to remove is here somewhere, go on searching
      attr(dend, "members") <- attr(dend, "members") - 1

      if (!is.father.of.leaf.to.remove(dend, leaf_name)) # if you are not the father, then go on and make this function work on each child
      {
        for (i in seq_len(length(dend)))
        {
          dend[[i]] <- remove_leaf_if_child(dend[[i]], leaf_name)
        }
      } else { # we'll merge
        if (length(dend) == 2) {
          leaf_location <- 1
          # if leaf location is 1, then move branch in leaf 2 to be the new x
          if (is.leaf(dend[[leaf_location]]) == T && labels(dend[[leaf_location]]) == leaf_name) {
            branch_to_bumpup <- 2
            dend <- dend[[branch_to_bumpup]]
          } else { # else - the leaf location must be located in position "2"

            branch_to_bumpup <- 1
            dend <- dend[[branch_to_bumpup]]
          }
        } else if (length(dend) > 2) {
          # If more than 2 branches, check if any are leaves
          dend_leaves <- unlist(lapply(dend, is.leaf))
          dend_labels <- character(length = length(dend_leaves))
          dend_labels[!dend_leaves] <- NA
          if (sum(dend_leaves) > 0) {
            # If so, check for matching labels to the leaf to prune
            dend_labels[dend_leaves] <- unlist(lapply(dend, function(x) attr(x, "label")))
            dend_matches <- dend_labels == leaf_name
            dend_keep <- which(!(dend_leaves & dend_matches))
            # Filter for only the non-matching members
            pruned <- dend[dend_keep]
            # Transfer attributes to the pruned list
            attributes(pruned) <- attributes(dend)
            # Adjust the "members" attribute of the dend.
            attr(pruned, "members") <- length(dend_keep)
            dend <- pruned
          }
        }
      }
    }
    return(dend)
  }


  new_dend <- remove_leaf_if_child(dend, leaf_name)
  new_dend <- suppressWarnings(stats_midcache.dendrogram(new_dend)) # fixes the attributes
  #   new_x <- fix_members_attr.dendrogram(new_x) # fix the number of memebers attr for each node
  return(new_dend)
}

#' @title Prunes a tree (using leaves' labels)
#' @rdname prune
#' @export
#'
#' @description  Trimms a tree (dendrogram, hclust) from a set of leaves based on their labels.
#'
#' @param dend tree object (dendrogram/hclust/phylo)
#' @param leaves a character vector of the label(S) of the tip(s) (leaves) we wish to prune off the tree.
#' @param reindex_dend logical (default is TRUE). If TRUE, the leaves of the new dendrograms
#' include the rank of the old order.dendrogram.
#' This insures that their values are just like the number of leaves.
#' When FALSE, the values in the leaves is that of the original dendrogram. Thie is useful
#' if prunning a dendrogram but then wanting to use \link{order.dendrogram} with the original values.
#' When using prune.hclust, then reindex_dend is used by default since otherwise the \link{as.hclust} function
#' would return an error.
#' @param ... passed on
#' @details
#' I was not sure if to call this function drop.tip (from ape), snip/prune (from rpart) or just remove.leaves.  I ended up deciding on prune.
#'
#' @return A pruned tree
#' @seealso \link{prune_leaf}, \link[ape]{drop.tip}
#' @examples
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' par(mfrow = c(1, 2))
#' plot(dend, main = "original tree")
#' plot(prune(dend, c("Alaska", "California")), main = "tree without Alaska and California")
#'
#'
#' # this works because prune uses reindex_dend = TRUE by default
#' as.hclust(prune(dend, c("Alaska", "California")))
#' prune(hc, c("Alaska", "California"))
prune <- function(dend, ...) {
  UseMethod("prune")
}

#' @export
#' @rdname prune
prune.default <- function(dend, ...) {
  stop("object dend must be a dendrogram/hclust/phylo object")
}

# ' @S3method prune dendrogram
#' @export
#' @rdname prune
prune.dendrogram <- function(dend, leaves, reindex_dend = TRUE, ...) {
  leaves <- as.character(leaves)

  for (i in seq_along(leaves))
  {
    # this function is probably not the fastest - but it works...
    dend <- prune_leaf(dend, leaves[i]) # move step by stem to remove all of these leaves...
  }

  if (reindex_dend) dend <- reindex_dend(dend)

  return(dend)
}


# ' @S3method prune hclust
#' @export
#' @rdname prune
prune.hclust <- function(dend, leaves, ...) {
  x_dend <- as.dendrogram(dend)
  x_dend_pruned <- x_dend %>%
    prune(leaves, ...) %>%
    reindex_dend()
  x_pruned <- as_hclust_fixed(x_dend_pruned, dend)

  return(x_pruned)
}

# ' @S3method prune phylo
#' @export
#' @rdname prune
prune.phylo <- function(dend, ...) {
  # library(ape)
  ape::drop.tip(phy = dend, ...)
}


#' @export
#' @rdname prune
prune.rpart <- function(dend, ...) {
  # library(ape)
  rpart::prune.rpart(tree = dend, ...)
}




#' Prune trees to their common subtrees
#'
#' @param dend a \link{dendlist} of length two
#' @param ... ignored
#'
#' @return
#' A dendlist after prunning the labels to only include
#' those that are part of common subtrees in both dendrograms.
#'
#' @export
#' @seealso \link{common_subtrees_clusters}
#'
#' @examples
#'
#' # NULL
prune_common_subtrees.dendlist <- function(dend, ...) {
  if (!length(dend) == 2) stop("The dend must of be of length 2")
  if (!is.dendlist(dend)) stop("The dend must of be of class dendlist")

  # dend <- d_train_test
  clusters <- common_subtrees_clusters(dend[[1]], dend[[2]])
  labels_to_prune <- labels(dend[[1]])[clusters == 0]
  dend1 <- prune(dend[[1]], labels_to_prune)
  dend2 <- prune(dend[[2]], labels_to_prune)
  dend_12 <- dendlist(dend1, dend2)
  names(dend_12) <- names(dend)
  dend_12
}







#' @title Intersect trees
#' @description
#' Return two trees after pruning them so that the only leaves left are the intersection of their labels.
#' @export
#' @param dend1 tree object (dendrogram/hclust/phylo)
#' @param dend2 tree object (dendrogram/hclust/phylo)
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' Should a warning be issued if there was a need to perform intersaction.
#' @param ... passed on
#' @return A \link{dendlist} with two pruned trees
#' @seealso \link{prune}, \link{intersect}, \link{labels}
#' @examples
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#' labels(dend) <- 1:5
#' dend1 <- prune(dend, 1)
#' dend2 <- prune(dend, 5)
#' intersect_dend <- intersect_trees(dend1, dend2)
#'
#' layout(matrix(c(1, 1, 2, 3, 4, 5), 3, 2, byrow = TRUE))
#' plot(dend, main = "Original tree")
#' plot(dend1, main = "Tree 1:\n original with label 1 pruned")
#' plot(dend2, main = "Tree 2:\n original with label 2 pruned")
#' plot(intersect_dend[[1]],
#'   main = "Tree 1 pruned
#'       with the labels that intersected with those of Tree 2"
#' )
#' plot(intersect_dend[[2]],
#'   main = "Tree 2 pruned
#'       with the labels that intersected with those of Tree 1"
#' )
intersect_trees <- function(dend1, dend2, warn = dendextend_options("warn"), ...) {
  labels_dend1 <- labels(dend1)
  labels_dend2 <- labels(dend2)
  intersected_labels <- intersect(labels_dend1, labels_dend2)

  if (length(intersected_labels) == 0) {
    warning("The two trees had no common labels!")
    return(dendlist())
  }

  # prune tree 1
  ss_labels_to_keep <- labels_dend1 %in% intersected_labels
  ss_labels_to_prune_1 <- !ss_labels_to_keep
  pruned_dend1 <- prune(dend1, labels_dend1[ss_labels_to_prune_1])

  # prune tree 2
  ss_labels_to_keep <- labels_dend2 %in% intersected_labels
  ss_labels_to_prune_2 <- !ss_labels_to_keep
  pruned_dend2 <- prune(dend2, labels_dend2[ss_labels_to_prune_2])

  if (warn && any(c(ss_labels_to_prune_1, ss_labels_to_prune_2))) {
    warning("The labels in both tree had different values - trees were pruned.")
  }

  return(dendlist(pruned_dend1, pruned_dend2))
}



#' @title Reindexing a pruned dendrogram
#' @export
#'
#' @description \code{prune_leaf} does not update leaf indices as it prune
#' leaves. As a result, some leaves of the pruned dendrogram may have leaf
#' indeices larger than the number of leaves in the pruned dendrogram, which may
#' cause errors in downstream functions such as \code{as.hclust}.
#'
#' This function re-indexes the leaves such that the leaf indices are no larger
#' than the total number of leaves.
#'
#' @param dend dendrogram object
#'
#' @return A \code{dendrogram} object with the leaf reindexed
#'
#'
#' @examples
#' hc <- hclust(dist(USArrests[1:5, ]), "ave")
#' dend <- as.dendrogram(hc)
#'
#' dend_pruned <- prune(dend, c("Alaska", "California"), reindex_dend = FALSE)
#'
#' ## A leave have an index larger than the number of leaves:
#' unlist(dend_pruned)
#' # [1] 4 3 1
#' #'
#' dend_pruned_reindexed <- reindex_dend(dend_pruned)
#'
#' ## All leaf indices are no larger than the number of leaves:
#' unlist(dend_pruned_reindexed)
#' # [1] 3 2 1
#'
#' ## The dendrograms are equal:
#' all.equal(dend_pruned, dend_pruned_reindexed)
#' # TRUE
reindex_dend <- function(dend) {
  order.dendrogram(dend) <- dend %>%
    order.dendrogram() %>%
    rank() %>%
    as.integer()
  # as.integer(rank(order.dendrogram(dend)))
  return(dend)
}




# methods(prune)
# example(rotate)
# example(prune)
# example(intersect_trees)
