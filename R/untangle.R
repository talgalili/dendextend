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






#' @title untangle dendrograms
#' @export
#' @rdname untangle
#' @description
#' One untangle function to rule them all.
#'
#' This function untangles dendrogram lists (dendlist),
#' Using various heuristics.
#'
#' @author Tal Galili
#'
#' @param dend1 a dednrogram or a dendlist object
#' @param dend2 A second dednrogram (to untangle against)
#' @param which an integer vector of length 2, indicating
#' which of the trees in the dendlist object should be plotted
#' @param method a character indicating the type of untangle
#' heuristic to use.
#' @param ... passed to the releavnt untangle function
#' @details
#' This function wraps all of the untagnle functions,
#' in order to make it easier to find our about (and use) them.
#' @return A \link{dendlist}, with two trees after
#' they have been untangled.
#'
#' If the dendlist was originally larger than 2, it will return the original dendlist
#' but with the relevant trees properly rotate.
#'
#' @seealso
#' \link{tanglegram}, \link{untangle_random_search},
#' \link{untangle_step_rotate_1side}, \link{untangle_step_rotate_2side},
#' \link{untangle_DendSer},
#' \link{entanglement}
#' @examples
#' \dontrun{
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#' dend1 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("com") %>%
#'   as.dendrogram()
#' dend2 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("sin") %>%
#'   as.dendrogram()
#' dend12 <- dendlist(dend1, dend2)
#'
#' dend12 %>% tanglegram()
#'
#' untangle(dend1, dend2, method = "random", R = 5) %>% tanglegram()
#'
#' # it works, and we get something different:
#' set.seed(1234)
#' dend12 %>%
#'   untangle(method = "random", R = 5) %>%
#'   tanglegram()
#'
#' set.seed(1234)
#' # fixes it completely:
#' dend12 %>%
#'   untangle(method = "random", R = 5) %>%
#'   untangle(method = "step1") %>%
#'   tanglegram()
#' # not good enough
#' dend12 %>%
#'   untangle(method = "step1") %>%
#'   tanglegram()
#' # not good enough
#' dend12 %>%
#'   untangle(method = "step2") %>%
#'   tanglegram()
#' # How we might wish to use it:
#' set.seed(12777)
#' dend12 %>%
#'   untangle(method = "random", R = 1) %>%
#'   untangle(method = "step2") %>%
#'   tanglegram()
#' }
untangle <- function(dend1, ...) {
  UseMethod("untangle")
}

#' @export
#' @rdname untangle
untangle.default <- function(dend1, ...) {
  stop("No default function for tanglegram - must use a dendrogram/hclust/phylo object")
}



#' @export
#' @rdname untangle
untangle_labels <- function(dend1, dend2, ...) {
  dend2 <- rotate(dend2, labels(dend1))
  dendlist(dend1, dend2)
}




#' @export
#' @rdname untangle
untangle.dendrogram <- function(dend1, dend2,
                                method = c("labels", "ladderize", "random", "step1side", "step2side", "DendSer"), ...) {
  method <- match.arg(method)

  switch(method,
    random = untangle_random_search(dend1, dend2, ...),
    step1side = untangle_step_rotate_1side(dend1, dend2, ...),
    step2side = untangle_step_rotate_2side(dend1, dend2, ...),
    DendSer = untangle_DendSer(dendlist(dend1, dend2), ...),
    ladderize = ladderize(dendlist(dend1, dend2), ...),
    labels = untangle_labels(dend1, dend2, ...)
  )
}

#' @export
#' @rdname untangle
untangle.dendlist <- function(dend1,
                              method = c("labels", "ladderize", "random", "step1side", "step2side", "DendSer"),
                              which = c(1L, 2L), ...) {
  method <- match.arg(method)
  the_names <- names(dend1)[which]

  untangle_result <- untangle(dend1[[which[1]]], dend1[[which[2]]], method = method, ...)

  if (length(dend1) > 2) {
    dend1[[which[1]]] <- untangle_result[[1]]
    dend1[[which[2]]] <- untangle_result[[2]]
    names(dend1) <- the_names
    return(dend1)
  } else { # no need for all the copying if the list had only two elements in it.
    names(untangle_result) <- the_names
    return(untangle_result)
  }
}


# center <- function(type = c("mean", "median", "trimmed")) {
#    print(match.arg(type))
# }
# center(type="tri")



# get("sort")
#' 'shuffle' is a function that randomilly rotates ("shuffles") a tree.
#' a dendrogram leaves order (by means of rotation)

#' @title Random rotation of trees
#' @export
#' @rdname shuffle
#'
#' @description
#' 'shuffle' randomilly rotates ("shuffles") a tree, changing its presentation
#' while preserving its topolgoy.
#' 'shuffle' is based on \link[dendextend]{rotate} and through its methods can
#' work for any of the major tree objects in R (\link{dendrogram}/\link{hclust}/\link[ape]{phylo}).
#'
#' This function is useful in combination with \link{tanglegram} and \link{entanglement}.
#'
#' @param dend a tree object (\link{dendrogram}/\link{hclust}/\link[ape]{phylo})
#' @param which an integer vector for indicating
#' which of the trees in the dendlist object should be plotted
#' default is missing, in which case all the dends in dendlist
#' will be shuffled
#' @param ... Ignored.
#'
#' @return A randomlly rotated tree object
#' @seealso \code{\link{tanglegram}},  \code{\link{entanglement}},
#' \code{\link[dendextend]{rotate}}
#' @examples
#' dend <- USArrests %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' set.seed(234238)
#' dend2 <- shuffle(dend)
#'
#' tanglegram(dend, dend2, margin_inner = 7)
#' entanglement(dend, dend2) # 0.3983
#'
#' # although these ARE the SAME tree:
#' tanglegram(sort(dend), sort(dend2), margin_inner = 7)
shuffle <- function(dend, ...) {
  UseMethod("shuffle")
}

#' @export
#' @rdname shuffle
shuffle.default <- function(dend, ...) {
  # takes a dendrogram object and shuffles its branches in a random fashion
  # 	n_leaves <- length(labels(dend))	# leaves.value is faster then labels!
  n_leaves <- nleaves(dend)
  random_weights <- sample(seq_len(n_leaves)) # a random ordaring of 1:n_leaves weights
  rotate(dend, random_weights) # since we have a method here for dend/hclust/phylo - this makes this function rather generic...
}


#' @export
#' @rdname shuffle
shuffle.dendrogram <- shuffle.default


#' @export
#' @rdname shuffle
shuffle.dendlist <- function(dend, which, ...) {

  #    if(T) 1 else 2
  #    if(F) 1 else 2
  #    if(F) 1 else
  #       2
  what_to_shuffle <- if (missing(which)) seq_len(length(dend)) else which

  for (i in what_to_shuffle) {
    dend[[i]] <- shuffle(dend[[i]])
  }

  dend
}


#' @export
#' @rdname shuffle
shuffle.hclust <- shuffle.default

#' @export
#' @rdname shuffle
shuffle.phylo <- shuffle.default





#' @title Untangle - random search
#' @export
#' @description
#' Searches for two untangled dendrogram by randomlly shuflling them and each
#' time checking if their entanglement was improved.
#'
#' @param dend1 a tree object (of class dendrogram/hclust/phylo).
#' @param dend2 a tree object (of class dendrogram/hclust/phylo).
#' @param R numeric (default is 100). The number of shuffles to perform.
#' @param L the distance norm to use for measuring the distance between the
#' two trees. It can be any positive number, often one will want to
#'  use 0, 1, 1.5, 2 (see 'details' for more).
#'  It is passed to \link{entanglement}.
#' @param leaves_matching_method a character scalar passed to \link{entanglement}.
#' It can be either "order" or "labels" (default). If using "labels",
#' then we use the labels for matching the leaves order value.
#' And if "order" then we use the old leaves order value for matching the
#' leaves order value.
#'
#' Using "order" is faster, but "labels" is safer. "order" will assume that
#' the original two trees had their labels and order values MATCHED.
#'
#' Hence, it is best to make sure that the trees used here have the same labels
#' and the SAME values matched to these values - and then use "order" (for
#' fastest results).
#'
#' If "order" is used, the function first calls \link{match_order_by_labels}
#' in order to make sure that the two trees have their labels synced with
#' their leaves order values.
#'
#' @param ... not used
#'
#' @details
#'
#' Untangaling two trees is a hard combinatorical problem without a closed
#' form solution. One way for doing it is to run through a random spectrom
#' of options and look for the "best" two trees. This is what this function
#' offers.
#'
#' @return A dendlist with two trees with the best entanglement that was found.
#' @seealso \link{tanglegram}, \link{match_order_by_labels},
#' \link{entanglement}.
#' @examples
#'
#' \dontrun{
#' dend1 <- iris[, -5] %>%
#'   dist() %>%
#'   hclust("com") %>%
#'   as.dendrogram()
#' dend2 <- iris[, -5] %>%
#'   dist() %>%
#'   hclust("sin") %>%
#'   as.dendrogram()
#' tanglegram(dend1, dend2)
#'
#' set.seed(65168)
#' dend12 <- untangle_random_search(dend1, dend2, R = 10)
#' tanglegram(dend12[[1]], dend12[[2]])
#' tanglegram(dend12)
#'
#' entanglement(dend1, dend2, L = 2) # 0.8894
#' entanglement(dend12[[1]], dend12[[2]], L = 2) # 0.0998
#' }
untangle_random_search <- function(dend1, dend2, R = 100L, L = 1, leaves_matching_method = c("labels", "order"), ...) {
  # this is a simple random search algorithm for the optimal tanglegram layout problem.
  # it shufflers the trees, and see if we got a better entanglement or not

  leaves_matching_method <- match.arg(leaves_matching_method)
  if (leaves_matching_method == "order") {
    old_dend2 <- dend2
    dend2 <- match_order_by_labels(old_dend2, dend1)
    if (!identical(dend2, old_dend2) & dendextend_options("warn")) warning("The leaves order in 'dend2' were changed. If you want to avoid that, use leaves_matching_method = 'labels'.")
  }

  optimal_dend1 <- dend1
  optimal_dend2 <- dend2

  best_ordaring_entanglement <- entanglement(dend1, dend2, L, leaves_matching_method)

  for (i in 1:R) {
    s_dend1 <- shuffle(dend1)
    s_dend2 <- shuffle(dend2)
    current_entanglement <- entanglement(s_dend1, s_dend2, L, leaves_matching_method)

    # if we came across a better ordaring, then update the "Best" treerograms
    if (current_entanglement < best_ordaring_entanglement) {
      best_ordaring_entanglement <- current_entanglement
      optimal_dend1 <- s_dend1
      optimal_dend2 <- s_dend2
    }
  }

  return(dendlist(optimal_dend1, optimal_dend2))
}




flip_strings <- function(STRING, str1, str2) {
  # gets a string which includes str1 and str2, and makes sure to flip them in the string
  STRING <- sub(str1, "_____1_", STRING, fixed = T) # substitutes the first string with a place holder (1)
  STRING <- sub(str2, "_____2_", STRING, fixed = T) # substitutes the second string with a place holder (2)
  STRING <- sub("_____1_", str2, STRING, fixed = T) # substitutes the place holder (1) with the second string
  STRING <- sub("_____2_", str1, STRING, fixed = T) # substitutes the place holder (2) with the first string
  return(STRING)
}
# flip_strings("abcdefgh", "ab", "fgh") # "fghcdeab"

add_zzz <- function(x) {
  # this function adds a"_" character to the end of every element of the vector.
  # this is used to make numeric values unique (so to not confuse 1 and 10 or 17 and 7 !)
  x <- as.character(x)
  x <- paste("zzz", x, "zzz", sep = "")
  x
}
remove_zzz <- function(x) {
  gsub("zzz", "", x, fixed = T)
}
# remove_zzz(add_zzz(1:6))
collapse_with_pipes <- function(x) {
  paste(x, collapse = "||")
}
collapse_pipes_zzz <- function(x) {
  paste(add_zzz(x), collapse = "||")
}
remove_pipes_and_zzz <- function(x) {
  strsplit(remove_zzz(x), "||", fixed = T)[[1]]
}


#' @title Flip leaves
#' @export
#' @description
#' Rotate a branch in a tree so that the locations of two bundles of leaves
#' are flipped.
#'
#' @param dend a dendrogram object
#' @param leaves1 a vector of leaves order value to flip.
#' @param leaves2 a (second) vector of leaves order value to flip.
#' @param ... not used
#' @details
#' This function is based on a bunch of string manipulation functions. There
#' may be a smarter/better way for doing it...
#'
#' @return A dendrogram object with flipped leaves.
#' @seealso \link{tanglegram}, \link{match_order_by_labels},
#' \link{entanglement}.
#' @examples
#'
#' \dontrun{
#' dend1 <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend2 <- flip_leaves(dend1, c(3, 5), c(1, 2))
#' tanglegram(dend1, dend2)
#' entanglement(dend1, dend2, L = 2) # 0.4
#' }
flip_leaves <- function(dend, leaves1, leaves2, ...) {
  # flip a node in a tree based on the leaves in each branch in the node:
  # this function gets a dendgram with two vector of leaves that needs to be flipped with one another on the tree
  # we assume here unique values of leaves.
  # also notice that this is based on the values of the leaves and NOT their labels.
  leaves_order <- order.dendrogram(dend)
  weights <- seq_along(leaves_order)

  # turn the values of leaves and leaves1/2 to strings with || delim:
  leaves_order_string <- collapse_pipes_zzz(leaves_order)
  leaves1_string <- collapse_pipes_zzz(leaves1)
  leaves2_string <- collapse_pipes_zzz(leaves2)
  # then flips the locations of leaves1 and 2 in the string
  flipped_leaves_order_string <- flip_strings(leaves_order_string, leaves1_string, leaves2_string)
  # and turn the string back to a vector of flipped leaves values:
  flipped_leaves_order <- as.integer(remove_pipes_and_zzz(flipped_leaves_order_string))

  new_order_weights <- match(flipped_leaves_order, leaves_order) # order the leaves_order to be like flipped_leaves_order
  # leaves_order[new_order_weights]
  # now use this order to order the weights!
  new_weights <- weights[new_order_weights]

  flipped_dend <- rotate(dend, new_weights) # and lastly - rotate the dend by the leaves to flip.

  return(flipped_dend)
}


# I didn't use this evantually:
# library(combinat)
# source for this package: https://stackoverflow.com/questions/7906332/how-to-calculate-combination-and-permutation-in-r



#' @title Rotate tree branches for k
#' @export
#' @description
#' Given a tree and a k number of clusters, the tree is rotated so that the
#' extra clusters added from k-1 to k clusters are flipped.
#'
#' This is useful for finding good trees for a \link{tanglegram}.
#' @param dend a dendrogram object
#' @param k integer scalar with the number of clusters the tree should be cut into.
#' @param dend_heights_per_k a named vector that resulted from running
#' \link{heights_per_k.dendrogram}. When running the function many times,
#' supplying this object will help improve the running time if using the
#' \link{cutree.dendrogram} method..
#'
#' @param ... not used
#' @return A list with dendrogram objects with all the possible rotations
#' for k clusters (beyond the k-1 clusters!).
#' @seealso \link{tanglegram}, \link{match_order_by_labels},
#' \link{entanglement}, \link{flip_leaves}.
#' @examples
#'
#' \dontrun{
#' dend1 <- USArrests[1:5, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend2 <- all_couple_rotations_at_k(dend1, k = 2)[[2]]
#' tanglegram(dend1, dend2)
#' entanglement(dend1, dend2, L = 2) # 0.5
#'
#' dend2 <- all_couple_rotations_at_k(dend1, k = 3)[[2]]
#' tanglegram(dend1, dend2)
#' entanglement(dend1, dend2, L = 2) # 0.4
#'
#' dend2 <- all_couple_rotations_at_k(dend1, k = 4)[[2]]
#' tanglegram(dend1, dend2)
#' entanglement(dend1, dend2, L = 2) # 0.05
#' }
all_couple_rotations_at_k <- function(dend, k, dend_heights_per_k, ...) {
  # This function gets the dend tree, and a k number of clusters
  # and returns all of the permutated dendrogram trees, rotating only two of the k clusters at each permutation
  # if this was done for ALL permutation, the algorithm would not be feasable.
  # practically, for a binary tree - this only gives two trees as an output (the original, and the flipped new k'th cluster)

  if (length(k) != 1L) {
    warning("'k' should be an integer SCALAR, using only the first element of k.")
    k <- k[1L]
  }
  if (k == 1) {
    return(dend)
  } # there are no possible rotations for k==1

  if (missing(dend_heights_per_k)) {
    dend_heights_per_k <- heights_per_k.dendrogram(dend)
  } # since this function takes a looong time, I'm running it here so it will need to run only once!
  # And I would MUCH rather give this vector upfront - so the entire thing will be faster...

  leaves_order <- order.dendrogram(dend)
  k_cluster_leaves <- cutree(dend, k,
    order_clusters_as_data = FALSE,
    dend_heights_per_k = dend_heights_per_k, # makes it faster
    use_labels_not_values = FALSE
  ) # makes it 10 times faster (and we don't use the labels of the clusters, only the cluster vector)
  km1_cluster_leaves <- cutree(dend, k - 1,
    order_clusters_as_data = FALSE,
    dend_heights_per_k = dend_heights_per_k, # makes it faster
    use_labels_not_values = FALSE
  ) # makes it 10 times faster (and we don't use the labels of the clusters, only the cluster vector)

  # if we can't cut the current stage (for example, because we have more than 2 branches, than return the original tree
  if (any(is.na(k_cluster_leaves))) {
    return(list(dend))
  }
  # If we can't cut the tree above us, then loop up until you find a k for which you can cut.
  # there might be bugs for this code, more careful thought should be made in such cases...
  while (any(is.na(km1_cluster_leaves))) {
    k <- k - 1
    km1_cluster_leaves <- cutree(dend, k - 1,
      order_clusters_as_data = FALSE,
      dend_heights_per_k = dend_heights_per_k, # makes it faster
      use_labels_not_values = FALSE
    ) # makes it 10 times faster (and we don't use the labels of the clusters, only the cluster vector)
    warning(paste("couldn't cut tree at k-1, trying it for", k - 1))
  }


  # kkm1_df <-
  # data.frame(km1_cluster_leaves, k_cluster_leaves)

  permutated_dend <- list(dend) # this one will hold all of the permutations
  permutation_indx <- 1 # this one will tell us at what stage of the permutation we are at

  for (i in unique(km1_cluster_leaves)) {
    ss <- i == km1_cluster_leaves
    unique_clusters_in_branch <- unique(k_cluster_leaves[ss])

    if (length(unique_clusters_in_branch) > 1) { # the only way there is a reason to do permutations here is if the current cluster we are looking at has more than 1 member
      number_of_clusters_in_branch <- length(unique_clusters_in_branch)
      branches_permutations <- as.matrix(combn(unique_clusters_in_branch, 2)) # a matrix were each column is a permutation of 2 out of the clusters in this branch (most often just 2, but sometimes more...)
      # 		as.matrix(combn(1:3, 2))
      # permn(number_of_clusters_in_branch) # this will be 2 most of the time, but this structure allows one to deal with clusters which have more than 2 branches
      n_permutations <- ncol(branches_permutations)

      for (j in seq_len(n_permutations)) { # would often run just once.

        # choosing the leaves belonging to each of the two clusters
        ss_leaves1 <- k_cluster_leaves == branches_permutations[1, j]
        ss_leaves2 <- k_cluster_leaves == branches_permutations[2, j]
        leaves1 <- leaves_order[ss_leaves1]
        leaves2 <- leaves_order[ss_leaves2]

        # 				plot(flip_leaves(dend, leaves1, leaves2))
        # Flipping the branches of the two adjecent clusters:
        permutation_indx <- permutation_indx + 1
        permutated_dend[[permutation_indx]] <- flip_leaves(dend, leaves1, leaves2) # this will not work for hclust (will for dend)
      }
    }
  }
  return(permutated_dend)
}





#' @title Stepwise untangle one tree compared to another
#' @export
#' @description
#' Given a fixed tree and a tree we wish to rotate, this function goes
#' through all of the k number of clusters (from 2 onward), and each time
#' rotates the branch which was introduced in the new k'th cluster.
#' This rotated tree is compared with the fixed tree, and if it has a better
#' entanglement, it will be used for the following iterations.
#'
#' This is a greedy forward selection algorithm for rotating the tree and
#' looking for a better match.
#'
#' This is useful for finding good trees for a \link{tanglegram}.
#' @param dend1 a dendrogram object. The one we will rotate to best fit
#' dend2_fixed.
#' @param dend2_fixed a dendrogram object. This one is kept fixed.
#' @param L the distance norm to use for measuring the distance between the
#' two trees. It can be any positive number,
#' often one will want to use 0, 1, 1.5, 2 (see 'details' in \link{entanglement}).
#'
#' @param direction a character scalar, either "forward" (default) or "backward".
#' Impacts the direction of clustering that are tried. Either from 2 and up
#' (in case of "forward"), or from nleaves to down (in case of "backward")
#'
#' If k_seq is not NULL, then it overrides "direction".
#'
#' @param k_seq a sequence of k clusters to go through for improving
#' dend1. If NULL (default), then we use the "direction" parameter.
#'
#' @param dend_heights_per_k a numeric vector of values which indicate which height will produce which number of clusters (k)
#'
#' @param leaves_matching_method a character scalar passed to \link{entanglement}.
#' It can be either "order" or "labels" (default). If using "labels",
#' then we use the labels for matching the leaves order value.
#' And if "order" then we use the old leaves order value for matching the
#' leaves order value.
#'
#' Using "order" is faster, but "labels" is safer. "order" will assume that
#' the original two trees had their labels and order values MATCHED.
#'
#' Hence, it is best to make sure that the trees used here have the same labels
#' and the SAME values matched to these values - and then use "order" (for
#' fastest results).
#'
#' If "order" is used, the function first calls \link{match_order_by_labels}
#' in order to make sure that the two trees have their labels synced with
#' their leaves order values.
#'
#' @param ... not used
#'
#' @return A dendlist with
#' 1) dend1 after it was rotated to best fit dend2_fixed.
#' 2) dend2_fixed.
#' @seealso \link{tanglegram}, \link{match_order_by_labels},
#' \link{entanglement}, \link{flip_leaves}, \link{all_couple_rotations_at_k},
#' \link{untangle_step_rotate_2side}.
#'
#' @examples
#'
#' \dontrun{
#' dend1 <- USArrests[1:10, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' set.seed(3525)
#' dend2 <- shuffle(dend1)
#' tanglegram(dend1, dend2)
#' entanglement(dend1, dend2, L = 2) # 0.4727
#'
#' dend2_corrected <- untangle_step_rotate_1side(dend2, dend1)[[1]]
#' tanglegram(dend1, dend2_corrected) # FIXED.
#' entanglement(dend1, dend2_corrected, L = 2) # 0
#' }
untangle_step_rotate_1side <- function(dend1, dend2_fixed, L = 1.5, direction = c("forward", "backward"),
                                       k_seq = NULL, dend_heights_per_k, leaves_matching_method = c("labels", "order"), ...) {
  # this function gets two dendgrams, and goes over each k splits of the first dend1, and checks if the flip at level k of splitting imporves the entanglement between dend1 and dend2 (Which is fixed)
  n_leaves <- nleaves(dend1)
  best_dend <- dend1
  if (missing(dend_heights_per_k)) dend_heights_per_k <- heights_per_k.dendrogram(best_dend) # since this function takes a looong time, I'm running it here so it will need to run only once!

  leaves_matching_method <- match.arg(leaves_matching_method)
  if (leaves_matching_method == "order") {
    old_dend2_fixed <- dend2_fixed
    dend2_fixed <- match_order_by_labels(old_dend2_fixed, dend1)
    if (!identical(dend2_fixed, old_dend2_fixed) & dendextend_options("warn")) warning("The leaves order in 'dend2_fixed' were changed. If you want to avoid that, use leaves_matching_method = 'labels'.")
  }

  direction <- match.arg(direction)
  if (is.null(k_seq)) {
    # choose step direction:
    if (direction == "backward") {
      k_seq <- n_leaves:2
    } else { # forward
      k_seq <- 2:n_leaves
    }
  }


  for (k in k_seq) {
    dend1_k_rotated <- all_couple_rotations_at_k(best_dend, k, dend_heights_per_k = dend_heights_per_k)
    dend1_cut_k_entanglements <- lapply(dend1_k_rotated, entanglement, dend2 = dend2_fixed, L = L, leaves_matching_method = leaves_matching_method)
    ss_best_dend <- which.min(dend1_cut_k_entanglements)
    current_best_dend <- dend1_k_rotated[[ss_best_dend]]

    # if this loop's best dendro is not identical to our last best dendro - then we should pick it as the new best dendro
    # 		And that means we'll have to update the heights_per_k.dendrogram (which takes time, and we would like to avoid if it is not necessary)
    if (!identical(current_best_dend, best_dend)) {
      best_dend <- current_best_dend
      # We don't need to run the next line twice since the heights per k are the same for any rotated tree...
      #          best_dend_heights_per_k <- heights_per_k.dendrogram(best_dend)
    } # however, if the current dend is just like our best dend - then there is NO NEED to update heights_per_k.dendrogram (and we just saved some time!!)
    # this combination is only useful if we have a tree for which there are only a few rotations which are useful
  }

  return(dendlist(best_dend = best_dend, dend2_fixed = dend2_fixed))
}








#' @title Stepwise untangle two trees one at a time
#' @export
#' @description
#' This is a greedy forward selection algorithm for rotating the tree and
#' looking for a better match.
#'
#' This is useful for finding good trees for a \link{tanglegram}.
#'
#' It goes through rotating dend1, then dend2, and so on - until a locally optimal solution is found.
#'
#' Similar to "step1side", one tree is held fixed and the other tree is rotated.
#' This function goes through all of the k number of clusters (from 2 onward),
#' and each time rotates the branch which was introduced in the new k'th cluster.
#' This rotated tree is compared with the fixed tree, and if it has a better
#' entanglement, it will be used for the following iterations.
#' Once finished the rotated tree is held fixed, and the fixed tree
#' is now rotated. This continues until a local optimal solution is reached.
#'
#' @param dend1 a dendrogram object. The one we will rotate to best fit
#' dend2.
#' @param dend2 a dendrogram object. The one we will rotate to best fit
#' dend1.
#' @param L the distance norm to use for measuring the distance between the
#' two trees. It can be any positive number,
#' often one will want to use 0, 1, 1.5, 2 (see 'details' in \link{entanglement}).
#'
#' @param direction a character scalar, either "forward" (default) or "backward".
#' Impacts the direction of clustering that are tried. Either from 2 and up
#' (in case of "forward"), or from nleaves to down (in case of "backward")
#'
#' If k_seq is not NULL, then it overrides "direction".
#'
#' @param max_n_iterations integer. The maximal number of times to switch between optimizing one tree with another.
#' @param print_times logical (TRUE), should we print how many times we switched between rotating the two trees?
#' @param k_seq a sequence of k clusters to go through for improving
#' dend1. If NULL (default), then we use the "direction" parameter.
#' @param ... not used
#'
#' @return A list with two dendrograms (dend1/dend2),
#' after they are rotated to best fit one another.
#'
#' @seealso \link{tanglegram}, \link{match_order_by_labels},
#' \link{entanglement}, \link{flip_leaves}, \link{all_couple_rotations_at_k}.
#' \link{untangle_step_rotate_1side}.
#' @examples
#'
#' \dontrun{
#' dend1 <- USArrests[1:20, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend2 <- USArrests[1:20, ] %>%
#'   dist() %>%
#'   hclust(method = "single") %>%
#'   as.dendrogram()
#' set.seed(3525)
#' dend2 <- shuffle(dend2)
#' tanglegram(dend1, dend2, margin_inner = 6.5)
#' entanglement(dend1, dend2, L = 2) # 0.79
#'
#' dend2_corrected <- untangle_step_rotate_1side(dend2, dend1)
#' tanglegram(dend1, dend2_corrected, margin_inner = 6.5) # Good.
#' entanglement(dend1, dend2_corrected, L = 2) # 0.0067
#' # it is better, but not perfect. Can we improve it?
#'
#' dend12_corrected <- untangle_step_rotate_2side(dend1, dend2)
#' tanglegram(dend12_corrected[[1]], dend12_corrected[[2]], margin_inner = 6.5) # Better...
#' entanglement(dend12_corrected[[1]], dend12_corrected[[2]], L = 2) # 0.0045
#'
#'
#' # best combination:
#' dend12_corrected_1 <- untangle_random_search(dend1, dend2)
#' dend12_corrected_2 <- untangle_step_rotate_2side(dend12_corrected_1[[1]], dend12_corrected_1[[2]])
#' tanglegram(dend12_corrected_2[[1]], dend12_corrected_2[[2]], margin_inner = 6.5) # Better...
#' entanglement(dend12_corrected_2[[1]], dend12_corrected_2[[2]], L = 2) # 0 - PERFECT.
#' }
untangle_step_rotate_2side <- function(dend1, dend2, L = 1.5, direction = c("forward", "backward"), max_n_iterations = 10L, print_times = dendextend_options("warn"),
                                       k_seq = NULL, ...) {
  # this function gets two dendgrams, and orders dend1 and 2 until a best entengelment is reached.

  direction <- match.arg(direction)

  dend1_heights_per_k <- heights_per_k.dendrogram(dend1)
  dend2_heights_per_k <- heights_per_k.dendrogram(dend2)

  # Next, let's try to improve upon this tree using a forwared rotation of our tree:
  dend1_better <- untangle_step_rotate_1side(dend1, dend2, L = L, dend_heights_per_k = dend1_heights_per_k, direction = direction, k_seq = k_seq)[[1]]
  dend2_better <- untangle_step_rotate_1side(dend2, dend1_better, L = L, dend_heights_per_k = dend2_heights_per_k, direction = direction, k_seq = k_seq)[[1]]

  entanglement_new <- entanglement(dend1_better, dend2_better, L = L)
  entanglement_old <- entanglement_new + 1

  times <- 1

  while (times < max_n_iterations & !identical(entanglement_new, entanglement_old)) { # if we got an improvement from last entaglement, we'll keep going!
    entanglement_old <- entanglement_new

    dend1_better_loop <- untangle_step_rotate_1side(dend1_better, dend2_better,
      L = L,
      dend_heights_per_k = dend1_heights_per_k, direction = direction, k_seq = k_seq
    )[[1]]
    # if the new dend1 is just like we just had - then we can stop the function since we found the best solution - else - continue
    if (identical(dend1_better_loop, dend1_better)) {
      break
    } else {
      dend1_better <- dend1_better_loop
    }

    # if the new dend2 is just like we just had - then we can stop the function since we found the best solution - else - continue
    dend2_better_loop <- untangle_step_rotate_1side(dend2_better, dend1_better,
      L = L,
      dend_heights_per_k = dend2_heights_per_k, direction = direction, k_seq = k_seq
    )[[1]]
    if (identical(dend2_better_loop, dend2_better)) {
      break
    } else {
      dend2_better <- dend2_better_loop
    }

    entanglement_new <- entanglement(dend1_better, dend2_better, L = L)
    times <- times + 1
  }

  # identical(1,1+.00000000000000000000000001) # T
  if (print_times) cat("\nWe ran untangle ", times, " times\n")

  return(dendlist(dend1_better, dend2_better))
}












##### Other attempts which have not
##### proven themselves as useful...






#
# untangle.forward.step.rotate.1side <- function(dend1, dend2_fixed) {
#    # this function gets two dendgrams, and goes over each k splits of the first dend1, and checks if the flip at level k of splitting imporves the entanglement between dend1 and dend2 (Which is fixed)
# 	leaves_order <- order.dendrogram(dend1)
# 	best_dend <- dend1
#
# 	k_visited <- rep(F, length(leaves_order))
# 	k_visited[1] <- T # I don't need the first one
# 	k <- 1
#
# 	while(!all(k_visited)) {
# 		# create all of the rotations with k+-1:
# 		dend1_k_p1_rotated <- all_couple_rotations_at_k(best_dend, k+1)
# 		dend1_k_m1_rotated <- all_couple_rotations_at_k(best_dend, k-1)
# 		# find the enteglement for all of them:
# 		dend1_cut_k_p1_entanglements <- lapply(dend1_k_p1_rotated, entanglement, dend2 = dend2_fixed)
# 		dend1_cut_k_m1_entanglements <- lapply(dend1_k_m1_rotated, entanglement, dend2 = dend2_fixed)
# 		# what is best, forward or backward?
# 		if(min(dend1_cut_k_p1_entanglements) > min(dend1_cut_k_m1_entanglements)) {
#
# 		}
# 		k <- k + 1
# 		ss_best_dend <- which.min(dend1_cut_k_entanglements)
# 		best_dend <- dend1_k_rotated[[ss_best_dend]]
#
# 		all_couple_rotations_at_k(best_dend, -1)
# 	}
#
# 	return(best_dend)
# }


# dend12s_1_better <- untangle_step_rotate_1side(dend1, dend2)
# cutree(dend1, 10)




# evolution algorithm
untangle_intercourse <- function(brother_1_dend1, brother_1_dend2,
                                 sister_2_dend1, sister_2_dend2, L = 1) {
  # Gets two pairs of dend, and returns two childrens (inside a list)
  children_1 <- untangle_step_rotate_2side(brother_1_dend1, sister_2_dend2, L = L)
  children_2 <- untangle_step_rotate_2side(sister_2_dend1, brother_1_dend2, L = L)

  dendlist(children_1, children_2)
}

entanglement_return_best_brother <- function(brother_1_dend1, brother_1_dend2,
                                             brother_2_dend1, brother_2_dend2, L = 1) {
  # Gets two pairs of dend, and returns the pair with the best (minimal) entanglement

  if (entanglement(brother_1_dend1, brother_1_dend2, L = L) <
    entanglement(brother_2_dend1, brother_2_dend2, L = L)) {
    return(dendlist(brother_1_dend1, brother_1_dend2))
  } else {
    return(dendlist(brother_2_dend1, brother_2_dend2))
  }
}

untangle_intercourse_evolution <- function(intercourse, L = 1) {
  # intercourse is a list with two elements.  Each element has two dends
  entanglement_return_best_brother(intercourse[[1]][[1]], intercourse[[1]][[2]],
    intercourse[[2]][[1]], intercourse[[2]][[2]],
    L = L
  )
}


untangle_evolution <- function(brother_1_dend1, brother_1_dend2,
                               sister_2_dend1, sister_2_dend2, L = 1) {
  intercourse <- untangle_intercourse(brother_1_dend1, brother_1_dend2,
    sister_2_dend1, sister_2_dend2,
    L = L
  ) # creates a list with two pairs of dends
  untangle_intercourse_evolution(intercourse, L = L) # returns the best child
}










####
# A new approuch - I will go through every possible flip on one side, and find the one that gives the best improvement.
# I will do the same on each tree, back and forth, until no better flip is found.

untangle_best_k_to_rotate_by_1side <- function(dend1, dend2_fixed, L = 1) {
  # this function gets two dendgrams, and goes over each k splits of the first dend1, and checks if the flip at level k of splitting imporves the entanglement between dend1 and dend2 (Which is fixed)
  leaves_order <- order.dendrogram(dend1)
  best_dend <- dend1
  dend1_k_rotated <- NULL

  best_dend_heights_per_k <- heights_per_k.dendrogram(best_dend) # since this function takes a looong time, I'm running it here so it will need to run only once!
  # this makes the function about twice as fast...

  for (k in 2:length(leaves_order)) {
    dend1_k_rotated <- c(
      dend1_k_rotated,
      all_couple_rotations_at_k(best_dend, k,
        dend_heights_per_k = best_dend_heights_per_k
      )
    )
  }

  dend1_cut_k_entanglements <- lapply(dend1_k_rotated, entanglement, dend2 = dend2_fixed, L = L)
  ss_best_dend <- which.min(dend1_cut_k_entanglements)
  best_dend <- dend1_k_rotated[[ss_best_dend]]
  return(best_dend)
}



flip_1_and_2 <- function(x) {
  ifelse(x == 1, 2, 1)
}

untangle_best_k_to_rotate_by_2side_backNforth <- function(dend1, dend2, times_to_stop = 2, print_times = T, L = 1) {
  # this function gets two dendgrams, and orders dend1 and then 2 and then 1 again - back and forth -until a best entengelment is reached.

  was_improved <- T # e.g: we can improve it further
  counter <- 1

  while (was_improved) {
    entanglement_old <- entanglement(dend1, dend2, L = L)
    dend1 <- untangle_best_k_to_rotate_by_1side(dend1, dend2, L = L)
    dend2 <- untangle_best_k_to_rotate_by_1side(dend2, dend1, L = L)
    entanglement_new <- entanglement(dend1, dend2, L = L)
    was_improved <- identical(entanglement_old, entanglement_new)
    counter <- counter + 1
  }
  # identical(1,1+.00000000000000000000000001) # T
  if (print_times) cat("We ran untangle_best_k_to_rotate_by_2side_backNforth ", counter, " times")

  return(dendlist(dend1, dend2))
}



#
#
# untangle_OLO <- function(dend1, dend2, ...) {
#
#    if(is.dendlist(dend1)) {
#       dend2 <- dend1[[2]]
#       dend1 <- dend1[[1]]
#    }
#
#    # hmap(sqrt(d2), Colv = "none", trace = "none", col = viridis(200))
#    # Error in (function (x, Rowv = TRUE, Colv = if (symm) "Rowv" else TRUE,  :
#    #                       formal argument "Colv" matched by multiple actual arguments
#    # d <- cophenetic(dend2) # doesn't work so great
#
#    vec <- cbind(order.dendrogram(dend1), order.dendrogram(dend2))
#    rownames(vec) <- labels(dend1)[order.dendrogram(dend1)]
#    d <- dist(vec)
#    o <- seriate(d, method = "OLO", control = list(hclust = as.hclust(dend1)) )
#    dend1 <- rotate(dend1, order = rev(labels(d)[get_order(o)]))
#    # library(dendextend)
#    # o <- seriate(d, method = "OLO", control = list(hclust = as.hclust(dend2)) )
#    # dend2 <- rotate(dend2, order = labels(d)[get_order(o)])
#    return(dendlist(dend1, dend2))
# }
#
#
#
# if(F) {
#    ## Not run:
#    require(dendextend)
#    set.seed(23235)
#    ss <- sample(1:150, 10 )
#    dend1 <- iris[ss,-5] %>% dist %>% hclust("com") %>% as.dendrogram
#    dend2 <- iris[ss,-5] %>% dist %>% hclust("sin") %>% as.dendrogram
#    dend12 <- dendlist(dend1, sort(dend2, type = "nodes", decreasing= T))
#    # dend12 <- dendlist(dend1, sort(dend1))
#    dend12 %>% tanglegram
#    dend12_OLO <- untangle_OLO(dend12)
#    dend12_OLO %>% tanglegram
#    dend12_OLO %>% sort(type = "nodes") %>%  tanglegram
#
# }


#
# if(F) {
#    # example
#    dist_DATA <- dist(USArrests[1:20,])
#    # First two dummy clusters (since you didn't provide with some...)
#    hc1 <- hclust(dist_DATA , "single")
#    hc2 <- hclust(dist_DATA , "complete")
#    dend1 <- as.dendrogram(hc1)
#    dend2 <- as.dendrogram(hc2)
#    entanglement(dend1, dend2)
#
#    system.time(dend12_best_01 <- untangle_step_rotate_2side(dend1, dend2, L = 2)) # 0.47 sec
#    system.time(dend12_best_02 <- untangle_best_k_to_rotate_by_2side_backNforth(dend1, dend2, L = 2)) # 0.44 sec
#    tanglegram(dend1, dend2)
#    tanglegram(dend12_best_01[[1]], dend12_best_01[[2]])
#    tanglegram(dend12_best_02[[1]], dend12_best_02[[2]])
# }
#
#
#
#
#
#
#
#
# richrach <- function(x) {
#    # move back and forth between the beginning and the end of a vector
#    c(t(cbind(x, rev(x))))[1:length(x)]
#    # example:
#    # richrach(1:6)
#    # from this:  1 2 3 4 5 6
#    # to this: 1 6 2 5 3 4
# }
#
# richrach_xy <- function(x,y) {
#    # move back and forth between the beginning and the end of a vector
#    c(t(cbind(x, y)))[1:length(x)]
#    # example:
#    # richrach(1:6)
#    # from this:  1 2 3 4 5 6
#    # to this: 1 6 2 5 3 4
# }
#
#
# odd_locations <- function(x) {
#    x[seq(1, length(x), by = 2)]
# }
# # odd_locations(1:6)
#
# #
# #
# # if(FALSE) {
# #
# #    dist_DATA <- dist(USArrests[1:30,])
# #    dist_DATA <- dist(USArrests[1:10,])
# #    # First two dummy clusters (since you didn't provide with some...)
# #    hc1 <- hclust(dist_DATA , "single")
# #    hc2 <- hclust(dist_DATA , "complete")
# #    dend1 <- as.dendrogram(hc1)
# #    dend2 <- as.dendrogram(hc2)
# #
# #    tanglegram(dend1, dend2)
# #    entanglement(dend1, dend2) # 0.8
# #
# #    # after sorting we get a better measure of entanglement and also a better looking plot
# #    tanglegram(sort(dend1), sort(dend2))
# #    entanglement(sort(dend1), sort(dend2)) # 0.1818
# #
# #    # let's cause some shuffle... (e.g: mix the dendrogram, and see how that effects the outcome)
# #    set.seed(134)
# #    s_dend1 <- shuffle(dend1)
# #    s_dend2 <- shuffle(dend2)
# #    tanglegram(s_dend1, s_dend2)
# #    entanglement(s_dend1, s_dend2) # 0.7515
# #
# #
# #    set.seed(1234)
# #    dend12s <- untangle.random.search(dend1, dend2, R = 10)
# #    entanglement(dend12s[[1]], dend12s[[2]]) # 0.042
# #    tanglegram(dend12s[[1]], dend12s[[2]]) #
# #    # this is a case where it is CLEAR that the simplest heuristic would improve this to 0 entanglement...
# #
# #    # let's see if we can reach a good solution using a greedy forward selection algorithm
# #    dend12s_1_better <- untangle_step_rotate_1side(dend12s[[1]], dend12s[[2]])
# #    entanglement(dend12s_1_better, dend12s[[2]]) # from 0.042 to 0.006 !!
# #    tanglegram(dend12s_1_better, dend12s[[2]]) #
# #
# #    # let's see from the beginning
# #    entanglement(dend1, dend2) # 0.6
# #    tanglegram(dend1, dend2) # 0.6
# #    dend12s_1_better <- untangle_step_rotate_1side(dend1, dend2)
# #    entanglement(dend12s_1_better, dend2) # from 0.6 to 0.036
# #    tanglegram(dend12s_1_better, dend2) #
# #    # let's try the other side:
# #    dend12s_2_better <- untangle_step_rotate_1side(dend2, dend12s_1_better)
# #    entanglement(dend12s_1_better, dend12s_2_better) # no improvment
# #
# #
# #
# #    dend2_01 <- untangle_step_rotate_1side(dend2, dend1)
# #    dend2_02 <- untangle.backward.rotate.1side(dend2, dend1)
# #    dend2_03 <- untangle.backward.rotate.1side(dend2_01, dend1)
# #    dend2_04 <- untangle_step_rotate_1side(dend2_02, dend1)
# #    dend2_05 <- untangle_evolution(dend1, dend2 , dend1, dend2_01 )
# #    entanglement(dend1, dend2)
# #    entanglement(dend1, dend2_01)
# #
# #    entanglement(dend1, dend2_02)
# #    entanglement(dend1, dend2_03)
# #    entanglement(dend1, dend2_04)
# #    entanglement(dend2_05[[1]], dend2_05[[2]])
# #    tanglegram(dend1, dend2)
# #    tanglegram(dend1, dend2_01)
# #    tanglegram(dend1, dend2_02)
# #    tanglegram(dend1, dend2_03)
# #    tanglegram(dend1, dend2_04)
# #    tanglegram(dend2_05[[1]], dend2_05[[2]])
# #
# #
# #
# #    entanglement(dend1, dend2)
# #    tanglegram(dend1, dend2)
# #    dend2_01 <- untangle_step_rotate_1side(dend2, dend1)
# #    dend2_01 <- untangle.backward.rotate.1side(dend2, dend1)
# #    tanglegram(dend1, dend2_01)
# #
# #
# #
# #    #
# #    dist_DATA <- dist(USArrests[1:10,])
# #    # First two dummy clusters (since you didn't provide with some...)
# #    hc1 <- hclust(dist_DATA , "single")
# #    hc2 <- hclust(dist_DATA , "complete")
# #    dend1 <- as.dendrogram(hc1)
# #    dend2 <- as.dendrogram(hc2)
# #    dend1_01 <- untangle_step_rotate_1side(dend1, dend2)
# #    entanglement(dend1, dend2)
# #    entanglement(dend1_01, dend2)
# #    tanglegram(dend1, dend2)
# #    tanglegram(dend1_01, dend2)
# #
# #    system.time(dend1_01 <- untangle_step_rotate_1side(dend1, dend2)) # 0.47 sec
# #    system.time(dend1_01 <- untangle.best.k.to.rotate.by(dend1, dend2)) # 0.44 sec
# #    tanglegram(dend1, dend2)
# #    tanglegram(dend1_01, dend2)
# #
# #
# #
# #
# #    #### profiling
# #    library(profr)
# #    slow_dude <- profr(untangle_step_rotate_1side(dend2, dend1))
# #    head(slow_dude)
# #    summary(slow_dude)
# #    plot(slow_dude)
# #
# #    library(reshape)
# #    a <- cast(slow_dude, f~., value="time", fun.aggregate=c(length, sum))
# #    a[order(a[,3]),]
# #    ## End(Not run)
# #    slow_dude[slow_dude$time > .079991, ]
# #
# #
# #    # this also helped:
# #    # 	install.packages("microbenchmark")
# #    library(microbenchmark)
# #
# #    system.time(entanglement(dend1, dend2) 	) # 0.01
# #    microbenchmark( entanglement(dend1, dend2) , times = 10 )# so this is 10 times faster (the medians)
# #    #		betweem 0.011 to 0.038
# #
# # }
# #
# #
# #
# #
# #
# #
# # if(FALSE){
# #
# #    # Finding the BEST tree by going through many random seeds and looking for a good solution :)
# #
# #    entanglement_history <- c()
# #
# #
# #    get.seed <- function(max_lengh = 10e7) round(runif(1)*max_lengh)
# #
# #    best_seed <- 28754448 # 55639690 # 5462457 # 75173309 # 20295644
# #    set.seed(best_seed)
# #    times_a_better_seed_was_found <- 0
# #    random_dendros <- untangle.random.search(yoavs_tree, Dan_arc_tree, R = 1, L = 1.5)
# #    rotated_dendros <- untangle_step_rotate_2side(random_dendros[[1]], random_dendros[[2]], L = 1.5)
# #    best_entanglement <- entanglement(rotated_dendros[[1]], rotated_dendros[[2]], L = 1.5)
# #    # tanglegram(rotated_dendros[[1]], rotated_dendros[[2]])
# #
# #
# #    for(i in 1:100000) {
# #       print(i)
# #       current_seed <- get.seed()
# #       set.seed(current_seed)
# #       random_dendros <- untangle.random.search(yoavs_tree, Dan_arc_tree, R = 10, L = 1.5)
# #       rotated_dendros <- untangle_step_rotate_2side(random_dendros[[1]], random_dendros[[2]], L = 1.5)
# #       new_entanglement <- entanglement(rotated_dendros[[1]], rotated_dendros[[2]], L = 1.5)
# #
# #       entanglement_history <- c(entanglement_history, new_entanglement)
# #
# #       if(new_entanglement < best_entanglement ){
# #          times_a_better_seed_was_found <- times_a_better_seed_was_found + 1
# #          best_seed <- current_seed
# #          print(best_seed)
# #          print(new_entanglement)
# #          best_entanglement <- new_entanglement
# #          tanglegram(rotated_dendros[[1]], rotated_dendros[[2]])
# #       }
# #       flush.console()
# #    }
# #
# #
# #    hist(entanglement_history)
# #
# # }
# #
# #
# #
# #
# #
#
#
#
#
#
#
# # this function is from the combinat package
# # permn <- function (x, fun = NULL, ...)
# # {
# #    if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) ==
# # 				x)
# # 		x <- seq(x)
# # 	n <- length(x)
# # 	nofun <- is.null(fun)
# # 	out <- vector("list", gamma(n + 1))
# # 	p <- ip <- seqn <- 1:n
# # 	d <- rep(-1, n)
# # 	d[1] <- 0
# # 	m <- n + 1
# # 	p <- c(m, p, m)
# # 	i <- 1
# # 	use <- -c(1, n + 2)
# # 	while (m != 1) {
# # 		out[[i]] <- if (nofun)
# # 			x[p[use]]
# # 		else fun(x[p[use]], ...)
# # 		i <- i + 1
# # 		m <- n
# # 		chk <- (p[ip + d + 1] > seqn)
# # 		m <- max(seqn[!chk])
# # 		if (m < n)
# # 			d[(m + 1):n] <- -d[(m + 1):n]
# # 		index1 <- ip[m] + 1
# # 		index2 <- p[index1] <- p[index1 + d[m]]
# # 		p[index1 + d[m]] <- m
# # 		tmp <- ip[index2]
# # 		ip[index2] <- ip[m]
# # 		ip[m] <- tmp
# # 	}
# # 	out
# # }
# #
#
#
# #
# #
# #
# # order.weights.by.cluster.order <- function(weights, cluster_id, new_cluster_order) {
# #    # this function gets a vector of weights. The clusters each weight belongs to
# #    # and a new order for the clusters
# #    # and outputs the new weight vector after ordering the vector by the new order of the cluster (internal order of elements within each cluster is preserved)
# #    output <- NULL
# #    for(i in seq_along(new_cluster_order)) {
# #       output <- c(output, weights[cluster_id == new_cluster_order[i]])
# #    }
# #    return(output)
# # }
# # if(F){
# #    # example:
# #    x = c(1,2,3,4,5,6)
# #    ord1 = c(1,1,2,2,2,3)
# #    ord_of_clusters = c(2,1,3)
# #    c(x[ord1 == ord_of_clusters[1]],x[ord1 == ord_of_clusters[2]],x[ord1 == ord_of_clusters[3]])
# #    order.weights.by.cluster.order(x, ord1, ord_of_clusters)
# # }
#
#
#
#
#
#
