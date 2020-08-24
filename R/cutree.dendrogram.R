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


#' @title Sort the values level in a vector
#' @export
#' @description
#' Takes a numeric vector and sort its values so that they
#' would be increasing from left to right.
#' It is different from \code{\link{sort}} in that the function
#' will only "sort" the values levels, and not the vector itself.
#'
#' This function is useful for \link[dendextend]{cutree} - making the
#' sort_cluster_numbers parameter possible. Using that parameter with TRUE
#' makes the clusters id's from cutree to be ordered from left to right.
#' e.g: the left most cluster in the tree will be numbered "1", the one
#' after it will be "2" etc...).
#'
#' @param x a numeric vector.
#' @param MARGIN passed to \link{apply}. It is a vector giving the subscripts
#' which the function will be applied over.
#'  E.g., for a matrix 1 indicates rows, 2 indicates columns,
#'  c(1, 2) indicates rows and columns. Where X has named dimnames,
#'  it can be a character vector selecting dimension names.
#' @param decreasing logical (FALSE). Should the sort be increasing or decreasing?
#' @param force_integer logical (FALSE). Should the values returned be integers?
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' (for example when x had NA values in it)
#' @param ... ignored.
#' @return if x is an object - it returns logical - is the object of class dendrogram.
#' @seealso \code{\link{sort}}, \code{\link{fac2num}}, \code{\link[dendextend]{cutree}}
#' @examples
#'
#' x <- 1:4
#' sort_levels_values(x) # 1 2 3 4
#'
#' x <- c(4:1)
#' names(x) <- letters[x]
#' attr(x, "keep_me") <- "a cat"
#' sort_levels_values(x) # 1 2 3 4
#'
#' x <- c(4:1, 4, 2)
#' sort_levels_values(x) # 1 2 3 4 1 3
#'
#' x <- c(2, 2, 3, 2, 1)
#' sort_levels_values(x) # 1 1 2 1 3
#'
#' x <- matrix(16:1, 4, 4)
#' rownames(x) <- letters[1:4]
#' x
#' apply(x, 2, sort_levels_values)
sort_levels_values <- function(x, MARGIN = 2, decreasing = FALSE, force_integer = FALSE, warn = dendextend_options("warn"), ...) {
  if (any(is.na(x))) {
    if (warn) warning("'x' had NA values - it is returned as is.")
    return(x)
  }

  if (!is.numeric(x)) stop("x must be a numeric vector/matrix")

  # make a function that would work on a vector
  sort_levels_values_vec <- function(x) {
    f_x <- factor(x, levels = unique(x))
    levels(f_x) <- sort(as.numeric(levels(f_x)), decreasing = decreasing)
    new_x <- x
    #       force_integer is available in the wrapping function
    new_x[seq_along(new_x)] <- fac2num(f_x, force_integer = force_integer) # this makes sure we retain things like names and attr
    return(new_x)
  }

  if (is.matrix(x)) {
    new_x <- apply(x, MARGIN = MARGIN, sort_levels_values_vec)
  } else {
    new_x <- sort_levels_values_vec(x)
  }

  return(new_x)
}





#' @title Check if numbers are natural
#' @export
#' @description Vectorized function for checking if numbers are natural or not.
#' Helps in checking if a vector is of type "order".
#' @param x a vector of numbers
#' @param tol tolerence to floating point issues.
#' @param ... (not currently in use)
#' @return logical - is the entered number natural or not.
#' @author Marco Gallotta (a.k.a: marcog), Tal Galili
#' @source
#' This function was written by marcog, as an answer to my question here:
#' \url{https://stackoverflow.com/questions/4562257/what-is-the-fastest-way-to-check-if-a-number-is-a-positive-natural-number-in-r}
#' @seealso \code{\link{is.numeric}}, \code{\link{is.double}}, \code{\link{is.integer}}
#' @examples
#' is.natural.number(1) # is TRUE
#' (x <- seq(-1, 5, by = 0.5))
#' is.natural.number(x)
#' # is.natural.number( "a" )
#' all(is.natural.number(x))
is.natural.number <- function(x, tol = .Machine$double.eps^0.5, ...) {
  x > tol & abs(x - round(x)) < tol
}

## Not important enough to include
# all.natural.numbers <- function(x) all(is.natural.number(x))   # check if all the numbers in a vector are natural
# why is this important?
# because it can enable one to check if what we have is a vector of "order"






#' @title cutree for dendrogram (by 1 height only!)
#' @export
#' @description Cuts a dendrogram tree into several groups
#' by specifying the desired cut height (only a single height!).
#' @param dend   a dendrogram object
#' @param h    numeric scalar (NOT a vector) with a height where the dend should be cut.
#' @param use_labels_not_values logical, defaults to TRUE. If the actual labels of the
#' clusters do not matter - and we want to gain speed (say, 10 times faster) -
#' then use FALSE (gives the "leaves order" instead of their labels.).
#' @param order_clusters_as_data logical, defaults to TRUE. There are two ways by which
#' to order the clusters: 1) By the order of the original data. 2) by the order of the
#' labels in the dendrogram. In order to be consistent with \link[stats]{cutree}, this is set
#' to TRUE.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... (not currently in use)
#' @return \code{cutree_1h.dendrogram} returns an integer vector with group memberships
#' @author Tal Galili
#' @seealso \code{\link{hclust}}, \code{\link{cutree}}
#' @examples
#' hc <- hclust(dist(USArrests[c(1, 6, 13, 20, 23), ]), "ave")
#' dend <- as.dendrogram(hc)
#' cutree(hc, h = 50) # on hclust
#' cutree_1h.dendrogram(dend, h = 50) # on a dendrogram
#'
#' labels(dend)
#'
#' # the default (ordered by original data's order)
#' cutree_1h.dendrogram(dend, h = 50, order_clusters_as_data = TRUE)
#'
#' # A different order of labels - order by their order in the tree
#' cutree_1h.dendrogram(dend, h = 50, order_clusters_as_data = FALSE)
#'
#'
#' # make it faster
#' \dontrun{
#' library(microbenchmark)
#' microbenchmark(
#'   cutree_1h.dendrogram(dend, h = 50),
#'   cutree_1h.dendrogram(dend, h = 50, use_labels_not_values = FALSE)
#' )
#' # 0.8 vs 0.6 sec - for 100 runs
#' }
#'
cutree_1h.dendrogram <- function(dend, h,
                                 order_clusters_as_data = TRUE, use_labels_not_values = TRUE,
                                 warn = dendextend_options("warn"), ...) {
  if (missing(h)) stop("h is missing")

  if (length(h) > 1) {
    if (warn) warning("h has length > 1 and only the first element will be used")
    h <- h[1]
  }

  # deal with cases that we cut the dend to all leaves. (negative h)
  if (h < 0) {
    labels_dend <- labels(dend)
    cluster_vec <- 1:length(labels_dend)
    names(cluster_vec)[order.dendrogram(dend)] <- labels_dend
    return(cluster_vec)
  }



  if (use_labels_not_values) {
    FUN <- labels
  } else {
    FUN <- order.dendrogram
  }


  #    cut_replace <- function(dend, h, FUN) {
  #       if(attr(dend, "height") <= h  ) return(list(FUN(dend)))
  #
  #       clusters <- list()
  #       counter <- 1
  #
  #       add_clusters <- function(dend_node)
  #       {
  #          if(is.leaf(dend_node)) {
  #             clusters[[counter]] <<-FUN(dend_node)
  #             return(NULL)
  #          }
  #          for(i in seq(dend_node)) {
  #             dend_node_child_height <- attr(dend_node[[i]], "height")
  #             if(dend_node_child_height>h) { # notice I'm using > and not >= (this might be worth checking further)
  #                if(is.leaf(dend_node[[i]])) {
  #                   clusters[[counter]] <<- FUN(dend_node[[i]])
  #                } else {
  #                   add_clusters(dend_node[[i]])
  #                }
  #             } else { # this node is now a new cluser, hence:
  #                clusters[[counter]] <<- FUN(dend_node[[i]])
  #                counter <<- 1 + counter
  #             }
  #          }
  #          return(NULL)
  #       }
  #       add_clusters(dend)
  #       return(clusters)
  #    }
  # an alternatice which is not faster (but may be used in the future for making this into an Rcpp function!)
  #    names_in_clusters <- cut_replace(dend, h, FUN)

  #    names_in_clusters <- sapply(cut(dend, h = h)$lower, FUN)   # If the proper labels are not important, this function is around 10 times faster than using labels (so it is much better for some other algorithms)
  names_in_clusters <- cut_lower_fun(dend, h, FUN) # If the proper labels are not important, this function is around 10 times faster than using labels (so it is much better for some other algorithms)
  # Type of output:
  #    [[1]]
  #    [1] "Minnesota"
  #
  #    [[2]]
  #    [1] "Maryland" "Colorado" "Alabama"  "Illinois"

  number_of_clusters <- length(names_in_clusters)
  number_of_members_in_clusters <- sapply(names_in_clusters, length) # a list with item per cluster. each item is a character vector with the names of the items in that cluster
  cluster_vec <- rep(rev(seq_len(number_of_clusters)), times = number_of_members_in_clusters) # like in the original cutree
  # I am using "rev" on "seq_len" - so that the resulting cluster numbers will be consistant with those of cutree.hclust

  # 2011-01-10: this is to fix the "bug" (I don't think it's a feature) of having the cut.dendrogram return splitted tree when h is heigher then the tree...
  # now it gives consistent results with cutree
  if (h > attr(dend, "height")) cluster_vec <- rep(1L, length(cluster_vec))

  names(cluster_vec) <- unlist(names_in_clusters)


  # note: The order of the items in cluster_vec, is according to their order in the dendrogram.
  # If the dendrogram was created through as.dendrogram(hclust_object)
  # The original order of the names of the items, from which the hclust (and the dendrogram) object was created from, will not be preserved!

  clusters_order <- order.dendrogram(dend)

  if (order_clusters_as_data) {
    if (!all(clusters_order %in% seq_along(clusters_order))) {
      if (warn) {
        warning("rank() was used for the leaves order number! \nExplenation: leaves tip number (the order), and the ranks of these numbers - are not equal.\n  The dend was probably subsetted, pruned and/or merged with other dends- and now the order \n labels don't make so much sense (hence, the rank on them was used).")
        warning("Here is the cluster order vector (from the dend tips) \n", paste(clusters_order, collapse = ", "), "\n")
      }
      clusters_order <- rank(clusters_order, ties.method = "first") # we use the "first" ties method - to handle the cases of ties in the ranks (after splits/merges with other dends)
    }

    cluster_vec <- cluster_vec[order(clusters_order)] # this reorders the cluster_vec according to the original order of the items from which the dend (maybe hclust) was created
  }

  # 2013-07-28: stay consistant with hclust:
  # if we have as many clusters as items - they should be numbered
  # from left to right...
  dend_size <- nleaves(dend, method = "order")
  if (number_of_clusters == dend_size) cluster_vec[seq_len(dend_size)] <- seq_len(dend_size)

  return(cluster_vec)
}











#' @title Which height will result in which k for a dendrogram
#' @description Which height will result in which k for a dendrogram.
#' This helps with speeding up the \link{cutree.dendrogram} function.
#' @export
#' @aliases
#' dendextend_heights_per_k.dendrogram
#' @param dend a dendrogram.
#' @param ... not used.
#' @return a vector of heights, with its names being the k clusters that will
#' result for cutting the dendrogram at each height.
#'
#' @examples
#' \dontrun{
#' hc <- hclust(dist(USArrests[1:4, ]), "ave")
#' dend <- as.dendrogram(hc)
#' heights_per_k.dendrogram(dend)
#' ##       1        2        3        4
#' ## 86.47086 68.84745 45.98871 28.36531
#'
#' cutree(hc, h = 68.8) # and indeed we get 2 clusters
#'
#' unbranch_dend <- unbranch(dend, 2)
#' plot(unbranch_dend)
#' heights_per_k.dendrogram(unbranch_dend)
#' # 1        3        4
#' # 97.90023 57.41808 16.93594
#' # we do NOT have a height for k=2 because of the tree's structure.
#'
#'
#' library(microbenchmark)
#' dend <- as.dendrogram(hclust(dist(iris[1:150, -5])))
#' dend <- as.dendrogram(hclust(dist(iris[1:30, -5])))
#' dend <- as.dendrogram(hclust(dist(iris[1:3, -5])))
#' microbenchmark(
#'   #    dendextendRcpp::heights_per_k.dendrogram(dend),
#'   dendextendRcpp::dendextendRcpp_heights_per_k.dendrogram(dend),
#'   dendextendRcpp::old_heights_per_k.dendrogram(dend)
#' )
#' # improvment is 10 times faster (in Rcpp) for a tree of size 3
#' # 76 times faster for a tree of size 30
#' # And:
#' # 134 times faster for a tree of size 150!!
#' }
heights_per_k.dendrogram <- function(dend, ...) {
  # gets a dendro tree
  # returns a vector of heights, and the k clusters we'll get for each of them.

  our_dend_heights <- sort(unique(get_branches_heights(dend, sort = FALSE)), TRUE)

  heights_to_remove_for_A_cut <- min(-diff(our_dend_heights)) / 2 # the height to add so to be sure we get a "clear" cut
  heights_to_cut_by <- c(
    (max(our_dend_heights) + heights_to_remove_for_A_cut), # adding the height for 1 clusters only (this is not mandetory and could be different or removed)
    (our_dend_heights - heights_to_remove_for_A_cut)
  )
  # 	names(heights_to_cut_by) <- sapply(heights_to_cut_by, function(h) {length(cut(dend, h = h)$lower)}) # this is the SLOW line - I need to do it differently...
  names(heights_to_cut_by) <- sapply(heights_to_cut_by, function(h) {
    length(cut(dend, h = h)$lower)
  }) # this is the SLOW line - I need to do it differently...


  dend_size <- nleaves(dend)
  # I use "length(heights_to_cut_by)" for cases when I don't have a height for every possible cut
  names(heights_to_cut_by)[length(heights_to_cut_by)] <- as.character(dend_size) # should always be the max...
  names(heights_to_cut_by)[1] <- "1" # should always be 1. (the fact that it's currently not is a bug - remove this line once it is fixed)
  return(heights_to_cut_by)
  # notice we might have certion k's that won't exist in this list!
}






#' @title cutree for dendrogram (by 1 k value only!)
#' @export
#' @description Cuts a dendrogram tree into several groups
#' by specifying the desired number of clusters k (only a single k value!).
#'
#' In case there exists no such k for which exists a relevant split of the
#' dendrogram, a warning is issued to the user, and NA is returned.
#' @param dend   a dendrogram object
#' @param k    numeric scalar (not a vector!) with the number of clusters
#' the tree should be cut into.
#' @param dend_heights_per_k a named vector that resulted from running.
#' \code{heights_per_k.dendrogram}. When running the function many times,
#' supplying this object will help improve the running time.
#' @param use_labels_not_values logical, defaults to TRUE. If the actual labels of the
#' clusters do not matter - and we want to gain speed (say, 10 times faster) -
#' then use FALSE (gives the "leaves order" instead of their labels.).
#' This is passed to \code{cutree_1h.dendrogram}.
#' @param order_clusters_as_data logical, defaults to TRUE. There are two ways by which
#' to order the clusters: 1) By the order of the original data. 2) by the order of the
#' labels in the dendrogram. In order to be consistent with \link[stats]{cutree}, this is set
#' to TRUE.
#' This is passed to \code{cutree_1h.dendrogram}.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' Should the function send a warning in case the desried k is not available?
#' @param ... (not currently in use)
#' @return \code{cutree_1k.dendrogram} returns an integer vector with group
#' memberships.
#'
#' In case there exists no such k for which exists a relevant split of the
#' dendrogram, a warning is issued to the user, and NA is returned.
#' @author Tal Galili
#' @seealso \code{\link{hclust}}, \code{\link{cutree}},
#' \code{\link{cutree_1h.dendrogram}}
#' @examples
#' hc <- hclust(dist(USArrests[c(1, 6, 13, 20, 23), ]), "ave")
#' dend <- as.dendrogram(hc)
#' cutree(hc, k = 3) # on hclust
#' cutree_1k.dendrogram(dend, k = 3) # on a dendrogram
#'
#' labels(dend)
#'
#' # the default (ordered by original data's order)
#' cutree_1k.dendrogram(dend, k = 3, order_clusters_as_data = TRUE)
#'
#' # A different order of labels - order by their order in the tree
#' cutree_1k.dendrogram(dend, k = 3, order_clusters_as_data = FALSE)
#'
#'
#' # make it faster
#' \dontrun{
#' library(microbenchmark)
#' dend_ks <- heights_per_k.dendrogram
#' microbenchmark(
#'   cutree_1k.dendrogram = cutree_1k.dendrogram(dend, k = 4),
#'   cutree_1k.dendrogram_no_labels = cutree_1k.dendrogram(dend,
#'     k = 4, use_labels_not_values = FALSE
#'   ),
#'   cutree_1k.dendrogram_no_labels_per_k = cutree_1k.dendrogram(dend,
#'     k = 4, use_labels_not_values = FALSE,
#'     dend_heights_per_k = dend_ks
#'   )
#' )
#' # the last one is the fastest...
#' }
#'
cutree_1k.dendrogram <- function(dend, k,
                                 dend_heights_per_k = NULL,
                                 use_labels_not_values = TRUE,
                                 order_clusters_as_data = TRUE,
                                 warn = dendextend_options("warn"), ...) {
  #    if(!is.integer(k) && warn) warning("k is not an integer - using k<-as.integer(k)")
  k <- as.integer(k) # making this consistant with cutree.hclust!!

  # STOPING RULES:

  # if k is not natural - stop!
  if (!is.natural.number(k)) stop(paste("k must be a natural number!  The k you used (", k, ") is not a natural number"))

  # if k is "too large" then stop!
  if (k > nleaves(dend)) stop(paste("elements of 'k' must be between 1 and", nleaves(dend)))

  # if k is 1 - it is trivial to run and return:
  if (k == 1L) {
    h_to_use <- attr(dend, "height") + 1
    cluster_vec <- cutree_1h.dendrogram(dend,
      h = h_to_use,
      use_labels_not_values = use_labels_not_values,
      order_clusters_as_data = order_clusters_as_data,
      ...
    )
    return(cluster_vec)
  }

  # deal with cases that we cut the dend to all leaves. (k == nleaves)
  if (k == nleaves(dend)) {
    labels_dend <- labels(dend)
    cluster_vec <- 1:length(labels_dend)
    names(cluster_vec)[order.dendrogram(dend)] <- labels_dend
    return(cluster_vec)
  }



  # step 1: find all possible h cuts for dend
  if (is.null(dend_heights_per_k)) {
    # since this is a step which takes a long time, If possible, I'd rather supply this to the function, so to make sure it runs faster...
    dend_heights_per_k <- heights_per_k.dendrogram(dend)
  }


  # step 2: Check location in the vector of the height for the k we are interested in
  height_for_our_k <- which(names(dend_heights_per_k) == k)
  if (length(height_for_our_k) != 0) # if such a height exists
  {
    h_to_use <- dend_heights_per_k[height_for_our_k]
    cluster_vec <- cutree_1h.dendrogram(dend,
      h = h_to_use,
      use_labels_not_values = use_labels_not_values,
      order_clusters_as_data = order_clusters_as_data,
      ...
    )
    #       if(to_print) print(paste("The dendrogram was cut at height",
    #                                round(h_to_use, 4), "in order to create",k, "clusters."))
  } else {
    cluster_vec <- rep(NA, nleaves(dend, method = "order"))

    # telling the user way he can't use this k
    if (warn) {
      warning("Couldn't cut the dend - returning NA.")

      k_s <- as.numeric(names(dend_heights_per_k))
      # either his k is outside the possible options
      if (k > max(k_s) || k < min(k_s)) {
        range_for_clusters <- paste("[", paste(range(k_s), collapse = "-"), "]", sep = "") # it's always supposed to be between 1 to max number of items (so this could be computed in more efficient ways)
        warning(paste("No cut exists for creating", k, "clusters.  The possible range for clusters is:", range_for_clusters))
      } else {
        warning(paste("You (probably) have some branches with equal heights so that there exist no height(h) that can create", k, " clusters"))
      }
    }
  }
  return(cluster_vec)
}





#' @title Cut a Tree (Dendrogram/hclust/phylo) into Groups of Data
#' @export
#' @description Cuts a dendrogram tree into several groups
#' by specifying the desired number of clusters k(s), or cut height(s).
#'
#' For \code{hclust.dendrogram} -
#' In case there exists no such k for which exists a relevant split of the
#' dendrogram, a warning is issued to the user, and NA is returned.
#' @rdname cutree-methods
#'
#' @param tree   a dendrogram object
#' @param k    numeric scalar (OR a vector) with the number of clusters
#' the tree should be cut into.
#' @param h    numeric scalar (OR a vector) with a height where the tree
#' should be cut.
#' @param dend_heights_per_k a named vector that resulted from running.
#' \code{heights_per_k.dendrogram}. When running the function many times,
#' supplying this object will help improve the running time if using k!=NULL .
#' @param use_labels_not_values logical, defaults to TRUE. If the actual labels of the
#' clusters do not matter - and we want to gain speed (say, 10 times faster) -
#' then use FALSE (gives the "leaves order" instead of their labels.).
#' This is passed to \code{cutree_1h.dendrogram}.
#' @param order_clusters_as_data logical, defaults to TRUE. There are two ways by which
#' to order the clusters: 1) By the order of the original data. 2) by the order of the
#' labels in the dendrogram. In order to be consistent with \link[stats]{cutree}, this is set
#' to TRUE.
#' This is passed to \code{cutree_1h.dendrogram}.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' Should the function send a warning in case the desried k is not available?
#' @param try_cutree_hclust logical. default is TRUE. Since cutree for hclust is
#' MUCH faster than for dendrogram - cutree.dendrogram will first try to change the
#' dendrogram into an hclust object. If it will fail (for example, with unbranched trees),
#' it will continue using the cutree.dendrogram function.
#' If try_cutree_hclust=FALSE, it will force to use cutree.dendrogram and not
#' cutree.hclust.
#' @param NA_to_0L logical. default is TRUE. When no clusters are possible,
#' Should the function return 0 (TRUE, default), or NA (when set to FALSE).
#' @param ... (not currently in use)
#'
#' @details
#' At least one of k or h must be specified, k overrides h if both are given.
#'
#' as opposed to \link[stats]{cutree} for hclust, \code{cutree.dendrogram} allows the
#' cutting of trees at a given height also for non-ultrametric trees
#' (ultrametric tree == a tree with monotone clustering heights).
#'
#' @return
#'
#' If k or h are scalar - \code{cutree.dendrogram} returns an integer vector with group
#' memberships.
#' Otherwise a matrix with group memberships is returned where each column
#' corresponds to the elements of k or h, respectively
#' (which are also used as column names).
#'
#' In case there exists no such k for which exists a relevant split of the
#' dendrogram, a warning is issued to the user, and NA is returned.
#'
#'
#' @author
#' \code{cutree.dendrogram} was written by Tal Galili.
#' \code{cutree.hclust} is redirecting the function
#' to \link[stats]{cutree} from base R.
#'
#' @seealso \code{\link{hclust}}, \code{\link[stats]{cutree}},
#' \code{\link{cutree_1h.dendrogram}}, \code{\link{cutree_1k.dendrogram}},
#'
#' @examples
#'
#' \dontrun{
#' hc <- hclust(dist(USArrests[c(1, 6, 13, 20, 23), ]), "ave")
#' dend <- as.dendrogram(hc)
#' unbranch_dend <- unbranch(dend, 2)
#'
#' cutree(hc, k = 2:4) # on hclust
#' cutree(dend, k = 2:4) # on dendrogram
#'
#' cutree(hc, k = 2) # on hclust
#' cutree(dend, k = 2) # on dendrogram
#'
#' cutree(dend, h = c(20, 25.5, 50, 170))
#' cutree(hc, h = c(20, 25.5, 50, 170))
#'
#' # the default (ordered by original data's order)
#' cutree(dend, k = 2:3, order_clusters_as_data = FALSE)
#' labels(dend)
#'
#' # as.hclust(unbranch_dend) # ERROR - can not do this...
#' cutree(unbranch_dend, k = 2) # all NA's
#' cutree(unbranch_dend, k = 1:4)
#' cutree(unbranch_dend, h = c(20, 25.5, 50, 170))
#' cutree(dend, h = c(20, 25.5, 50, 170))
#'
#'
#' library(microbenchmark)
#' ## this shows how as.hclust is expensive - but still worth it if possible
#' microbenchmark(
#'   cutree(hc, k = 2:4),
#'   cutree(as.hclust(dend), k = 2:4),
#'   cutree(dend, k = 2:4),
#'   cutree(dend, k = 2:4, try_cutree_hclust = FALSE)
#' )
#' # the dendrogram is MUCH slower...
#'
#' # Unit: microseconds
#' ##                       expr      min       lq    median        uq       max neval
#' ##        cutree(hc, k = 2:4)   91.270   96.589   99.3885  107.5075   338.758   100
#' ##    tree(as.hclust(dend),
#' ## 			  k = 2:4)           1701.629 1767.700 1854.4895 2029.1875  8736.591   100
#' ##      cutree(dend, k = 2:4) 1807.456 1869.887 1963.3960 2125.2155  5579.705   100
#' ##  cutree(dend, k = 2:4,
#' ## 	try_cutree_hclust = FALSE) 8393.914 8570.852 8755.3490 9686.7930 14194.790   100
#'
#' # and trying to "hclust" is not expensive (which is nice...)
#' microbenchmark(
#'   cutree_unbranch_dend = cutree(unbranch_dend, k = 2:4),
#'   cutree_unbranch_dend_not_trying_to_hclust =
#'     cutree(unbranch_dend, k = 2:4, try_cutree_hclust = FALSE)
#' )
#'
#'
#' ## Unit: milliseconds
#' ##                   expr      min       lq   median       uq      max neval
#' ## cutree_unbranch_dend       7.309329 7.428314 7.494107 7.752234 17.59581   100
#' ## cutree_unbranch_dend_not
#' ## _trying_to_hclust        6.945375 7.079198 7.148629 7.577536 16.99780   100
#' ## There were 50 or more warnings (use warnings() to see the first 50)
#'
#' # notice that if cutree can't find clusters for the desired k/h, it will produce 0's instead!
#' # (It will produce a warning though...)
#' # This is a different behaviout than stats::cutree
#' # For example:
#' cutree(as.dendrogram(hclust(dist(c(1, 1, 1, 2, 2)))),
#'   k = 5
#' )
#' }
#'
cutree <- function(tree, k = NULL, h = NULL, ...) {
  UseMethod("cutree")
}


#' @export
#' @rdname cutree-methods
cutree.default <- function(tree, k = NULL, h = NULL, ...) {
  cutree(as.dendrogram(tree), k = k, h = h, ...)
  # stop("Function cutree is only available for hclust/dendrogram/phylo objects only.")
}

#' @export
#' @rdname cutree-methods
cutree.hclust <- function(tree, k = NULL, h = NULL,
                          use_labels_not_values = TRUE, # ignored here...
                          order_clusters_as_data = TRUE,
                          warn = dendextend_options("warn"),
                          NA_to_0L = TRUE, # ignored here...
                          ...) {
  sort_cluster_numbers <- TRUE

  ## Add an important warning before R crashes.
  if (warn) {
    if (any(is.na(labels(tree)))) {
      warning("'tree' has NA's in its labels (e.g: labels(tree)) - 
              cutree might crash R.
              If you used as.hclust on a subset of a dendrogram (e.g: dend[[1]]),
              Make sure to first fix the dendrogram's order tips. See:
              help('order.dendrogram<-')
              for suggestion on how to do that.

               (use warn=FALSE if you don't want to see this warning again)
              ")
      #          ANSWER <- menu(c("Yes (continue)", "No (stop)"), graphics = FALSE, title = "Are you sure you want to proceed with cutree?")
      #          if(exists("ANSWER") && ANSWER==2) stop("'cutree' was stopped by the user.")
    }
  }


  clusters <- stats::cutree(tree, k = k, h = h, ...)
  if (!order_clusters_as_data) {
    if (is.matrix(clusters)) {
      clusters <- clusters[tree$order, ]
    } else {
      clusters <- clusters[tree$order]
    }
  }


  # sort the clusters id
  if (sort_cluster_numbers) clusters <- sort_levels_values(clusters, force_integer = TRUE, warn = FALSE)
  # we know that cluster id is an integer, so it is fine to use force_integer = TRUE

  return(clusters)
}














# ' @S3method cutree phylo
#' @export
#' @rdname cutree-methods
cutree.phylo <- function(tree, k = NULL, h = NULL, ...) {
  cutree(as.dendrogram(tree), k = k, h = h, ...)
}


# ' @S3method cutree phylo
#' @export
#' @rdname cutree-methods
cutree.phylo <- function(tree, k = NULL, h = NULL, ...) {
  cutree(as.dendrogram(tree), k = k, h = h, ...)
}

# ' @S3method cutree phylo
#' @export
#' @rdname cutree-methods
cutree.agnes <- function(tree, k = NULL, h = NULL, ...) {
  cutree(as.dendrogram(tree), k = k, h = h, ...)
}
# stats::cutree

#' @export
#' @rdname cutree-methods
cutree.diana <- function(tree, k = NULL, h = NULL, ...) {
  cutree(as.dendrogram(tree), k = k, h = h, ...)
}


# In "cutree.dendrogram" I use "tree" instead of "dend" - in order to stay compatible with stats:cutree


# ' @S3method cutree dendrogram
#' @export
#' @rdname cutree-methods
cutree.dendrogram <- function(tree, k = NULL, h = NULL,
                              dend_heights_per_k = NULL,
                              use_labels_not_values = TRUE,
                              order_clusters_as_data = TRUE,
                              # sort_cluster_numbers = TRUE,
                              warn = dendextend_options("warn"),
                              try_cutree_hclust = TRUE,
                              NA_to_0L = TRUE,
                              ...) {
  sort_cluster_numbers <- TRUE

  # warnings and stopping rules:
  if (!is.dendrogram(tree)) stop("tree should be of class dendrogram (and for some reason - it is not)")
  if (is.null(k) && is.null(h)) stop("Neither k nor h were specified")
  if (!is.null(k) && !is.null(h)) {
    if (warn) warning("Both k and h were specified - using k as default 
                       (consider using only h or k in order to avoid confusions)")
    h <- NULL
  }


  # If it is possible to use cutree.hclust - we will!
  # this would be faster, especially when using k.
  # and if it doesn't, we would fall back on our function
  if (try_cutree_hclust) {

    # Fixed the case when order.dendrogram is not the numbers it should
    # Replacing the order tips with their rank. (otherwise, cutree will CRASH R <= 3.0.1)
    order_tree <- order.dendrogram(tree)
    if (!all(order_tree %in% seq_along(order_tree))) {
      if (warn) {
        warning("rank() was used for the leaves order number! \nExplenation: leaves tip number (the order), and the ranks of these numbers - are not equal.\n  The tree was probably subsetted, pruned and/or merged with other trees- and now the order \n labels don't make so much sense (hence, the rank on them was used).")
        warning("Here is the cluster order vector (from the tree tips) \n", paste(order_tree, collapse = ", "), "\n")
      }
      tree <- rank_order.dendrogram(tree)
      #          order.dendrogram(tree) <- rank(order_tree, ties.method = "first")   # we use the "first" ties method - to handle the cases of ties in the ranks (after splits/merges with other trees)
    }

    # if we succeed (tryCatch) in turning it into hclust - use it!
    # if not - go on with the function.
    hclust_tree <- tryCatch(
      as.hclust(tree),
      error = function(e) FALSE
    )

    if (is.hclust(hclust_tree)) {
      return(cutree(
        tree = hclust_tree, k = k, h = h,
        order_clusters_as_data = order_clusters_as_data,
        # sort_cluster_numbers = sort_cluster_numbers,
        ...
      ))
    }
  }



  if (!is.null(k)) {
    #       cluster_vec <- cutree_1k.dendrogram(tree, k,...) # NO, this would only work for scalars...

    if (is.null(dend_heights_per_k)) {
      # since this is a step which takes a long time, If possible, I'd rather supply this to the function, so to make sure it runs faster...
      dend_heights_per_k <- heights_per_k.dendrogram(tree)
    }
    cutree_per_k <- function(x, tree, ...) cutree_1k.dendrogram(k = x, dend = tree, ...)
    clusters <- sapply(
      X = k, FUN = cutree_per_k,
      tree = tree,
      dend_heights_per_k = dend_heights_per_k,
      use_labels_not_values = use_labels_not_values,
      order_clusters_as_data = order_clusters_as_data,
      warn = warn,
      ...
    )
    colnames(clusters) <- k
  }

  # What to do in case h is supplied
  if (!is.null(h)) {
    #       cluster_vec <- cutree_1h.dendrogram(tree, h,...) # nope...
    cutree_per_h <- function(x, tree, ...) cutree_1h.dendrogram(h = x, dend = tree, ...)
    clusters <- sapply(
      X = h, FUN = cutree_per_h,
      tree = tree,
      use_labels_not_values = use_labels_not_values,
      order_clusters_as_data = order_clusters_as_data,
      warn = warn,
      ...
    )
    colnames(clusters) <- h
  }

  # return a vector if h/k are scalars:
  if (ncol(clusters) == 1) clusters <- clusters[, 1] # make it NOT a matrix

  # sort the clusters id
  if (sort_cluster_numbers) clusters <- sort_levels_values(clusters, force_integer = TRUE, warn = FALSE)
  # we know that cluster id is an integer, so it is fine to use force_integer = TRUE

  if (any(is.na(clusters))) warning("It is impossible to produce a one-to-one cut for the k/h you specidied. 0's have been introduced. Note that this result would be different from R's default cutree output.")

  if (NA_to_0L) clusters[is.na(clusters)] <- 0L

  return(clusters)
}





# if(FALSE) {
# library(dendextend)

# set.seed(23235)
# ss <- sample(1:150, 10 )
# hc1 <- hclust(dist(iris[ss,-5]), "com")
# dend1 <- as.dendrogram(hc1)

## does not give the same results!
# cutree(dend1, k=2:3, order_clusters_as_data=TRUE, try_cutree_hclust=FALSE, sort_cluster_numbers=FALSE)

## this gives the same results as the default:
# cutree.hclust(as.hclust(dend1), k = 3, order_clusters_as_data=TRUE, sort_cluster_numbers=FALSE)
# stats::cutree(as.hclust(dend1), k = 3)
## but not for dendrogram:
# cutree(dend1, k=3, order_clusters_as_data=TRUE, try_cutree_hclust=FALSE, sort_cluster_numbers=TRUE)
# cutree(dend1, k=3, order_clusters_as_data=TRUE, try_cutree_hclust=FALSE, sort_cluster_numbers=FALSE)


## this gives the same results as the default:
# cutree.hclust(as.hclust(dend1), k = 3, order_clusters_as_data=TRUE, sort_cluster_numbers=TRUE)
# stats::cutree(as.hclust(dend1), k = 3)
# stats::cutree(hc1, k = 3)
# cutree.dendrogram(dend1, k=3, order_clusters_as_data=TRUE, try_cutree_hclust=FALSE,
# sort_cluster_numbers=TRUE)


# }









### TODO:
### possible functions to add:
# ultrametric
# is.ultrametric(as.phylo(as.hclust(dend)))
# is.ultrametric(as.phylo(as.hclust(hang.dendrogram(dend))))
# plot(as.phylo(as.hclust(hang.dendrogram(dend))))
# is.ultrametric(as.phylo(as.hclust(dend, hang = 2)))
# is.binary.tree






## ----------------------
## examples:
# hc <- hclust(dist(USArrests[c(1:3,7,5),]), "ave")
# dhc <- as.dendrogram(hc)
# str(dhc)
# plot(hc)

# cutree(dhc, h = 50)
# cutree.dendrogram(dhc, h = 50)
# cutree.dendrogram(dhc, k = 3) # same output
# cutree.dendrogram(dhc, k = 3,h = 50) # conflicting options - using h as default
# cutree.dendrogram(dhc, k = 10) # handaling the case were k is not a viable number of clusters

## showing another case were k is not an option
# attr(dhc[[2]][[1]], "height") <- 23.2
# attr(dhc[[2]][[2]], "height") <- 23.2
# plot(dhc)
# is.ultrametric(as.phylo(dhc))
# cutree.dendrogram(dhc, k = 4) # handaling the case were k is not a viable number of clusters
# cutree.dendrogram(dhc, k = 3.2) # handaling the case were k is not a viable number of clusters


# heights_per_k.dendrogram(dhc)
