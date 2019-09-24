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


# debug(branches_attr_by_clusters)




#' @title Change col/lwd/lty of branches based on clusters
#' @export
#' @description
#' The user supplies a dend, a vector of clusters, and what to modify (and how).
#'
#' And the function returns a dendrogram with branches col/lwd/lty accordingly.
#' (the function assumes unique labels)
#'
#' @details
#'
#' This is probably NOT a very fast implementation of the function, but it works.
#'
#' This function was designed to enable the manipulation (mainly coloring) of
#' branches, based on the results from the \link[dynamicTreeCut]{cutreeDynamic}
#' function.
#'
#' @param dend a dendrogram dend
#' @param clusters an integer vector of clusters.
#' This HAS to be of the same length as the number of leaves.
#' Items that belong to no cluster should get the value 0.
#' The vector should be of the same order as that of the labels in the dendrogram.
#' If you create the clusters from something like \link{cutree} you would first
#' need to use \link{order.dendrogram} on it, before using it in the function.
#' @param values the attributes to use for non 0 values.
#' This should be of the same length as the number of unique non-0 clusters.
#' If it is shorter, it is recycled.
#'
#' OR, this can also be of the same length as the number of leaves in the tree
#' In which case, the values will be aggreagted (i.e.: \link{tapply}), to match
#' the number of clusters. The first value of each cluster will be used as the main
#' value.
#'
#' TODO: So far, the function doesn't deal well with NA values. (this might be changed in the future)
#'
#' @param attr a character with one of the following values: col/lwd/lty
#' @param branches_changed_have_which_labels character with either "any" (default) or "all".
#' Inidicates how the branches should be updated.
#' @param ... ignored.
#'
#' @return
#' A dendrogram with modified branches (col/lwd/lty).
#' @seealso
#' \link{branches_attr_by_labels},
#' \link{get_leaves_attr}, \link{nnodes}, \link{nleaves}
#' \link[dynamicTreeCut]{cutreeDynamic},
#' \link[WGCNA]{plotDendroAndColors}
#' @examples
#'
#' \dontrun{
#'
#' ### Getting the hc object
#' iris_dist <- iris[, -5] %>% dist()
#' hc <- iris_dist %>% hclust()
#' # This is how it looks without any colors:
#' dend <- as.dendrogram(hc)
#' plot(dend)
#'
#' # Both functions give the same outcome
#' # options 1:
#' dend %>%
#'   set("branches_k_color", k = 4) %>%
#'   plot()
#' # options 2:
#' clusters <- cutree(dend, 4)[order.dendrogram(dend)]
#' dend %>%
#'   branches_attr_by_clusters(clusters) %>%
#'   plot()
#'
#' # and the second option is much slower:
#' system.time(set(dend, "branches_k_color", k = 4)) # 0.26 sec
#' system.time(branches_attr_by_clusters(dend, clusters)) # 1.61 sec
#' # BUT, it also allows us to do more flaxible things!
#'
#' #--------------------------
#' #   Plotting dynamicTreeCut
#' #--------------------------
#'
#' # let's get the clusters
#' library(dynamicTreeCut)
#' clusters <- cutreeDynamic(hc, distM = as.matrix(iris_dist))
#' # we need to sort them to the order of the dendrogram:
#' clusters <- clusters[order.dendrogram(dend)]
#'
#' # get some functions:
#' library(dendextendRcpp)
#' library(colorspace)
#' no0_unique <- function(x) {
#'   u_x <- unique(x)
#'   u_x[u_x != 0]
#' }
#'
#' clusters_numbers <- no0_unique(clusters)
#' n_clusters <- length(clusters_numbers)
#' cols <- rainbow_hcl(n_clusters)
#' dend2 <- branches_attr_by_clusters(dend, clusters, values = cols)
#' # dend2 <- branches_attr_by_clusters(dend, clusters)
#' plot(dend2)
#' # add colored bars:
#' ord_cols <- rainbow_hcl(n_clusters)[order(clusters_numbers)]
#' tmp_cols <- rep(1, length(clusters))
#' tmp_cols[clusters != 0] <- ord_cols[clusters != 0][clusters]
#' colored_bars(tmp_cols, y_shift = -1.1, rowLabels = "")
#' # all of the ordering is to handle the fact that the cluster numbers are not ascending...
#'
#' # How is this compared with the usual cutree?
#' dend3 <- color_branches(dend, k = n_clusters)
#' labels(dend2) <- as.character(labels(dend2))
#' # this needs fixing, since the labels are not character!
#' # Well, both cluster solutions are not perfect, but at least they are interesting...
#' tanglegram(dend2, dend3,
#'   main_left = "cutreeDynamic", main_right = "cutree",
#'   columns_width = c(5, .5, 5),
#'   color_lines = cols[iris[order.dendrogram(dend2), 5]]
#' )
#' # (Notice how the color_lines is of the true Species of each Iris)
#' # The main difference is at the bottom,
#' }
branches_attr_by_clusters <- function(dend, clusters, values, attr = c("col", "lwd", "lty"),
                                      branches_changed_have_which_labels = c("any", "all"),
                                      ...) {
  attr <- match.arg(attr)

  if (!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")
  if (missing(clusters)) stop("'clusters' parameter is missing.")
  if (!is.numeric(clusters)) {
    warning("'clusters' parameter was not a numeric vector, and was coerced into one.")
    clusters <- as.numeric(clusters)
  }

  n_leaves <- nleaves(dend)

  if (n_leaves != length(clusters)) stop("The number of values in 'clusters' does not equal the number of leaves.")

  u_clusters <- unique(clusters)
  u_clusters <- u_clusters[u_clusters != 0] # we only care about the unique clusters which are not 0!
  #    u_clusters <- sort(u_clusters) # and let's sort the clusters so that we would go
  n_u_clusters <- length(u_clusters)

  if (missing(values) & attr == "col") {
    values <- rep(1, length(clusters)) # make a vector of black colors
    # library(colorspace) # this package is now in imports
    values <- rainbow_fun(n_u_clusters)
  }


  n_values <- length(values)

  # if the value is of the length of the leaves
  # then we aggregate the values to have one value for each cluster!
  if (n_values == n_leaves) {
    ss_not0 <- clusters != 0
    values <- tapply(values[ss_not0], clusters[ss_not0], function(x) {
      x[1]
    })
  }

  if (n_values < n_u_clusters) {
    #          stop("Must give the same number of colors as number of clusters")
    warning("Length of values vector was shorter than the number of clusters (Excluding 0) - vector was recycled")
    values <- rep(values, length.out = n_u_clusters)
  }

  if (any(is.na(values))) {
    warning("There are NA's in the colors used by branches_attr_by_clusters. This probably means a bug somewhere. The color was replaced by 'black', but make sure your code does what you wanted it to...")
    values[is.na(values)] <- "black"
  }

  # let's find out which nodes we should modify for each cluster
  nodes_cluster_TF_mat <- matrix(FALSE, nrow = nnodes(dend), ncol = n_u_clusters)
  dend_labels <- labels(dend)

  has_any_labels <- function(sub_dend, the_labels) any(labels(sub_dend) %in% the_labels)
  has_all_labels <- function(sub_dend, the_labels) all(labels(sub_dend) %in% the_labels)

  branches_changed_have_which_labels <- match.arg(branches_changed_have_which_labels)
  #### This doesn't seem to work. Not sure why...
  has_x_labels <- switch(branches_changed_have_which_labels,
    all = has_all_labels,
    any = has_any_labels
  )
  #    if(branches_changed_have_which_labels == "any") has_x_labels <- has_any_labels
  #    if(branches_changed_have_which_labels == "all") has_x_labels <- has_all_labels


  for (i in seq_along(u_clusters)) {
    # looking at the labels of the current cluster:
    ss <- clusters == u_clusters[i]
    tmp_labels <- dend_labels[ss]
    # find which node belongs to it:

    #### DELETE:
    # #       tmp_nodes_TF <- noded_with_condition(dend, condition = has_x_labels,
    # #                                            the_labels = tmp_labels)
    #       if(branches_changed_have_which_labels == "any") {
    #          tmp_nodes_TF <- noded_with_condition(dend, condition = has_any_labels, the_labels = tmp_labels)
    #       }
    #       if(branches_changed_have_which_labels == "all") {
    #          tmp_nodes_TF <- noded_with_condition(dend, condition = has_all_labels, the_labels = tmp_labels)
    #       }

    tmp_nodes_TF <- noded_with_condition(dend, condition = has_x_labels, the_labels = tmp_labels)


    # and modify our TF cluster matrix:
    nodes_cluster_TF_mat[, i] <- tmp_nodes_TF
  }

  # let's find all the places where we should NOT make any modification:
  nodes_cluster_TF_mat_overlap <- apply(nodes_cluster_TF_mat, 1, function(x) {
    sum(x) > 1
  })

  # no we can go through all of the clusters and modify the dend as we should:
  for (i in seq_along(u_clusters)) {
    # set tmp values to be the cluster value (for relevant nodes) or Inf (for the rest):
    tmp_values <- ifelse(nodes_cluster_TF_mat[, i], values[i], Inf)
    # but if we have an overlap, let's set values back to Inf!
    tmp_values <- ifelse(nodes_cluster_TF_mat_overlap, Inf, tmp_values)

    # update the dend:
    dend <- assign_values_to_branches_edgePar(dend, value = tmp_values, edgePar = attr)
  }

  return(dend)
}














#' @title Change col/lwd/lty of branches matching labels condition
#' @export
#' @description
#' The user supplies a dend, labels, and type of condition (all/any), and TF_values
#' And the function returns a dendrogram with branches col/lwd/lty accordingly
#' @param dend a dendrogram dend
#' @param labels a character vector of labels from the tree
#' @param TF_values a two dimensional vector with the TF_values to use in case a branch fulfills the condition (TRUE)
#' and in the case that it does not (FALSE). Defaults are 2/Inf for col, lwd and lty.
#' (so it will insert the first value, and will not change all the FALSE cases)
#' @param attr a character with one of the following values: col/lwd/lty
#' @param type a character vector of either "all" or "any", indicating which of
#' the branches should be painted: ones that all of their labels belong to the supplied labels,
#' or also ones that even some of their labels are included in the labels vector.
#' @param ... ignored.
#' @return
#' A dendrogram with modified branches (col/lwd/lty).
#' @seealso \link{noded_with_condition}, \link{get_leaves_attr}, \link{nnodes}, \link{nleaves}
#' @examples
#' \dontrun{
#'
#' library(dendextend)
#'
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#'
#' # Getting the dend dend
#' dend <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend %>% plot()
#'
#' dend %>%
#'   branches_attr_by_labels(c("123", "126", "23", "29")) %>%
#'   plot()
#' dend %>%
#'   branches_attr_by_labels(c("123", "126", "23", "29"), "all") %>%
#'   plot() # the same as above
#' dend %>%
#'   branches_attr_by_labels(c("123", "126", "23", "29"), "any") %>%
#'   plot()
#'
#' dend %>%
#'   branches_attr_by_labels(
#'     c("123", "126", "23", "29"),
#'     "any", "col", c("blue", "red")
#'   ) %>%
#'   plot()
#' dend %>%
#'   branches_attr_by_labels(
#'     c("123", "126", "23", "29"),
#'     "any", "lwd", c(4, 1)
#'   ) %>%
#'   plot()
#' dend %>%
#'   branches_attr_by_labels(
#'     c("123", "126", "23", "29"),
#'     "any", "lty", c(2, 1)
#'   ) %>%
#'   plot()
#' }
branches_attr_by_labels <- function(dend, labels, TF_values = c(2, Inf), attr = c("col", "lwd", "lty"), type = c("all", "any"), ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")
  if (missing(labels)) stop("'labels' parameter is missing.")
  if (!is.character(labels)) {
    if (dendextend_options("warn")) warning("'labels' parameter was not a character vector, and was coerced into one.")
    labels <- as.character(labels)
  }


  type <- match.arg(type)
  attr <- match.arg(attr)

  # make sure that if the TF_values has only one value,
  # the other one will be Inf
  if (length(TF_values) == 1) TF_values <- c(TF_values, Inf)

  # deal with the case we have labels not included in the tree dend:
  labels_in_dend <- labels %in% labels(dend)
  if (!all(labels_in_dend)) {
    warning(
      "Not all of the labels you provided are included in the dendrogram.\n",
      "The following labels were omitted:", labels[!labels_in_dend]
    )
    #       cat("\n")
    labels <- labels[labels_in_dend]
  }

  has_any_labels <- function(sub_dend, the_labels) any(labels(sub_dend) %in% the_labels)
  has_all_labels <- function(sub_dend, the_labels) all(labels(sub_dend) %in% the_labels)

  what_to_change <- switch(type,
    all = noded_with_condition(dend, has_all_labels, the_labels = labels),
    any = noded_with_condition(dend, has_any_labels, the_labels = labels)
  )
  the_TF_values <- ifelse(what_to_change, TF_values[1], TF_values[2])
  # warnings()


  switch(attr,
    col = set(dend, "branches_col", the_TF_values),
    lwd = set(dend, "branches_lwd", the_TF_values),
    lty = set(dend, "branches_lty", the_TF_values)
  )
}








## provided by Manuela Hummel (m.hummel@dkfz.de)
# code partly taken from the 'globaltest' package (Jelle Goeman, Jan Oosting)

#' @title Change col/lwd/lty of branches from the root down to clusters defined by list of labels of respective members
#' @export
#' @description
#' The user supplies a dend, lists, and type of condition (all/any), and TF_values
#' And the function returns a dendrogram with branches col/lwd/lty accordingly
#' @param dend a dendrogram dend
#' @param lists a list where each element contains the labels of members in selected nodes
#' down to which the branches shall be adapted
#' @param TF_values a two dimensional vector with the TF_values to use in case a branch fulfills the condition (TRUE)
#' and in the case that it does not (FALSE). Defaults are 2/1 for col, lwd and lty.
#' (so it will insert the first value, and will not change all the FALSE cases)
#' @param attr a character with one of the following values: col/lwd/lty
#' @param ... ignored.
#' @return
#' A dendrogram with modified branches (col/lwd/lty).
#' @seealso \link{branches_attr_by_labels}
#' @examples
#' \dontrun{
#'
#' library(dendextend)
#'
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#'
#' # Getting the dend dend
#' dend <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend %>% plot()
#'
#' # define a list of nodes
#' L <- list(c("109", "123", "126", "145"), "29", c("59", "67", "97"))
#' dend %>%
#'   branches_attr_by_lists(L) %>%
#'   plot()
#'
#' # choose different color, and also change lwd and lty
#' dend %>%
#'   branches_attr_by_lists(L, TF_value = "blue") %>%
#'   branches_attr_by_lists(L, attr = "lwd", TF_value = 4) %>%
#'   branches_attr_by_lists(L, attr = "lty", TF_value = 3) %>%
#'   plot()
#' }
branches_attr_by_lists <- function(dend, lists, TF_values = c(2, 1), attr = c("col", "lwd", "lty"), ...) {
  if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")
  if (missing(lists)) stop("'lists' parameter is missing.")

  attr <- match.arg(attr)
  if (length(TF_values) == 1) TF_values <- c(TF_values, 1)
  uit <- dend

  # get nodes down to which attribute shall be changed
  sig <- any(sapply(lists, function(x) all(x %in% labels(uit))))

  attr(uit, "edgePar") <- switch(attr,
    col = c(attr(uit, "edgePar"), list(col = ifelse(sig, TF_values[1], TF_values[2]))),
    lwd = c(attr(uit, "edgePar"), list(lwd = ifelse(sig, TF_values[1], TF_values[2]))),
    lty = c(attr(uit, "edgePar"), list(lty = ifelse(sig, TF_values[1], TF_values[2])))
  )

  # continue with the child branches
  if (!is.leaf(dend)) {
    select.branch <- 1:length(dend)
    for (i in 1:length(select.branch)) {
      uit[[i]] <- Recall(dend[[select.branch[i]]], lists, TF_values, attr)
    }
  }
  return(uit)
}
