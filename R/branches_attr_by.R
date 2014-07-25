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
#' The HAS to be of the size of the number of leaves.
#' Items that belong to no cluster should get the value 0.
#' The vector should be of the same order as that of the labels in the dendrogram.
#' If you create the clusters from something like \link{cutree} you would first
#' need to use \link{order.dendrogram} on it, before using it in the function.
#' @param values the attributes to use for non 0 values.
#' This should be of the same length as the number of non-0 clusters.
#' If it is not, it is recycled.
#' @param attr a character with one of the following values: col/lwd/lty
#' @param ... ignored.
#' @return 
#' A dendrogram with modified branches (col/lwd/lty).
#' @seealso 
#' \link{branches_attr_by_labels},
#' \link{get_leaves_attr}, \link{nnodes}, \link{nleaves}
#' \link[dynamicTreeCut]{cutreeDynamic}
#' @examples
#' 
#' \dontrun{
#' 
#' ### Getting the hc object
#' hc <- iris[,-5] %>% dist %>% hclust 
#' # This is how it looks without any colors:
#' dend <- as.dendrogram(hc)
#' plot(dend)
#' 
#' # Both functions give the same outcome
#' # options 1:
#' dend %>% set("branches_k_color", k = 4) %>% plot
#' # options 2:
#' clusters <- cutree(dend, 4)[order.dendrogram(dend)]
#' dend %>% branches_attr_by_clusters(clusters) %>% plot
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
#' require(dynamicTreeCut)
#' clusters <- cutreeDynamic(hc)
#' # we need to sort them to the order of the dendrogram:
#' clusters <- clusters[order.dendrogram(dend)]
#' 
#' # get some functions:
#' require(dendextendRcpp)
#' require(colorspace)
#' no0_unique <- function(x) {
#'    u_x <- unique(x)   
#'    u_x[u_x != 0]
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
#' tmp_cols  <- rep(1, length(clusters))
#' tmp_cols[clusters != 0] <- ord_cols[clusters != 0][clusters]
#' colored_bars(tmp_cols, y_shift = -1.1, rowLabels = "")
#' # all of the ordering is to handle the fact that the cluster numbers are not ascending...
#' 
#' # How is this compared with the usual cutree?
#' dend3 <- color_branches(dend, k = 4)
#' labels(dend2) <- as.character(labels(dend2)) # this needs fixing, since the labels are not character!
#' # Well, both cluster solutions are not at all good, but at least they are interesting...
#' tanglegram(dend2, dend3, 
#'            columns_width = c(5,.5,5), 
#'            color_lines = cols[iris[order.dendrogram(dend2),5]])
#' # (Notice how the color_lines is of the true Species of each Iris)
#' 
#' }
branches_attr_by_clusters <- function(dend, clusters, values, attr = c("col", "lwd", "lty"),  ...) {
   attr <- match.arg(attr)
   
   if(!is.dendrogram(dend)) warning("'dend' should be a dendrogram.")   
   if(missing(clusters)) stop("'clusters' parameter is missing.")
   if(!is.numeric(clusters)) {      
      warning("'clusters' parameter was not a numeric vector, and was coerced into one.")
      clusters <- as.numeric(clusters)
   }
   
   if(nleaves(dend) != length(clusters)) stop("The number of values in 'clusters' does not equal the number of leaves.")
   
   u_clusters <- unique(clusters)
   u_clusters <- u_clusters[u_clusters != 0] # we only care about the unique clusters which are not 0!
#    u_clusters <- sort(u_clusters) # and let's sort the clusters so that we would go 
   n_clusters <- length(u_clusters)
   
   if(missing(values) & attr == "col" ) {
      values <- rep(1, length(clusters)) # make a vector of black colors
      require(colorspace) # this might need to be fixed later for getting no error in checks.
      values <- rainbow_hcl(n_clusters)
   }
   
   
   if(length(values) < n_clusters) {
      #          stop("Must give the same number of colors as number of clusters")
      warning("Length of values vector was shorter than the number of clusters (Excluding 0) - vector was recycled")
      values <- rep(values, length.out = n_clusters)
   }
   

   # let's find out which nodes we should modify for each cluster
   nodes_cluster_TF_mat <- matrix(FALSE, nrow = nnodes(dend), ncol = n_clusters)
   dend_labels <- labels(dend)
   has_any_labels <- function(sub_dend, the_labels) any(labels(sub_dend) %in% the_labels)
   for(i in seq_along(u_clusters)) {
      # looking at the labels of the current cluster:
      ss <- clusters == u_clusters[i]
      tmp_labels <- dend_labels[ss]
      # find which node belongs to it:
      tmp_nodes_TF <- noded_with_condition(dend, has_any_labels, the_labels = tmp_labels)      
      # and modify our TF cluster matrix:
      nodes_cluster_TF_mat[,i] <- tmp_nodes_TF
   }

   # let's find all the places where we should NOT make any modification:
   nodes_cluster_TF_mat_overlap <- apply(nodes_cluster_TF_mat, 1, function(x) {sum(x) > 1})
   
   # no we can go through all of the clusters and modify the dend as we should:
   for(i in seq_along(u_clusters)) {
      # set tmp values to be the cluster value (for relevant nodes) or NA (for the rest):
      tmp_values <- ifelse(nodes_cluster_TF_mat[,i], values[i], NA)
      # but if we have an overlap, let's set values back to NA!
      tmp_values <- ifelse(nodes_cluster_TF_mat_overlap, NA, tmp_values)
      
      # update the dend:
      dend <- assign_values_to_branches_edgePar(object = dend, value = tmp_values, edgePar = attr)     
   }
   
   return(dend)
}
