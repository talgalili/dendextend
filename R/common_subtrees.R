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






# Uses distinct_edges to find which nodes are distinct
# And then returns a logical vector indicating for each node 
# if it is one in which it, and all of its children, are nodes
# that contain items that are shared (as a group) with those in dend2
# put differently - these are the nodes that form a sub-tree which will be
# identical (topologically) with some subtree in dend2
nodes_with_shared_labels <- function(dend1, dend2, ...) {
   
   distinct_nodes <- distinct_edges(dend1, dend2)
   good_nodes <- logical(nnodes(dend1))
   i_nodes <- 0
   
   check_if_node_is_good <- function(dend_node)
   {
      i_nodes <<- i_nodes + 1
      current_i_nodes <- i_nodes
      
      if(is.leaf(dend_node)) {
         good_nodes[current_i_nodes] <<- TRUE
         return(TRUE)
      }
      
      is_distinct_node <- i_nodes %in% distinct_nodes
      
      n_children <- length(dend_node)
      are_children_good <- logical(n_children)
      
      for(i in 1:n_children) {
         are_children_good[i] <- check_if_node_is_good(dend_node[[i]])
      }
      
      is_good_node <- all(c(are_children_good, !is_distinct_node))
      if(is_good_node) good_nodes[current_i_nodes] <<- TRUE      
      return(is_good_node)
   }
   
   
   check_if_node_is_good(dend1)
   
   good_nodes
}


# rank_values_with_clustrs(c(1,3,3,3,2,2))
rank_values_with_clustrs <- function(x) {
   rle_lengths <- rle(x)$lengths
   rep(seq_along(rle_lengths), times = rle_lengths)   
}


replace_unique_items_with_0_and_rank <- function(x, ...) {
   # insert zeros
   tbl_x <- table(x)
   unique_char_values <- names(tbl_x)[tbl_x==1]
   ss_unique <- as.character(x) %in% unique_char_values
   x[ss_unique] <- 0
   #    x # this is cleaned
   
   ### # fails:
   ### x[!ss_unique] <- rank(x[!ss_unique], ties = "min")
   
   # now we rank
   x[!ss_unique] <- rank_values_with_clustrs(x[!ss_unique])
   #    rep(1:3, times = c(1,3,3))
   #    rle(c(1,2,2,3,3,4))   
   x
}
# replace_unique_items_with_0_and_rank(c(1,2,2,3,3,4))




#' Find clusters of common subtrees
#' @export
#' @description 
#' Gets a dend and the output from "nodes_with_shared_labels"
#' and returns a vector (length of labels), indicating the clusters
#' forming shared subtrees
#' @param dend1 a \link{dendrogram}.
#' @param dend2 a \link{dendrogram}.
#' @param leaves_get_0_cluster logical (TRUE). Should the leaves which are not part of
#' a larger common subtree get a unique cluster number, or the value 0.
#' @param ... not used.
#'
#' @return
#' An integer vector, with values indicating which leaves in dend1 form
#' a common subtree cluster, with ones available in dend2
#' @seealso  \link{color_branches}, \link{tanglegram}
#' 
#' @examples
#' 
#' 
#' library(dendextend)
#' dend1 <- 1:6 %>% dist %>% hclust %>% as.dendrogram
#' dend2 <- dend1 %>% set("labels", c(1:4,6:5))
#' tanglegram(dend1, dend2)
#' 
#' clusters1 <- common_subtrees_clusters(dend1, dend2)
#' dend1_2 <- color_branches(dend1, clusters = clusters1)
#' plot(dend1_2)
#' plot(dend1_2, horiz = TRUE)
#' tanglegram(dend1_2, dend2, highlight_distinct_edges = FALSE)
#' tanglegram(dend1_2, dend2)
#' 
common_subtrees_clusters <- function(dend1, dend2, leaves_get_0_cluster = TRUE, ...) {
   
   good_nodes <- nodes_with_shared_labels(dend1, dend2)
   dend <- dend1
   
   
   clusters <- numeric(nleaves(dend))   
   cluster_group <- 0
   leaf_id <- 0
   i_nodes <- 0
   
   
   fill_clusters <- function(dend_node)
   {
      i_nodes <<- i_nodes + 1      
      current_node <- i_nodes
      
      if(is.leaf(dend_node)) {
         if(!good_nodes[current_node]) cluster_group <<- cluster_group + 1
         leaf_id <<- leaf_id + 1
         clusters[leaf_id] <<- cluster_group
         return(NULL)
      }
      
      # If not leaf:
      n_children <- length(dend_node)
      
      for(i in 1:n_children) {
         if(!good_nodes[current_node]) cluster_group <<- cluster_group + 1
         fill_clusters(dend_node[[i]])
      }
      
      return(NULL)
   }
   
   fill_clusters(dend)
   
   clusters <- rank_values_with_clustrs(clusters)
   
   if(leaves_get_0_cluster) {
      clusters <- replace_unique_items_with_0_and_rank(clusters)
   }
   
   clusters
   
}













