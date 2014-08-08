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



#' @title Find lowest common branch were the two items are shared
#' @export
#' @description
#' Given two vectors, for two items, of cluster belonging - the function finds
#' the lowest branch (e.g: largest number of k clusters) for which the two
#' items are in the same cluster for the two trees.
#' @param item1 a named numeric vector (of cluster group with names of k level)
#' @param item2 a named numeric vector (of cluster group with names of k level)
#' @param ... not used
#' @seealso
#' \link{cor_bakers_gamma}
#' @return 
#' The first location (from left) where the two vectors have the same A dendrogram, after adjusting the members attr in all of its nodes.
#' @examples
#' 
#' item1 <- structure(c(1L, 1L, 1L, 1L), .Names = c("1", "2", "3", "4"))
#' item2 <- structure(c(1L, 1L, 2L, 2L), .Names = c("1", "2", "3", "4"))
#' lowest_common_branch(item1, item2)
#' 
lowest_common_branch <- function(item1, item2,...)
{
   # for two rows of cluster belonging,
   # this finds all the cases where the two items are identical (e.g: belong to the same branch)
   # Then it takes the most extreme value (lowest branch) where this happens
   # and finally - it extracts the level of that lowest brnach (names) and turn it to be numeric
   value <- as.numeric(names(tail(which(item1 == item2),1))	)
   if(length(value) == 0) value <- 0
   return(value)
}


#' @title Bakers Gamma for two k matrices
#' @param k_matrix_tree1 a matrix of k cluster groupings from a dendrogram
#' @param k_matrix_tree2 a (second) matrix of k cluster groupings from a dendrogram
#' @param to_plot logical (FALSE). Should a scaterplot be plotted, showing the
#' correlation between the lowest shared branch between two items in the two
#' compared trees.
#' @param ... not used
#' @seealso
#' \link{cor_bakers_gamma}
#' @return 
#' Baker's Gamma coefficient.
bakers_gamma_for_2_k_matrix <- function(k_matrix_tree1, k_matrix_tree2, to_plot = FALSE)
{
   
   if(dim(k_matrix_tree1)[1] != dim(k_matrix_tree2)[1]) stop("The k_matrixes seems to show a different number of items - we can not compare trees in this case!")
   if(!all(sort(rownames(k_matrix_tree1)) == sort(rownames(k_matrix_tree2)))) 
   {		# we are using "sort" since the rownames may be of different order - depending on the way the two trees were constructed.
      print(paste("Item names (rownames) of k_matrix_tree1:" ,rownames(k_matrix_tree1)))
      print(paste("Item names (rownames) of k_matrix_tree2:" ,rownames(k_matrix_tree2)))
      stop("The k_matrixes seems to have different item names - \n we can not compare trees in this case! \n Consider using use_labels_not_values = T (or F) in cutree")
   }
   
   all_combinations_of_items <- t(combn(seq_len(dim(k_matrix_tree1)[1]),2))
   number_of_combinations_of_items <- dim(all_combinations_of_items)[1]
   
   
   cor_mat <- matrix(0, number_of_combinations_of_items, 2)
   
   
   for(i in seq_len(number_of_combinations_of_items))
   {
      item_id_1_name <- rownames(k_matrix_tree1)[all_combinations_of_items[i,1]]
      item_id_2_name <- rownames(k_matrix_tree1)[all_combinations_of_items[i,2]]
      # The names must be identical in both trees - we've made a stopping rule for that already.
      item1 <- k_matrix_tree1[item_id_1_name,]
      item2 <- k_matrix_tree1[item_id_2_name,]
      # print(paste(i, item1, item2))
      cor_mat[i,1] <- lowest_common_branch(item1,  item2)
      
      item1 <- k_matrix_tree2[item_id_1_name,]
      item2 <- k_matrix_tree2[item_id_2_name,]
      cor_mat[i,2] <- lowest_common_branch(item1,  item2)
   }
   
   COR_object <- cor(cor_mat[,1], cor_mat[,2], method = "spearman")
   if(is.na(COR_object)) COR_object <- 1 # because this is NA only if the two vectors are identical
   #  that happens only when the two trees have only leaves. Which is o.k. to define as correlation of 1.
   
   if(to_plot) {
      plot(cor_mat, sub = paste("COR =", round(COR_object,4)))
   }	
   
   return(COR_object)
}








#' @title Baker's Gamma correlation coefficient
#' @export
#' @description
#' Calculate Baker's Gamma correlation coefficient for two trees.
#' 
#' Assumes the labels in the two trees fully match. If they do not
#' please first use \link{intersect_trees} to have them matched.
#' 
#' WARNING: this can be quite slow for medium/large trees.
#' 
#' @param tree1 a tree (dendrogram/hclust/phylo)
#' @param tree2 a tree (dendrogram/hclust/phylo)
#' @param use_labels_not_values logical (TRUE). Should labels be used in the 
#' k matrix when using cutree? Set to FALSE will make the function a bit faster
#' BUT, it assumes the two trees have the exact same leaves order values for 
#' each labels. This can be assured by using \link{match_order_by_labels}.
#' @param to_plot logical (FALSE). Passed to \link{bakers_gamma_for_2_k_matrix}
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' should a warning be issued when using \link[dendextend]{cutree}?
#' @param ... Passed to \link[dendextend]{cutree}.
#' 
#' @details
#' Baker's Gamma (see reference) is a measure of accosiation (similarity) 
#' between two trees of heirarchical clustering (dendrograms).
#' 
#' It is calculated by taking two items, and see what is the heighst
#' possible level of k (number of cluster groups created when cutting the tree)
#' for which the two item still belongs to the same tree. That k is returned, 
#' and the same is done for these two items for the second tree.
#' There are n over 2 combinations of such pairs of items from the items in 
#' the tree, and all of these numbers are calculated for each of the two trees. 
#' Then, these two sets of numbers (a set for the items in each tree)
#' are paired according to the pairs of items compared, and a spearman 
#' correlation is calculated.
#' 
#' The value can range between -1 to 1. With near 0 values meaning that
#' the two trees are not statistically similar.
#' For exact p-value one should result to a permutation test. One such option
#' will be to permute over the labels of one tree many times, and calculating 
#' the distriubtion under the null hypothesis (keeping the trees topologies
#' constant).
#' 
#' Notice that this measure is not affected by the height of a branch but only
#' of its relative position compared with other branches.
#' 
#' @seealso
#' \link{cor_cophenetic}
#' @return 
#' Baker's Gamma association Index between two trees (a number between -1 to 1)
#' 
#' @references
#' 
#' Baker, F. B., Stability of Two Hierarchical Grouping Techniques Case
#'  1: Sensitivity to Data Errors. Journal of the American Statistical 
#'  Association, 69(346), 440 (1974).
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 10 )
#' hc1 <- hclust(dist(iris[ss,-5]), "com")
#' hc2 <- hclust(dist(iris[ss,-5]), "single")
#' dend1 <- as.dendrogram(hc1)
#' dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)   
#' 
#' cor_bakers_gamma(hc1, hc2)
#' cor_bakers_gamma(dend1, dend2)
#' 
#' dend1 <- match_order_by_labels(dend1, dend2) # if you are not sure
#' cor_bakers_gamma(dend1, dend2, use_labels_not_values = FALSE)   
#' 
#' library(microbenchmark)
#' microbenchmark(
#'    with_labels = cor_bakers_gamma(dend1, dend2, try_cutree_hclust=FALSE)   ,
#'    with_values = cor_bakers_gamma(dend1, dend2, 
#'                use_labels_not_values = FALSE, try_cutree_hclust=FALSE)   ,
#'    times=10
#' )
#' 
#' 
#' cor_bakers_gamma(dend1, dend1, use_labels_not_values = FALSE)   
#' cor_bakers_gamma(dend1, dend1, use_labels_not_values = TRUE)   
#' 
#' }
#' 
cor_bakers_gamma <- function(tree1, tree2, use_labels_not_values = TRUE, to_plot = FALSE, warn = dendextend_options("warn"), ...){
   UseMethod("cor_bakers_gamma")
}

#' @export
cor_bakers_gamma.default <- function(tree1, tree2, ...) {
   tree1 <- as.dendrogram(tree1)
   tree2 <- as.dendrogram(tree2)
   cor_bakers_gamma(tree1, tree2 ,...)
}

# ' @S3method cor_bakers_gamma dendrogram
#' @export
cor_bakers_gamma.dendrogram <- function(tree1, tree2, use_labels_not_values = TRUE, to_plot = FALSE, warn = dendextend_options("warn"), ...)
{
   k_matrix_tree1 <- cutree(tree1, k = 1:nleaves(tree1), use_labels_not_values=use_labels_not_values,warn=warn,...)
   k_matrix_tree2 <- cutree(tree2, k = 1:nleaves(tree2), use_labels_not_values=use_labels_not_values,warn=warn,...)
   bakers_gamma <- bakers_gamma_for_2_k_matrix(k_matrix_tree1, k_matrix_tree2, to_plot = to_plot)
   return(bakers_gamma)
}


# ' @S3method cor_bakers_gamma hclust
#' @export
cor_bakers_gamma.hclust <- function(tree1, tree2, use_labels_not_values = TRUE, to_plot = FALSE, warn = dendextend_options("warn"), ...)
{
   k_matrix_tree1 <- cutree(tree1, k = 1:nleaves(tree1), use_labels_not_values=use_labels_not_values,warn=warn,...)
   k_matrix_tree2 <- cutree(tree2, k = 1:nleaves(tree2), use_labels_not_values=use_labels_not_values,warn=warn,...)
   bakers_gamma <- bakers_gamma_for_2_k_matrix(k_matrix_tree1, k_matrix_tree2, to_plot = to_plot)
   return(bakers_gamma)
}



# k_matrix_tree1 <- cutree(hc1, k = 1:nleaves(hc1))
# k_matrix_tree2 <- cutree(hc2, k = 1:nleaves(hc1))
# library(compiler)
# enableJIT(3)
# system.time(bakers_gamma_for_2_k_matrix(k_matrix_tree1, k_matrix_tree2)) 
# before: 2.37
# after: 1.97
