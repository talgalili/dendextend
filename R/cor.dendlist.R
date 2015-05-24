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





#' @title Correlation matrix between a list of trees.
#' @export
#' @description
#' A correlation matrix between a list of trees.
#' 
#' Assumes the labels in the two trees fully match. If they do not
#' please first use \link{intersect_trees} to have them matched.
#' 
#' @param x a \link{dendlist} of trees
#' @param method a character string indicating which correlation coefficient 
#' is to be computed. One of "cophenetic" (default),  "baker", or "common_nodes".
#' It can be abbreviated. 
#' @param ... Ignored.
#' 
#' @seealso
#' \link{cophenetic}, \link{cor_cophenetic}, \link{cor_bakers_gamma}
#' @return 
#' A correlation matrix between the different trees
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 10 )
#' dend1 <- iris[ss,-5] %>% dist %>% hclust("com") %>% as.dendrogram
#' dend2 <- iris[ss,-5] %>% dist %>% hclust("single") %>% as.dendrogram
#' dend3 <- iris[ss,-5] %>% dist %>% hclust("ave") %>% as.dendrogram
#' dend4 <- iris[ss,-5] %>% dist %>% hclust("centroid") %>% as.dendrogram
#' #    cutree(dend1)   
#' cors <- cor.dendlist(dendlist(d1 = dend1, d2 = dend2, d3 = dend3, d4 = dend4))
#' 
#' cors
#' 
#' # a nice plot for them:
#' library(corrplot)
#' corrplot(cor.dendlist(dend1234), "pie", "lower")
#' 
#' }
cor.dendlist <- function(x, method = c("cophenetic", "baker",  "common_nodes"), ...) {
   if(!is.dendlist(x)) stop("x needs to be a dendlist object")
   method <- match.arg(method)
   
   n_list <- length(x)
   the_cor <- matrix(1, n_list, n_list)
   pairwise_combn <- combn(n_list, 2)
   
   for(i in 1:ncol(pairwise_combn)) {      
      l1 <- pairwise_combn[1,i]
      l2 <-pairwise_combn[2,i]
      the_cor[l1, l2] <- the_cor[l2, l1] <- 
         switch(method, 
                cophenetic = cor_cophenetic(x[[l1]], x[[l2]]),
                baker = cor_bakers_gamma(x[[l1]], x[[l2]]),
                common_nodes = cor_common_nodes(x[[l1]], x[[l2]])
         )
      
   }
   
   rownames(the_cor) <- colnames(the_cor) <- names(x)
   
   the_cor
}




# edgeset_dist


#' Proportion of commong nodes between two trees
#' @export
#' @description 
#' Calculates the number of nodes, in each tree, that are common (i.e.: that have the same exact list of labels).
#' The correlation is between 0 (actually, 2*(nnodes-1)/(2*nnodes), for two trees with 
#' the same list of labels - since the top node will always be identical for them).
#' Where 1 means that every node in the one tree, has a node in the other tree with the exact
#' same list of labels.
#' Notice this measure is non-parameteric (it ignores the heights and relative position of the nodes).
#' 
#' @param tree1 a dendrogram.
#' @param tree2 a dendrogram.
#' @param ... not used.
#'
#' @return
#' A correlation value between 0 to 1 (almost identical trees)
#' @seealso \link{distinct_edges}, \link{cor.dendlist}
#'
#' @examples
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 10 )
#' hc1 <- iris[ss,-5] %>% dist %>% hclust("com")
#' hc2 <- iris[ss,-5] %>% dist %>% hclust("single")
#' dend1 <- as.dendrogram(hc1)
#' dend2 <- as.dendrogram(hc2)
#' 
#' cor_cophenetic(dend1, dend2)
#' cor_common_nodes(dend1, dend2)
#' tanglegram(dend1, dend2) 
#' # we can see we have only two nodes which are different...
#' 
cor_common_nodes <- function(tree1, tree2, ...) {
   # dendextend:::edgeset_dist
   n_diff_nodes <- edgeset_dist(tree1, tree2)
   nnodes_trees <- nnodes(tree1) + nnodes(tree2)
   
   (nnodes_trees - n_diff_nodes)/ nnodes_trees
}






