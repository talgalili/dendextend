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


# Re-coding phylo.diff from the distory package, but for dendrograms
# http://cran.r-project.org/package=distory




#' @title A list with labels for each subtree (edge)
#' @export
#' @description
#' Returns the set of all bipartitions from all edges, that is: a list with the labels
#' for each of the nodes in the dendrogram.
#' @param x a dendrogram
#' @param ... Ignored.
#' 
#' @seealso
#' \link{distinct_edges}, \link{highlight_distinct_edges},
#' \link{dist.dendlist}, \link{tanglegram},
#' \link[distory]{partition.leaves}
#' 
#' @return 
#' A list with the labels for each of the nodes in the dendrogram.
#' 
#' @source
#' A \link{dendrogram} implementation for \link[distory]{partition.leaves} from the {distory} package
#' 
#' @examples
#' 
#' x <- 1:3 %>% dist %>% hclust %>% as.dendrogram
#' plot(x)
#' partition_leaves(x)
#' 
#' \dontrun{
#' set.seed(23235)
#' ss <- sample(1:150, 10 )
#' dend1 <- iris[ss,-5] %>% dist %>% hclust("com") %>% as.dendrogram
#' dend2 <- iris[ss,-5] %>% dist %>% hclust("single") %>% as.dendrogram
#' 
#' partition_leaves(dend1)
#' partition_leaves(dend2)
#' }
#' 
partition_leaves <- function(x, ...) {
   if (!is.dendrogram(x)) stop("'x' is not a dendrogram")
   
   nodes_labels <- vector("list", length = nnodes(x))
   
   i_counter <- 0
   push_node_labels <- function(dend_node) {
      i_counter <<- i_counter + 1
      
      nodes_labels[[i_counter]] <<- labels(dend_node)
      return(NULL)
   }   
   dendrapply(x, push_node_labels) 
   
   return(nodes_labels)
}





#' @title Finds the edges present in the first tree but not in the second
#' @export
#' 
#' @param x a dendrogram to find unique edges in
#' @param y a dendrogram to compare with
#' @param ... Ignored.
#' 
#' @seealso
#' \link{distinct_edges}, \link{highlight_distinct_edges},
#' \link{dist.dendlist}, \link{tanglegram}
#' \link[distory]{distinct.edges}
#' 
#' @return 
#' A numeric vector of edge ids for the first tree (x) that are not present in the second tree (y).
#' 
#' @source
#' A \link{dendrogram} implementation for \link[distory]{distinct.edges} from the {distory} package
#' 
#' @examples
#' 
#' x <- 1:5 %>% dist %>% hclust %>% as.dendrogram
#' y <- set(x, "labels", 5:1)
#' distinct_edges(x, y)
#' distinct_edges(y, x)
#' dend_diff(x, y)
#' # tanglegram(x, y)
#' 
distinct_edges  <- function (x, y, ...) 
{
   
   sort_a_character <- function(x) x %>% as.character %>% sort
   
   bp1 <- partition_leaves(x)
   bp1 <- lapply(bp1, sort_a_character)
   bp2 <- partition_leaves(y)
   bp2 <- lapply(bp2, sort_a_character)
   p = c()
   for (i in 1:length(bp1)) {
#      if (!(list(bp1[[i]]) %in% bp2)) {
      if (!(bp1[i] %in% bp2)) {
            p <- append(p, i)
      }
   }
   p
}




#' @title Highlight distint edges in a tree (compared to another one)
#' @export
#' @aliases 
#' highlight_distinct_edges.dendrogram
#' highlight_distinct_edges.dendlist
#'
#' @usage 
#' 
#' highlight_distinct_edges(x, ...) 
#' 
#' \method{highlight_distinct_edges}{dendrogram}(x,
#'                   y, 
#'                   value = 2, 
#'                   edgePar = c("col", "lty", "lwd"), 
#'                   ...) 
#'    
#' \method{highlight_distinct_edges}{dendlist}(x, ..., which)
#' 
#' @description
#' Highlight distint edges in a tree (compared to another one) by changing
#' the branches' color, line width, or line type.
#' 
#' This function enables this feature in \link{dend_diff} and \link{tanglegram}
#' 
#' @param x a dendrogram or \link{dendlist} to find unique edges in (to highlight)
#' @param y a dendrogram to compare with
#' @param value a new value scalar for the edgePar attribute.
#' @param edgePar a character indicating the value inside edgePar to adjust.
#' Can be either "col", "lty", or "lwd".
#' @param ... Ignored.
#' @param which an integer vector indicating, in the case "x" is a dendlist, 
#' on which of the trees should the modification be performed. 
#' If missing - the change will be performed on all of objects in the dendlist.
#' 
#' @seealso
#' \link{distinct_edges}, \link{highlight_distinct_edges},
#' \link{dist.dendlist}, \link{tanglegram}
#' \link{assign_values_to_branches_edgePar},
#' \link[distory]{distinct.edges}, 
#' 
#' @return 
#' A dendrogram with modified edges - the distinct ones are changed (color, line width, or line type)
#' 
#' @examples
#' 
#' x <- 1:5 %>% dist %>% hclust %>% as.dendrogram
#' y <- set(x, "labels", 5:1)
#' distinct_edges(x, y)
#' distinct_edges(y, x)
#' 
#' par(mfrow = c(1,2))
#' plot(highlight_distinct_edges(x, y))
#' plot(y)
#' 
#' # tanglegram(highlight_distinct_edges(x, y),y)
#' # dend_diff(x, y)
#' 
#' \dontrun{
#' 
#' # using  highlight_distinct_edges combined with dendlist and set
#' # to clearly highlight "stable" branches.
#' data(iris); ss <- c(1:5, 51:55, 101:105)
#' iris1 <-iris[ss,-5] %>% dist %>% hclust(method = "single") %>% as.dendrogram 
#' iris2 <- iris[ss,-5] %>% dist %>% hclust(method = "complete") %>% as.dendrogram
#' iris12 <- dendlist(iris1, iris2) %>% set("branches_k_color",k=3) %>% 
#'    set("branches_lwd", 3) %>%  highlight_distinct_edges(value = 1, edgePar = "lwd")
#' iris12 %>% untangle(method = "step2side")  %>% 
#' tanglegram(sub="Iris dataset", main_left = "'single' clustering", 
#' 				main_right = "'complete' clustering")
#' }
#' 
highlight_distinct_edges <- function (x, ...) {
   UseMethod("highlight_distinct_edges")
}


#' @export
highlight_distinct_edges.dendrogram <- function (x, y, 
                                      value = 2, edgePar = c("col", "lty", "lwd"), ...)  {
   
   edgePar <- match.arg(edgePar)
   
   the_distinct_edges <- distinct_edges(x, y)
   x_nnoded <- nnodes(x)
   new_value <- rep(Inf, x_nnoded)
   new_value[the_distinct_edges] <- value
   
   new_x <- assign_values_to_branches_edgePar(object = x, value = new_value, edgePar = edgePar)
      
   return(new_x)
}


#' @export
highlight_distinct_edges.dendlist <- function(x, ..., which = c(1L, 2L)) {
   l1 <- which[1]
   l2 <- which[2]
   x[[l1]] <- highlight_distinct_edges(x[[l1]], x[[l2]], ...)
   x[[l2]] <- highlight_distinct_edges(x[[l2]], x[[l1]], ...)
   
   x
}



#' @title Plots two trees side by side, highlighting edges unique to each tree in red.
#' @export
#' @aliases 
#' dend_diff.dendrogram
#' dend_diff.dendlist
#'
#' @usage 
#' 
#' dend_diff(x, ...) 
#' 
#' \method{dend_diff}{dendrogram}(x, y, horiz = TRUE,
#'                   ...) 
#'    
#' \method{dend_diff}{dendlist}(x, ..., which)
#' 
#' @description
#' Plots two trees side by side, highlighting edges unique to each tree in red.
#' 
#' @param x a dendrogram or \link{dendlist} to compre with
#' @param y a dendrogram to compare with
#' @param horiz logical (TRUE) indicating if the dendrogram should be drawn horizontally or not.
#' @param ... passed to \link{plot.dendrogram}
#' @param which an integer vector indicating, in the case "x" is a dendlist, 
#' on which of the trees should the modification be performed. 
#' If missing - the change will be performed on all of objects in the dendlist.
#' 
#' @source
#' A \link{dendrogram} implementation for \link[distory]{phylo.diff} from the {distory} package
#' 
#' @seealso
#' \link{distinct_edges}, \link{highlight_distinct_edges},
#' \link{dist.dendlist}, \link{tanglegram}
#' \link{assign_values_to_branches_edgePar},
#' \link[distory]{distinct.edges}, 
#' 
#' @return 
#' Invisible \link{dendlist} of both trees.
#' 
#' @examples
#' 
#' x <- 1:5 %>% dist %>% hclust %>% as.dendrogram
#' y <- set(x, "labels", 5:1)
#' 
#' dend_diff(x, y)
#' dend_diff(dendlist(x, y))
#' dend_diff(dendlist(y, x))
#' 
#' dend1 <- 1:10 %>% dist %>% hclust %>% as.dendrogram
#' dend2 <- dend1 %>% set("labels", c(1,3,2,4, 5:10) )
#' dend_diff(dend1, dend2)
#' 
dend_diff <- function (x, ...) {
   UseMethod("dend_diff")
}


#' @export
dend_diff.dendrogram <- function(x, y, horiz = TRUE, ...)  {
   x2 <- highlight_distinct_edges(x, y)   
   y2 <- highlight_distinct_edges(y, x)   
   
   # change mfrow
   op <- par()$mfrow
   par(mfrow = c(1, 2))
   
   plot(x2, horiz = horiz, ...)
   plot(y2, horiz = horiz, ...)
   
   # return mfrow
   op <- par()$mfrow
   par(mfrow = op)
   
   invisible(dendlist(x, y)) 
}


#' @export
dend_diff.dendlist <- function(x, ..., which = c(1L, 2L)) {
   l1 <- which[1]
   l2 <- which[2]
   dend_diff(x[[l1]], x[[l2]], ...)
   
   invisible(x)
}








edgeset_dist <- function(x, y, ...)  {
   length(distinct_edges(x, y)) + length(distinct_edges(y, x))
}



#' @title Topological Distances Between Two dendrograms
#' @export
#' @description
#' This function computes the Robinson-Foulds distance (also known as symmetric difference) 
#' between two dendrograms. This is the sum of edges in both trees with labels that exist in 
#' only one of the two trees (i.e.: the length of \link{distinct_edges}).
#' 
#' This function might implement other topological distances in the future.
#' 
#' @param x a \link{dendlist}
#' @param method currently only 'edgeset' is implemented. 
#' @param ... Ignored.
#' 
#' @seealso
#' \link{distinct_edges}, 
#' \link[ape]{dist.topo}, 
#' \link[distory]{dist.multiPhylo}, 
#' \link[phangorn]{treedist}, 
#' 
#' @return 
#' A \link{dist} object with topological distances between all trees
#' 
#' @examples
#' 
#' x <- 1:5 %>% dist %>% hclust %>% as.dendrogram
#' y <- set(x, "labels", 5:1)
#' 
#' dist.dendlist(dendlist(x1 = x,x2 = x,y1 = y))
#' dend_diff(x,y)
#' 
dist.dendlist <- function(x, method = c("edgeset"), ...) {
   if(!is.dendlist(x)) stop("x needs to be a dendlist object")
   method <- match.arg(method)
   
   n_list <- length(x)
   the_dist <- matrix(0, n_list, n_list)
   pairwise_combn <- combn(n_list, 2)
   
   for(i in 1:ncol(pairwise_combn)) {      
      l1 <- pairwise_combn[1,i]
      l2 <-pairwise_combn[2,i]
      the_dist[l1, l2] <- the_dist[l2, l1] <- edgeset_dist(x[[l1]], x[[l2]])
   }
   
   rownames(the_dist) <- colnames(the_dist) <- names(x)
   
   as.dist(the_dist)
}

