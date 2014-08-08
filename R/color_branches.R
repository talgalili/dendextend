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


#' @title Color tree's branches according to sub-clusters
#' @export
#' @aliases
#' colour_branches
#' branches_color
#' @description
#' This function is for dendrogram and hclust objects.
#' This function colors both the terminal leaves of a tree's cluster and the edges 
#' leading to those leaves. The edgePar attribute of nodes will be augmented by 
#' a new list item col.
#' The groups will be defined by a call to \code{\link[dendextend]{cutree}} 
#' using the k or h parameters.
#' 
#' If col is a color vector with a different length than the number of clusters
#' (k) - then a recycled color vector will be used.
#' 
#' @details 
#' If \code{groupLabels=TRUE} then numeric group labels will be added 
#'   to each cluster. If a vector is supplied then these entries will be used as 
#'   the group labels. If a function is supplied then it will be passed a 
#'   numeric vector of groups (e.g. 1:5) and must return the formatted group
#'   labels.
#'   
#' If the \link{labels} of the dendrogram are NOT character (but, for example
#' integers) - they are coerced into character. This step is essential for the
#' proper operation of the function. A dendrogram labels might happen to be 
#' integers if they are based on an \link{hclust} performed on a \link{dist}
#' of an object without \link{rownames}.
#' 
#' 
#' @param tree A \code{dendrogram} or \code{hclust} tree object
#' @param k number of groups (passed to \code{\link[dendextend]{cutree}})
#' @param h height at which to cut tree (passed to \code{\link[dendextend]{cutree}})
#' @param col Function or vector of Colors. By default it tries to use 
#' \link[colorspace]{rainbow_hcl} from the \code{colorspace} package.
#' (with parameters c=90 and l=50). If \code{colorspace} is not available,
#' It will fall back on the \link{rainbow} function.
#' @param groupLabels If TRUE add numeric group label - see Details for options
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' @param ... ignored.
#' @return a tree object of class dendrogram.
#' @author Tal Galili, extensively based on code by Gregory Jefferis
#' @source
#' This function is a derived work from the \code{\link[dendroextras]{color_clusters}}
#' function, with some ideas from the \code{\link[dendroextras]{slice}} function -
#' both are from the {\pkg{dendroextras}} package by jefferis.
#' 
#' It extends it by using \link[dendextend]{cutree.dendrogram} - allowing
#' the function to work for trees that hclust can not handle 
#' (unbranched and non-ultrametric trees).
#' Also, it allows REPEATED cluster color assignments to branches on to 
#' the same tree. Something which the original function was not able to handle.
#' 
#' @seealso \code{\link[dendextend]{cutree}},\code{\link{dendrogram}},
#' \code{\link{hclust}}, \code{\link{labels_colors}}
#' @examples
#' 
#' \dontrun{
#' dend <- USArrests %>% dist %>% hclust(method = "ave") %>% as.dendrogram
#' d1=color_branches(dend,5, col = c(3,1,1,4,1))
#' plot(d1) # selective coloring of branches :)
#' d2=color_branches(d1,5)
#' plot(d2) 
#' 
#' d1=color_branches(dend,5, col = c(3,1,1,4,1),groupLabels=TRUE)
#' plot(d1) # selective coloring of branches :)
#' d2=color_branches(d1,5,groupLabels=TRUE)
#' plot(d2) 
#' 
#' d5=color_branches(dend,5)
#' plot(d5)
#' d5g=color_branches(dend,5,groupLabels=TRUE)
#' plot(d5g)
#' d5gr=color_branches(dend,5,groupLabels=as.roman)
#' plot(d5gr)
#' 
#' # messy - but interesting:
#' dend_override=color_branches(dend,2,groupLabels=as.roman)
#' dend_override=color_branches(dend_override,4,groupLabels=as.roman)
#' dend_override=color_branches(dend_override,7,groupLabels=as.roman)
#' plot(dend_override)
#'  
#' d5=color_branches(tree=dend[[1]],k=5)
#'  
#' 
#' library(dendextend) 
#' data(iris, envir = environment()) 
#' d_iris <- dist(iris[,-5])
#' hc_iris <- hclust(d_iris)
#' dend_iris <- as.dendrogram(hc_iris)
#' dend_iris=color_branches(dend_iris,k=3)
#' 
#' library(colorspace)
#' labels_colors(dend_iris) <-
#'  rainbow_hcl(3)[sort_levels_values(
#'  as.numeric(iris[,5])[order.dendrogram(dend_iris)]
#'  )]
#' 
#' plot(dend_iris, 
#' main = "Clustered Iris dataset",
#'  sub = "labels are colored based on the true cluster")
#'  
#' 
#' 
#' # cutree(dend_iris,k=3, order_clusters_as_data=FALSE,
#'    # sort_cluster_numbers = TRUE, try_cutree_hclust=FALSE)
#' # cutree(dend_iris,k=3, order_clusters_as_data=FALSE,
#'    # sort_cluster_numbers = TRUE, try_cutree_hclust=TRUE)
#' 
#' library(colorspace)
#' 
#' data(iris, envir = environment()) 
#' d_iris <- dist(iris[,-5])
#' hc_iris <- hclust(d_iris)
#' labels(hc_iris) # no labels, because "iris" has no row names
#' dend_iris <- as.dendrogram(hc_iris)
#' is.integer(labels(dend_iris)) # this could cause problems...
#' 
#' iris_species <- rev(levels(iris[,5]))
#' dend_iris <- color_branches(dend_iris,k=3, groupLabels=iris_species)
#' is.character(labels(dend_iris)) # labels are no longer "integer"
#' 
#' # have the labels match the real classification of the flowers:
#' labels_colors(dend_iris) <-
#'    rainbow_hcl(3)[sort_levels_values(
#'       as.numeric(iris[,5])[order.dendrogram(dend_iris)]
#'    )]
#' 
#' # We'll add the flower type
#' labels(dend_iris) <- paste(as.character(iris[,5])[order.dendrogram(dend_iris)],
#'                            "(",labels(dend_iris),")", 
#'                            sep = "")
#' 
#' dend_iris <- hang.dendrogram(dend_iris,hang_height=0.1)
#' 
#' # reduce the size of the labels:
#' dend_iris <- assign_values_to_leaves_nodePar(dend_iris, 0.5, "lab.cex")
#' 
#' par(mar = c(3,3,3,7))
#' plot(dend_iris, 
#'      main = "Clustered Iris dataset
#'      (the labels give the true flower species)", 
#'      horiz =  TRUE,  nodePar = list(cex = .007))
#' legend("topleft", legend = iris_species, fill = rainbow_hcl(3))
#' a= dend_iris[[1]]
#' dend_iris1 <- color_branches(a,k = 3)
#' plot(dend_iris1)
#' 
#' # str(dendrapply(d2, unclass))
#' # unclass(d1)
#' 
#' c(1:5) %>% # take some data
#'    dist %>% # calculate a distance matrix, 
#'    hclust(method = "single") %>% # on it compute hierarchical clustering using the "average" method, 
#'    as.dendrogram %>% color_branches(k=3) %>% plot # nice, returns the tree as is...
#' 
#' }
#' 
color_branches <- function(tree, k=NULL, h=NULL, col, groupLabels=NULL, warn = dendextend_options("warn"), ...){
   
   if(missing(col)) {
      col <- if(require(colorspace))
         function(n) rainbow_hcl(n, c=90, l=50) else
            col <- rainbow
   }      
   
   if(is.null(k) & is.null(h)) {
      warning("k (number of clusters) is missing, using the tree size as a default")      
      k <- nleaves(tree)
   }
   
   if(!is.dendrogram(tree) && !is.hclust(tree)) stop("tree needs to be either a dendrogram or an hclust object")
   g <- dendextend::cutree(tree, k=k, h=h, order_clusters_as_data=FALSE, sort_cluster_numbers = TRUE)
   if(is.hclust(tree)) tree <- as.dendrogram(tree)
   
   k <- max(g)
   
   # For when we have flat trees
   if(k == 0L) {
      if(warn) warning("Tree has only one level - returning the dendrogram with no colors.")
      return(tree)
   }
   
   if(is.function(col)) {
      col <- col(k)
   } else {
      if(length(col) < k) {
         #          stop("Must give the same number of colors as number of clusters")
         warning("Length of color vector was shorter than the number of clusters - color vector was recycled")
         col <- rep(col, length.out = k)
      }
      if(length(col) > k) {
         #          stop("Must give the same number of colors as number of clusters")
         warning("Length of color vector was longer than the number of clusters - first k elements are used")
         col <- col[seq_len(k)]
      }
      
   }
   
   
   if(!is.null(groupLabels)){
      if(length(groupLabels)==1){
         if(is.function(groupLabels))
            groupLabels=groupLabels(seq.int(length.out=k))
         else if(is.logical(groupLabels)){
            if(groupLabels)
               groupLabels=seq.int(length.out=k)
            else groupLabels=NULL
         }
      }
      if(!is.null(groupLabels) && length(groupLabels)!=k)
         stop("Must give same number of group labels as clusters")
   }
   
   addcol <- function(dend_node,col) {      
      if(is.null(attr(dend_node, "edgePar"))) {
         attr(dend_node,'edgePar') <- list(col=col)
      } else {            
         attr(dend_node, "edgePar")[["col"]] <- col
#             within(attr(dend_node, "edgePar"), 
#                    {col=col}) 
         # this way it doesn't erase other nodePar values (if they exist)
      }
      unclass(dend_node)
   }
   
   descendTree <- function(sd){
      groupsinsubtree=unique(g[labels(sd)])
      if(length(groupsinsubtree)>1){
         # keep descending 
         for(i in seq(sd))
            sd[[i]] <- descendTree(sd[[i]])
      } else {
         # else assign Colors
         # sd=dendrapply(sd,addcol,col[groupsinsubtree],groupsinsubtree)
         sd=dendrapply(sd,addcol,col[groupsinsubtree])
         if(!is.null(groupLabels)){
            attr(sd,'edgetext')=groupLabels[groupsinsubtree]
             attr(sd,'edgePar')=c(attr(sd,'edgePar'),list(p.border=col[groupsinsubtree]))
#            attr(sd,'edgePar')[["p.border"]]=col[groupsinsubtree]
         }
      }
      unclass(sd)
   }
   
   if(!is.character(labels(tree))) labels(tree) <- as.character(labels(tree))
   tree <- descendTree(tree)
   class(tree) <- "dendrogram"
   tree   
}

# str(unclass(d2))
# plot(tree)

# nice idea - make this compatible with colour/color
#' @export
colour_branches <- color_branches

#' @export
branches_color <- color_branches











if(F) {
   
   data(iris, envir = environment()) 
   d_iris <- dist(iris[1:4,-5],method="man")
   hc_iris <- hclust(d_iris)
   labels(hc_iris) # no labels, because "iris" has no row names
   dend_iris <- as.dendrogram(hc_iris)
   plot(dend_iris)
   
   labels_colors(dend_iris)[2] <- 2
   is.integer(labels(dend_iris)) # this could cause problems...
   
   # than add this function to color_labels, add a labels parameter - and have it override everything else!
   plot(color_labels_by_labels(dend_iris, c("1","4"), c(2,3,5), warn = TRUE))
   
   # Maybe I should create color_labels_by_kh!!!
   color_labels
   # add a "warn" parameter
}

color_labels_by_labels <- function(tree, labels, col, warn = dendextend_options("warn"), ...) {
   tree_labels <- labels(tree)
   tree_col <- labels_colors(tree)
   
   ss_labels_to_color <- tree_labels %in% labels
   
   # recycle colors if they are shorter than the labels.
   rep_col <- rep_len(col, sum(ss_labels_to_color))
   if(length(rep_col) != length(col)) {
      if(warn) warning("The length of 'col' is different than that of the labels in the tree. 'col' recycled to fit the length.")
      col <- rep_col
   }   
   
   # fix vector of labels colors
   tree_col[ss_labels_to_color] <- col
   # fix labels colors in the tree
   labels_colors(tree) <- tree_col
   
   return(tree)   
}


















#' @title Color tree's labels according to sub-clusters
#' @export
#' @aliases
#' colour_labels
#' @description
#' This function is for dendrogram and hclust objects.
#' This function colors tree's labels.
#' 
#' The groups will be defined by a call to \code{\link[dendextend]{cutree}} 
#' using the k or h parameters.
#' 
#' If col is a color vector with a different length than the number of clusters
#' (k) - then a recycled color vector will be used.
#' 
#' @param tree A \code{dendrogram} or \code{hclust} tree object
#' @param k number of groups (passed to \code{\link[dendextend]{cutree}})
#' @param h height at which to cut tree (passed to \code{\link[dendextend]{cutree}})
#' @param col Function or vector of Colors. By default it tries to use 
#' \link[colorspace]{rainbow_hcl} from the \code{colorspace} package.
#' (with parameters c=90 and l=50). If \code{colorspace} is not available,
#' It will fall back on the \link{rainbow} function.
#' @param labels character vecotor. If not missing, it overrides k and h,
#' and simply colors these labels in the tree based on "col" parameter.
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' (in case h/k/labels are not supplied, or if col is too short)
#' @param ... ignored.
#' @return a tree object of class dendrogram.
#' @author Tal Galili
#' @source
#' This function is in the style of \code{\link{color_branches}}, and 
#' based on \code{\link{labels_colors}}.
#' @seealso \code{\link[dendextend]{cutree}},\code{\link{dendrogram}},
#' \code{\link{hclust}}, \code{\link{labels_colors}}, \code{\link{color_branches}}
#' @examples
#' 
#' \dontrun{
#' hc <- hclust(dist(USArrests), "ave")
#' dend <- as.dendrogram(hc)
#' dend=color_labels(dend,5, col = c(3,1,1,4,1))
#' dend=color_branches(dend,5, col = c(3,1,1,4,1))
#' plot(dend) # selective coloring of branches AND labels :)
#' 
#' # coloring some labels, based on label names:
#' dend=color_labels(dend,col = "red", labels = labels(dend)[c(4,16)]) 
#' plot(dend) # selective coloring of branches AND labels :)
#' 
#' d5=color_branches(dend,5)
#' plot(d5)
#' d5g=color_branches(dend,5,groupLabels=TRUE)
#' plot(d5g)
#' d5gr=color_branches(dend,5,groupLabels=as.roman)
#' plot(d5gr)
#' 
#' } 
#' 
color_labels <- function(tree, k=NULL, h=NULL, labels, col, warn = dendextend_options("warn"),...){

   if(!missing(labels)) return(color_labels_by_labels(tree=tree, labels=labels, col=col, warn=warn, ...) )     
   
   
   if(missing(col)) {
      if(require(colorspace)) {
         col <- function(n) rainbow_hcl(n, c=90, l=50)
      } else {
         col <- rainbow
      }      
   }   
   
   if(!is.dendrogram(tree) && !is.hclust(tree)) stop("tree needs to be either a dendrogram or an hclust object")
   
   if(missing(k) & missing(h)) {
      k = nleaves(tree)
      if(warn) warning("Neither k nor h were supplied - coloring all leaves based on 'col'.")
   }
   
   g <- dendextend::cutree(tree,k=k,h=h, order_clusters_as_data=FALSE, sort_cluster_numbers = TRUE)
   if(is.hclust(tree)) tree=as.dendrogram(tree)
   
   k <- max(g)
   if(is.function(col)) {
      col=col(k)
   } else {
      if(length(col) < k) {
         #          stop("Must give the same number of colors as number of clusters")
         if(warn) warning("Length of color vector was shorter than the number of clusters - color vector was recycled")
         col <- rep(col, length.out = k)
      }
      if(length(col) > k) {
         #          stop("Must give the same number of colors as number of clusters")
         if(warn) warning("Length of color vector was longer than the number of clusters - first k elements are used")
         col <- col[seq_len(k)]
      }
   }
   
   
   labels_colors(tree) <- col[g]
   
   return(tree)
}


# nice idea - make this compatible with colour/color
colour_labels <- color_labels



# 
# library(microbenchmark)
# microbenchmark(
#    stats:::labels.dendrogram(dend),
#    labels(dend)
#    )
# library(dendextendRcpp)
# 






#' Return the leaf Colors of a dendrogram
#' 
#' @details The returned Colors will be in dendrogram order.
#' @param d the dendrogram
#' @param col_to_return Character scalar - kind of Color attribute to return
#' @return named character vector of Colors, NA_character_ where missing
#' @author jefferis
#' @export
#' @aliases leaf_colors
#' @seealso \code{\link[dendroextras]{slice},\link{color_branches}}
#' @examples
#' dend <- USArrests %>% dist %>% hclust(method = "ave") %>% as.dendrogram
#' d5=color_branches(dend,5)
#' leaf_Colors(d5)
leaf_Colors <- function(d,col_to_return=c("edge",'node','label')){
   if(!inherits(d,'dendrogram')) stop("I need a dendrogram!")
   col_to_return=match.arg(col_to_return)
   leaf_col <- function(n,col_to_return) {
      if(is.leaf(n)) {
         col=switch(col_to_return,
                    edge=attr(n,'edgePar')$col,
                    node=attr(n,'nodePar')$col,
                    label=attr(n,'nodePar')$lab.col)
         if(is.null(col)) col=NA_character_
         structure(col,.Names=attr(n,'label'))
      } else NULL
   }
   
   unlist(dendrapply(d,leaf_col,col_to_return))
}

leaf_colors <- leaf_Colors
