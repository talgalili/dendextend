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
#' @param tree A \code{dendrogram} or \code{hclust} tree object
#' @param k number of groups (passed to \code{\link[dendextend]{cutree}})
#' @param h height at which to cut tree (passed to \code{\link[dendextend]{cutree}})
#' @param col Function or vector of Colors. By default it tries to use 
#' \link[colorspace]{rainbow_hcl} from the \code{colorspace} package.
#' (with parameters c=90 and l=50). If \code{colorspace} is not available,
#' It will fall back on the \link{rainbow} function.
#' @param groupLabels If TRUE add numeric group label - see Details for options
#' @param ... ignored.
#' @return a tree object of class dendrogram.
#' @author Tal Galili, extensively based on code by jefferis
#' @source
#' This function is a derived work from the \code{\link[dendroextra]{color_clusters}}
#' function, with some ideas from the \code{\link[dendroextra]{slice}} function -
#' both are from the {\pkg{dendroextra}} package by jefferis.
#' 
#' It extends it by using \link[dendextend]{cutree.dendrogram} - allowing
#' the function to work for trees that hclust can not handle 
#' (unrooted and non-ultrametric trees).
#' Also, it allows REPEATED cluster color assignments to branches on to 
#' the same tree. Something which the original function was not able to handle.
#' 
#' @seealso \code{\link[dendextend]{cutree}},\code{\link{dendrogram}},
#' \code{\link{hclust}}, \code{\link{labels_colors}}
#' @examples
#' 
#' \dontrun{
#' dend <- hclust(dist(USArrests), "ave")
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
#'  
#' # str(dendrapply(d2, unclass))
#' # unclass(d1)
#' 
#' }
#' 
color_branches<-function(tree,k=NULL,h=NULL,col,groupLabels=NULL,...){
   
   if(missing(col)) {
      if(require(colorspace)) {
         col <- function(n) rainbow_hcl(n, c=90, l=50)
      } else {
         col <- rainbow
      }      
   }   
   
   if(!is.dendrogram(tree) && !is.hclust(tree)) stop("tree needs to be either a dendrogram or an hclust object")
   g <- dendextend:::cutree(tree,k=k,h=h, order_clusters_as_data=FALSE, sort_cluster_numbers = TRUE)
   if(is.hclust(tree)) tree=as.dendrogram(tree)
   
   k <- max(g)
   if(is.function(col)) {
      col=col(k)
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
   
   addcol<-function(dend_node,col) {      
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
   
   descendTree<-function(sd){
      groupsinsubtree=unique(g[labels(sd)])
      if(length(groupsinsubtree)>1){
         # keep descending 
         for(i in seq(sd))
            sd[[i]]<-descendTree(sd[[i]])
      } else {
         # else assign Colors
         # sd=dendrapply(sd,addcol,col[groupsinsubtree],groupsinsubtree)
         sd=dendrapply(sd,addcol,col[groupsinsubtree])
         if(!is.null(groupLabels)){
            attr(sd,'edgetext')=groupLabels[groupsinsubtree]
#             attr(sd,'edgePar')=c(attr(sd,'edgePar'),list(p.border=col[groupsinsubtree]))
            attr(sd,'edgePar')[["p.border"]]=col[groupsinsubtree]
         }
      }
      unclass(sd)
   }
   tree <- descendTree(tree)
   class(tree) <- "dendrogram"
   tree   
}

# str(unclass(d2))
# plot(tree)

# nice idea - make this compatible with colour/color
colour_branches<-color_branches






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
color_labels<-function(tree,k=NULL,h=NULL,col,groupLabels=NULL,...){
   
   if(missing(col)) {
      if(require(colorspace)) {
         col <- function(n) rainbow_hcl(n, c=90, l=50)
      } else {
         col <- rainbow
      }      
   }   
   
   if(!is.dendrogram(tree) && !is.hclust(tree)) stop("tree needs to be either a dendrogram or an hclust object")
   g <- dendextend:::cutree(tree,k=k,h=h, order_clusters_as_data=FALSE, sort_cluster_numbers = TRUE)
   if(is.hclust(tree)) tree=as.dendrogram(tree)
   
   k <- max(g)
   if(is.function(col)) {
      col=col(k)
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
   
   
   labels_colors(tree) <- col[g]
   
   return(tree)
}


# nice idea - make this compatible with colour/color
colour_labels<-color_labels











#' Return the leaf Colors of a dendrogram
#' 
#' @details The returned Colors will be in dendrogram order.
#' @param d the dendrogram
#' @param col_to_return Character scalar - kind of Color attribute to return
#' @return named character vector of Colors, NA_character_ where missing
#' @author jefferis
#' @export
#' @aliases leaf_colors
#' @seealso \code{\link{slice},\link{color_branches}}
#' @examples
#' d5=color_branches(dend,5)
#' leaf_Colors(d5)
leaf_Colors<-function(d,col_to_return=c("edge",'node','label')){
   if(!inherits(d,'dendrogram')) stop("I need a dendrogram!")
   col_to_return=match.arg(col_to_return)
   leaf_col<-function(n,col_to_return) {
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

leaf_colors<-leaf_Colors
