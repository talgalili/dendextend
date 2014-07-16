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


#' @title Draw Rectangles Around a Dendrogram's Clusters
#' @export
#' @description
#' Draws rectangles around the branches of a dendrogram
#'  highlighting the corresponding clusters.
#'  First the dendrogram is cut at a certain level, 
#'  then a rectangle is drawn around selected branches.
#' @param tree a \link{dendrogram} object.
#' @param k Scalar. Cut the dendrogram such that exactly k clusters (if possible) are produced.
#' @param h Scalar. Cut the dendrogram by cutting at height h. (k overrides h)
#' @param which A vector selecting the clusters around 
#' which a rectangle should be drawn. which selects clusters by number
#'  (from left to right in the tree), Default is which = 1:k.
#' @param x A vector selecting the clusters around 
#' which a rectangle should be drawn. x selects clusters
#'  containing the respective horizontal coordinates.
#' @param border Vector with border colors for the rectangles.
#' @param cluster Optional vector with cluster memberships as returned by 
#' cutree(dend_obj, k = k), can be specified for efficiency if already computed.
#' @param ... parameters passed to rect (such as lwd, lty, etc.)
#' @param horiz logical (FALSE), indicating if the rectangles 
#' should be drawn horizontally or not (for when using 
#' plot(dend, horiz = TRUE) ) .
#' @seealso
#' \link{rect.hclust}, \link{order.dendrogram}, \link{cutree.dendrogram}
#' @return 
#' (Invisibly) returns a list where each element contains a vector
#'  of data points contained in the respective cluster.
#' @source
#' This function is based on \link{rect.hclust}, with slight modifications
#' to have it work with a dendrogram, as well as a few added features
#' (e.g: ... to rect, and horiz)
#' @examples
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 10 )
#' hc <- iris[ss,-5] %>% dist %>% hclust
#' dend <- hc %>% as.dendrogram
#' 
#' plot(dend)
#' rect.dendrogram(dend,2, border = 2)
#' plot(dend)
#' rect.dendrogram(dend,4, which = c(1,3), border = c(2,3))
#' rect.dendrogram(dend,4, x = 5, border = c(4))
#' rect.dendrogram(dend,3, border = 3, lwd = 2, lty = 2)
#' # now THIS, you can not do with the old rect.hclust
#' plot(dend, horiz = TRUE)
#' rect.dendrogram(dend,2, border = 2, horiz = TRUE)
#' rect.dendrogram(dend,4, border = 4, lty = 2, lwd = 3, horiz = TRUE)
rect.dendrogram <- function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2, 
          cluster = NULL, horiz = FALSE, ...) 
{
   if(!is.dendrogram(tree)) stop("x is not a dendrogram object.")

   if (length(h) > 1L | length(k) > 1L) 
      stop("'k' and 'h' must be a scalar(i.e.: of length 1)")
   
   # In tree_heights I am removing the first element
   # in order to be consistant with rect.hclust
   tree_heights <- heights_per_k.dendrogram(tree)[-1]
   tree_order <- order.dendrogram(tree)   
   
   if (!is.null(h)) {
      if (!is.null(k)) 
         stop("specify exactly one of 'k' and 'h'")
      
      ss_ks <- tree_heights < h
      k <- min(as.numeric(names(ss_ks))[ss_ks])
      k <- max(k, 2) # I don't like this default...
   }
   else if (is.null(k)) 
      stop("specify exactly one of 'k' and 'h'")
   if (k < 2 | k > length(tree_heights)) 
      stop(gettextf("k must be between 2 and %d", length(tree_heights)), 
           domain = NA)
   if (is.null(cluster)) 
      cluster <- cutree(tree, k = k)
   
   clustab <- table(cluster)[unique(cluster[tree_order])]
   
   m <- c(0, cumsum(clustab))
   if (!is.null(x)) {
      if (!is.null(which)) 
         stop("specify exactly one of 'which' and 'x'")
      which <- x
      for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
   }
   else if (is.null(which)) 
      which <- 1L:k
   if (any(which > k)) 
      stop(gettextf("all elements of 'which' must be between 1 and %d", 
                    k), domain = NA)
   border <- rep_len(border, length(which))
   retval <- list()
   for (n in seq_along(which)) {
      
      if(!horiz) { # the default
         xleft = m[which[n]] + 0.66
         ybottom = par("usr")[3L]
         xright = m[which[n] + 1] + 0.33
         ytop = mean(tree_heights[(k - 1):k])
      } else {         
         xleft = mean(tree_heights[(k - 1):k])
         ybottom = m[which[n]] + 0.66
         xright = par("usr")[2L]
         ytop = m[which[n] + 1] + 0.33
      }      
      rect(xleft, 
           ybottom,
           xright , 
           ytop , 
           border = border[n], ...)
      retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
   }
   invisible(retval)
}








identify.dendrogram <- function (x, FUN = NULL, N = 20, MAXCLUSTER = 20, DEV.FUN = NULL, 
          ...) 
{
   cluster <- cutree(x, k = 2:MAXCLUSTER)
   retval <- list()
   oldk <- NULL
   oldx <- NULL
   DEV.x <- grDevices::dev.cur()
   for (n in 1L:N) {
      grDevices::dev.set(DEV.x)
      X <- locator(1)
      if (is.null(X)) 
         break
      k <- min(which(rev(x$height) < X$y), MAXCLUSTER)
      k <- max(k, 2)
      if (!is.null(oldx)) {
         rect.hclust(x, k = oldk, x = oldx, cluster = cluster[, 
                                                              oldk - 1], border = "grey")
      }
      retval[[n]] <- unlist(rect.hclust(x, k = k, x = X$x, 
                                        cluster = cluster[, k - 1], border = "red"))
      if (!is.null(FUN)) {
         if (!is.null(DEV.FUN)) {
            grDevices::dev.set(DEV.FUN)
         }
         retval[[n]] <- FUN(retval[[n]], ...)
      }
      oldx <- X$x
      oldk <- k
   }
   grDevices::dev.set(DEV.x)
   invisible(retval)
}