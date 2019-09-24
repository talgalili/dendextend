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
#' @param horiz logical (FALSE), indicating if the rectangles
#' should be drawn horizontally or not (for when using
#' plot(dend, horiz = TRUE) ) .
#' @param density Passed to \link{rect}: the density of shading lines,
#' in lines per inch. The default value of NULL means that
#' no shading lines are drawn. A zero value of density means
#' no shading lines whereas negative values (and NA)
#' suppress shading (and so allow color filling).
#' If border is a vector of colors, the color of density will default to 1.
#' @param angle Passed to \link{rect}: angle (in degrees) of the shading lines.
#' (default is 45)
#' @param text a character vector of labels to plot underneath the clusters.
#' When NULL (default), no text is displayed.
#' @param text_cex a numeric (scalar) value of the text's cex value.
#' @param text_col a (scalar) value of the text's col(or) value.
#' @param xpd A logical value (or NA.), passed to \link{par}.
#' Default is TRUE, in order to allow the rect to be below the labels.
#' If FALSE, all plotting is clipped to the plot region, if TRUE,
#' all plotting is clipped to the figure region, and if NA, all plotting
#' is clipped to the device region. See also \link{clip}.
#' @param lower_rect a (scalar) value of how low should the lower part of the rect be.
#' If missing, it will take the value of par("usr")[3L] (or par("usr")[2L], depending
#' if horiz = TRUE or not), with also the width of the labels. (notice that we
#' would like to keep xpd = TRUE if we want the rect to be after the labels!)
#' You can use a value such as 0, to get the rect above the labels.
#'
#' Notice that for a plot with small margins, it would be better to set this
#' parameter manually.
#' @param upper_rect a (scalar) value to add (default is 0) to how high should the upper part of the rect be.
#' @param prop_k_height a (scalar) value (should be between 0 to 1), indicating what proportion
#' of the height our rect will be between the height needed for k and k+1 clustering.
#' @param stop_if_out logical (default is TRUE). This makes the function
#' stop if k of the locator is outside the range (this default reproduces the behavior
#' of the rect.hclust function).
#' @param ... parameters passed to rect (such as lwd, lty, etc.)
#' @seealso
#' \link{rect.hclust}, \link{order.dendrogram}, \link{cutree.dendrogram}
#' @return
#' (Invisibly) returns a list where each element contains a vector
#'  of data points contained in the respective cluster.
#' @source
#' This function is based on \link{rect.hclust}, with slight modifications
#' to have it work with a dendrogram, as well as a few added features
#' (e.g: ... to rect, and horiz)
#'
#' The idea of adding text and shading lines under the clusters comes from
#' skullkey from here:
#' \url{http://stackoverflow.com/questions/4720307/change-dendrogram-leaves}
#'
#' @examples
#'
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#' hc <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust()
#' dend <- hc %>% as.dendrogram()
#'
#' plot(dend)
#' rect.dendrogram(dend, 2, border = 2)
#' rect.dendrogram(dend, 3, border = 4)
#' Vectorize(rect.dendrogram, "k")(dend, 4:5, border = 6)
#'
#' plot(dend)
#' rect.dendrogram(dend, 3,
#'   border = 1:3,
#'   density = 2, text = c("1", "b", "miao"), text_cex = 3
#' )
#'
#' plot(dend)
#' rect.dendrogram(dend, 4, which = c(1, 3), border = c(2, 3))
#' rect.dendrogram(dend, 4, x = 5, border = c(4))
#' rect.dendrogram(dend, 3, border = 3, lwd = 2, lty = 2)
#' # now THIS, you can not do with the old rect.hclust
#' plot(dend, horiz = TRUE)
#' rect.dendrogram(dend, 2, border = 2, horiz = TRUE)
#' rect.dendrogram(dend, 4, border = 4, lty = 2, lwd = 3, horiz = TRUE)
#'
#' # This had previously failed since it worked with a wrong k.
#'
#' dend15 <- c(1:5) %>%
#'   dist() %>%
#'   hclust(method = "average") %>%
#'   as.dendrogram()
#' # dend15 <- c(1:25) %>% dist %>% hclust(method = "average") %>% as.dendrogram
#' dend15 %>%
#'   set("branches_k_color") %>%
#'   plot()
#' dend15 %>% rect.dendrogram(
#'   k = 3,
#'   border = 8, lty = 5, lwd = 2
#' )
rect.dendrogram <- function(tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2,
                            cluster = NULL, horiz = FALSE, density = NULL, angle = 45,
                            text = NULL, text_cex = 1, text_col = 1, xpd = TRUE,
                            lower_rect, upper_rect = 0, prop_k_height = 0.5,
                            stop_if_out = FALSE,
                            ...) {
  if (!is.dendrogram(tree)) stop("x is not a dendrogram object.")

  if (length(h) > 1L | length(k) > 1L) {
    stop("'k' and 'h' must be a scalar(i.e.: of length 1)")
  }

  # In tree_heights I am removing the first element
  # in order to be consistant with rect.hclust
  tree_heights <- heights_per_k.dendrogram(tree)[-1] # this is NOT really tree heights, but the height for which you need to cut in order to cut the tree
  tree_order <- order.dendrogram(tree)
  #    rm0 <- function(x) x[x != 0]
  #    height_to_add <- min(rm0(abs(diff(tree_heights))))/2 # the height to add so to be sure we get a "clear" cut



  if (!is.null(h)) {
    if (!is.null(k)) {
      stop("specify exactly one of 'k' and 'h'")
    }

    ss_ks <- tree_heights < h
    k <- min(as.numeric(names(ss_ks))[ss_ks])
    k <- max(k, 2) # I don't like this default...
  }
  else if (is.null(k)) {
    stop("specify exactly one of 'k' and 'h'")
  }
  if (k < 2 | k > length(tree_heights)) {
    if (stop_if_out) {
      stop(gettextf("k must be between 2 and %d", length(tree_heights)),
        domain = NA
      )
    } else {
      warning(gettextf("k must be between 2 and %d", length(tree_heights)),
        domain = NA
      )
    }
  }
  if (is.null(cluster)) {
    cluster <- cutree(tree, k = k)
  }

  clustab <- table(cluster)[unique(cluster[tree_order])]

  m <- c(0, cumsum(clustab))
  if (!is.null(x)) {
    if (!is.null(which)) {
      stop("specify exactly one of 'which' and 'x'")
    }
    which <- x
    for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
  } else if (is.null(which)) {
    which <- 1L:k
  }
  if (any(which > k)) {
    stop(gettextf(
      "all elements of 'which' must be between 1 and %d",
      k
    ), domain = NA)
  }
  border <- rep_len(border, length(which))
  retval <- list()

  old_xpd <- par()["xpd"]
  par(xpd = xpd)

  for (n in seq_along(which)) {
    # this is to deal with the case when k is not defined for all values
    # and that we can not use the next k+1 value to decide on the height to use.
    next_k_height <- tree_heights[names(tree_heights) == k + 1]
    if (length(next_k_height) == 0) {
      next_k_height <- 0
      prop_k_height <- 1 # use only the "k" now.
      # if(upper_rect == 0) upper_rect <- min(abs(diff(tree_heights))) / 2
    }

    if (!horiz) { # the default
      xleft <- m[which[n]] + 0.66
      # if(missing(lower_rect)) lower_rect <- par("usr")[3L] - strheight("W")*(max(nchar(labels(tree))) + 1)
      if (missing(lower_rect)) {
        lower_rect <- -max(strheight2(labels(tree)))
        dLeaf <- -0.75 * strheight("x")
        extra_space <- -strheight2("_")
        lower_rect <- lower_rect + dLeaf + extra_space
      }

      ybottom <- lower_rect
      xright <- m[which[n] + 1] + 0.33
      #          ytop = mean(tree_heights[(k - 1):k])
      #          ytop = tree_heights[k] + abs(ybottom)
      #          ytop = mean(tree_heights[(k - 1):k]) + abs(ybottom)

      ytop <- tree_heights[names(tree_heights) == k] * prop_k_height +
        next_k_height * (1 - prop_k_height) +
        upper_rect # tree_heights[k] + height_to_add # + abs(xright)
    } else {
      ybottom <- m[which[n]] + 0.66
      # if(missing(lower_rect)) lower_rect <- par("usr")[2L] + strwidth("X")*(max(nchar(labels(tree))) + 1)
      # if(missing(lower_rect)) lower_rect <- -max(strwidth(labels(dend)))

      if (missing(lower_rect)) {
        lower_rect <- min(strwidth(labels(tree))) # notice the char length is negative!
        dLeaf <- 0.75 * strwidth("w")
        extra_space <- strwidth("_")
        lower_rect <- lower_rect + dLeaf + extra_space
      }

      xright <- lower_rect

      ytop <- m[which[n] + 1] + 0.33
      #          xleft = mean(tree_heights[(k - 1):k])
      xleft <-
        tree_heights[names(tree_heights) == k] * prop_k_height +
        next_k_height * (1 - prop_k_height) +
        upper_rect # tree_heights[k] + height_to_add # + abs(xright)
    }
    rect(xleft,
      ybottom,
      xright,
      ytop,
      border = border[n], density = density, angle = angle, ...
    )

    # allow for a vectorized version of "text"
    if (!is.null(text)) {
      text((m[which[n]] + m[which[n] + 1] + 1) / 2,
        grconvertY(grconvertY(par("usr")[3L], "user", "ndc") + 0.02, "ndc", "user"),
        text[n],
        cex = text_cex, col = text_col
      )
    }

    retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
  }


  par(xpd = old_xpd)
  invisible(retval)
}












#' @title Identify Clusters in a Dendrogram (not hclust)
#' @export
#' @description
#' Just like \link{identify.hclust}:
#' reads the position of the graphics pointer when the (first)
#' mouse button is pressed. It then cuts the tree at the vertical
#'  position of the pointer and highlights the cluster containing
#'  the horizontal position of the pointer. Optionally a function is applied
#'  to the index of data points contained in the cluster.
#'
#' @param x a \link{dendrogram} object.
#' @param FUN (optional) function to be applied to the index numbers of the
#' data points in a cluster (see 'Details' below).
#' @param N the maximum number of clusters to be identified.
#' @param MAXCLUSTER the maximum number of clusters that can be produced by
#' a cut (limits the effective vertical range of the pointer).
#' @param DEV.FUN (optional) integer scalar. If specified, the corresponding
#' graphics device is made active before FUN is applied.
#' @param horiz logical (FALSE), indicating if the rectangles
#' should be drawn horizontally or not (for when using
#' plot(dend, horiz = TRUE) ) .
#' @param stop_if_out logical (default is FALSE). This default makes the function
#' NOT stop if k of the locator is outside the range (this default is different than the behavior
#' of the identify.hclust function - but it is nicer for the user.).
#' @param ... further arguments to FUN.
#' @details
#' By default clusters can be identified using the mouse and an invisible
#' list of indices of the respective data points is returned.
#' If FUN is not NULL, then the index vector of data points is passed to this
#'  function as first argument, see the examples below. The active
#'  graphics device for FUN can be specified using DEV.FUN.
#'  The identification process is terminated by pressing any mouse button other
#'  than the first, see also identify.
#' @seealso
#' \link{identify.hclust},
#' \link{rect.hclust}, \link{order.dendrogram}, \link{cutree.dendrogram}
#' @return
#' (Invisibly) returns a list where each element contains a vector
#'  of data points contained in the respective cluster.
#' @source
#' This function is based on \link{identify.hclust}, with slight modifications
#' to have it work with a dendrogram, as well as adding "horiz"
#' @examples
#'
#' \dontrun{
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#' hc <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust()
#' dend <- hc %>% as.dendrogram()
#'
#' plot(dend)
#' identify(dend)
#'
#' plot(dend, horiz = TRUE)
#' identify(dend, horiz = TRUE)
#' }
identify.dendrogram <- function(x, FUN = NULL, N = 20, MAXCLUSTER, DEV.FUN = NULL,
                                horiz = FALSE,
                                stop_if_out = FALSE,
                                ...) {

  # In tree_heights I am removing the first element
  # in order to be consistant with rect.hclust
  x_heights <- heights_per_k.dendrogram(x)[-1]
  # this is like rev(x$height)
  #    tree_order <- order.dendrogram(tree)
  if (missing(MAXCLUSTER)) MAXCLUSTER <- nleaves(x)

  cluster <- cutree(x, k = 2:MAXCLUSTER)
  retval <- list()
  oldk <- NULL
  oldx <- NULL
  DEV.x <- grDevices::dev.cur()
  for (n in 1L:N) {
    grDevices::dev.set(DEV.x)
    X <- locator(1)
    if (horiz) {
      tmp <- X$x
      X$x <- X$y
      X$y <- tmp
    }

    if (is.null(X)) {
      break
    }
    k <- min(which(x_heights < X$y), MAXCLUSTER)
    k <- max(k, 2)
    if (!is.null(oldx)) {
      rect.dendrogram(x,
        k = oldk, x = oldx,
        cluster = cluster[, oldk - 1],
        border = "grey", horiz = horiz, stop_if_out = stop_if_out
      )
    }
    retval[[n]] <- unlist(rect.dendrogram(x,
      k = k, x = X$x,
      cluster = cluster[, k - 1],
      border = "red", horiz = horiz, stop_if_out = stop_if_out
    ))
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
