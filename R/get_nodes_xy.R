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




# We had to write a stripped down version of the following functions:
# stats:::plot.dendrogram
# stats:::plotNodeLimit
# stats:::plotNode
# stats:::.memberDend(dend)
# stats:::.midDend(dend)




#' @title Get the x-y coordinates of a dendrogram's nodes
#' @description Get the x-y coordinates of a dendrogram's nodes. Can be used to add text or images on the tree.
#' @export
#' @param dend a dendrogram object
#' @param type type of plot.
#' @param center logical; if TRUE, nodes are plotted centered with respect to the
#' leaves in the branch. Otherwise (default),
#' plot them in the middle of all direct child nodes.
#' @param horiz logical indicating if the dendrogram should be drawn horizontally or not.
#' @param ... not used
#' @source This is a striped down version of the
#' function \code{\link{plot.dendrogram}}.
#' It performs (almost) the same task, only it does not do any plotting
#' but it does save the x-y coordiantes of the nodes.
#' @return
#' A 2-dimensional matrix, with rows as the number of nodes,
#' and the first column is the x location, while the second is the
#' y location.
#' @seealso \link{get_nodes_attr}, \link{nnodes},
#' \link{nleaves}
#' @examples
#' \dontrun{
#'
#' # If we would like to see the numbers from plot:
#' # ?getOption("verbose")
#' # options(verbose=TRUE)
#' # options(verbose=FALSE)
#'
#' # -----
#' # Draw a depth first search illustration
#' # -----
#'
#' dend <- 1:5 %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' get_nodes_xy(dend)
#'
#' # polygon(get_nodes_xy(dend), col = 2)
#' plot(dend,
#'   leaflab = "none",
#'   main = "Depth-first search in a dendrogram"
#' )
#' xy <- get_nodes_xy(dend)
#' for (i in 1:(nrow(xy) - 1)) {
#'   arrows(xy[i, 1], xy[i, 2],
#'     angle = 17,
#'     length = .5,
#'     xy[i + 1, 1], xy[i + 1, 2],
#'     lty = 1, col = 3, lwd = 1.5
#'   )
#' }
#' points(xy, pch = 19, cex = 4)
#' text(xy, labels = 1:nnodes(dend), cex = 1.2, col = "white", adj = c(0.4, 0.4))
#' }
get_nodes_xy <- function(dend, type = c("rectangle", "triangle"), center = FALSE,
                         horiz = FALSE, ...) {
  x <- dend # TODO: this code could be cleaned

  # this function/env let's us keep a "stack" of the xy matrix, and update it
  # no matter how deep we get in the recursion...
  nodes_xy <- local({

    # notice how we must create this function within get_nodes_xy
    # since it relies on the specific value of "x" when first created.
    xy_matrix <- matrix(0, nrow = nnodes(x), ncol = 2)
    i_node <- 0

    function(xy) {
      if (missing(xy)) {
        return(with(environment(nodes_xy), xy_matrix))
      }

      # next node
      i_node <<- i_node + 1
      xy_matrix[i_node, ] <<- xy
    }
  })


  type <- match.arg(type)
  hgt <- attr(x, "height")
  mem.x <- nleaves(x)

  if (center) {
    x1 <- 0.5
    x2 <- mem.x + 0.5
  }
  else {
    x1 <- 1
    x2 <- mem.x
  }

  plotNode2(x1, x2, x,
    type = type, center = center, leaflab = "none",
    horiz = horiz,
    nodes_xy = nodes_xy
  )

  return(nodes_xy())
}


















plotNodeLimit2 <- function(x1, x2, subtree, center) {
  inner <- !is.leaf(subtree) && x1 != x2
  if (inner) {
    K <- length(subtree)
    mTop <- nleaves(subtree)
    limit <- integer(K)
    xx1 <- x1
    for (k in 1L:K) {
      m <- nleaves(subtree[[k]])
      xx1 <- xx1 + (if (center) {
        (x2 - x1) * m / mTop
      } else {
        m
      })
      limit[k] <- xx1
    }
    limit <- c(x1, limit)
  }
  else {
    limit <- c(x1, x2)
  }
  mid <- attr(subtree, "midpoint")
  center <- center || (inner && !is.numeric(mid))
  x <- if (center) {
    mean(c(x1, x2))
  } else {
    x1 + (if (inner) {
      mid
    } else {
      0
    })
  }
  list(x = x, limit = limit)
}



.midDend2 <- function(x)
  if (is.null(mp <- attr(x, "midpoint"))) 0 else mp


plotNode2 <- function(x1, x2, subtree, type, center, leaflab, horiz = FALSE, nodes_xy) {
  inner <- !is.leaf(subtree) && x1 != x2
  yTop <- attr(subtree, "height")
  bx <- plotNodeLimit2(x1, x2, subtree, center)
  xTop <- bx$x

  if (getOption("verbose")) {
    cat(if (inner) {
      "inner node"
    } else {
      "leaf"
    }, ":")
    cat(if (inner) {
      paste(" height", formatC(yTop), "; ")
    }, "(x1,x2)= (",
    formatC(x1, width = 4), ",", formatC(x2, width = 4),
    ")", "--> xTop=", formatC(xTop, width = 8), "\n",
    sep = ""
    )
  }

  # update the xy coordinates using a function defined in the beginning!
  nodes_xy(c(xTop, yTop))



  i <- if (inner) {
    1
  } else {
    2
  }

  if (inner) {
    for (k in seq_along(subtree)) {
      child <- subtree[[k]]
      yBot <- attr(child, "height")
      if (getOption("verbose")) {
        cat("ch.", k, "@ h=", yBot, "; ")
      }
      if (is.null(yBot)) {
        yBot <- 0
      }
      xBot <- if (center) {
        mean(bx$limit[k:(k + 1)])
      } else {
        bx$limit[k] + .midDend2(child)
      }

      i <- if (!is.leaf(child)) {
        1
      } else {
        2
      }

      vln <- NULL

      if (!is.null(attr(child, "edgetext"))) {
        if (getOption("verbose")) {
          #                cat("-- with \"edgetext\"", format(edgeText))
          if (!is.null(vln)) {
            mx <- if (type == "triangle") {
              (xTop + xBot + ((xTop - xBot) / (yTop - yBot)) *
                vln) / 2
            } else {
              xBot
            }
            my <- (yTop + yBot + 2 * vln) / 2
          }
          else {
            mx <- if (type == "triangle") {
              (xTop + xBot) / 2
            } else {
              xBot
            }
            my <- (yTop + yBot) / 2
          }
        }

        #             vlm <- strheight(c(edgeText, "h"), cex = t.cex)/2
        #             hlm <- strwidth(c(edgeText, "m"), cex = t.cex)/2
        #             hl3 <- c(hlm[1L], hlm[1L] + hlm[2L], hlm[1L])
      }
      plotNode2(bx$limit[k], bx$limit[k + 1],
        subtree = child,
        type, center, leaflab, horiz, nodes_xy = nodes_xy
      )
    }
  }
  invisible()
}
