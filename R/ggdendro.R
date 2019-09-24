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

# Run first so that it would have access to all of these functions from stats.
# source("R\\stats_imports.R")

# This is a copy (import) of the function dendrogram_data, from Andrie de Vries's ggdendro package (which is a modified \link{plot.dendrogram}).
# https://cran.r-project.org/package=ggdendro
# This function is the basis for the as.ggdend function.
dendrogram_data <- function(x, type = c("rectangle", "triangle"), ...) {
  #    dev.new()
  #    plot(x) # we must add a new plot of the dend in order to extract features from it!

  # without the following line, we will get:
  #    Error in plot.xy(xy.coords(x, y), type = type, ...) :
  #       plot.new has not been called yet
  # unless a regular plot device was first initiated.
  # this is resolved by clearing the nodes from attributes!
  x <- x %>%
    remove_branches_edgePar() %>%
    remove_nodes_nodePar()
  #    x %>% remove_branches_edgePar %>% remove_nodes_nodePar %>% unclass

  leaflab <- "perpendicular"
  center <- FALSE
  xlab <- ""
  ylab <- ""
  horiz <- FALSE
  xaxt <- "n"
  yaxt <- "s"
  nodePar <- NULL
  edgePar <- list()
  dLeaf <- NULL
  edge.root <- is.leaf(x) || !is.null(attr(x, "edgetext"))
  type <- match.arg(type)
  hgt <- attr(x, "height")
  if (edge.root && is.logical(edge.root)) {
    edge.root <- 0.0625 * if (is.leaf(x)) {
      1
    } else {
      hgt
    }
  }
  mem.x <- .memberDend(x)
  yTop <- hgt + edge.root
  if (center) {
    x1 <- 0.5
    x2 <- mem.x + 0.5
  }
  else {
    x1 <- 1
    x2 <- mem.x
  }
  xl. <- c(x1 - 1 / 2, x2 + 1 / 2)
  yl. <- c(0, yTop)
  if (edge.root) {
    if (!is.null(et <- attr(x, "edgetext"))) {
      my <- mean(hgt, yTop)
    }
  }
  gg.plotNode <- function(x1, x2, subtree, type, center, leaflab,
                            dLeaf, nodePar, edgePar, horiz = FALSE, ddsegments = NULL,
                            ddlabels = NULL) {
    inner <- !is.leaf(subtree) && x1 != x2
    yTop <- attr(subtree, "height")
    bx <- plotNodeLimit(x1, x2, subtree, center)
    xTop <- bx$x
    hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
    if (!hasP) {
      nPar <- nodePar
    }
    Xtract <- function(nam, L, default, indx) rep(if (nam %in%
        names(L)) {
        L[[nam]]
      } else {
        default
      }, length.out = indx)[indx]
    asTxt <- function(x) {
      if (is.character(x) || is.expression(x)) {
        x
      } else if (is.null(x)) {
        ""
      } else {
        as.character(x)
      }
    }
    i <- if (inner || hasP) {
      1
    } else {
      2
    }
    if (!is.null(nPar)) {
      pch <- Xtract("pch", nPar, default = 1L:2, i)
      cex <- Xtract("cex", nPar, default = c(1, 1), i)
      col <- Xtract("col", nPar,
        default = par("col"),
        i
      )
      bg <- Xtract("bg", nPar, default = par("bg"), i)
      points(if (horiz) {
        cbind(yTop, xTop)
      } else {
        cbind(xTop, yTop)
      },
      pch = pch, bg = bg, col = col,
      cex = cex
      )
    }
    lab.cex <- 1
    if (is.leaf(subtree)) {
      if (leaflab == "perpendicular") {
        Y <- yTop - dLeaf * lab.cex
        X <- xTop
        srt <- 90
        adj <- 1
        nodeText <- asTxt(attr(subtree, "label"))
        ddlabels <- rbind(ddlabels, data.frame(
          x = X,
          y = 0, text = nodeText
        ))
      }
    }
    else if (inner) {
      segmentsHV <- function(x0, y0, x1, y1) {
        data.frame(x0, y0, x1, y1)
      }
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
          bx$limit[k] + .midDend(child)
        }
        if (type == "triangle") {
          ddsegments <- rbind(ddsegments, segmentsHV(
            xTop,
            yTop, xBot, yBot
          ))
        }
        else {
          ddsegments <- rbind(ddsegments, segmentsHV(
            xTop,
            yTop, xBot, yTop
          ))
          ddsegments <- rbind(ddsegments, segmentsHV(
            xBot,
            yTop, xBot, yBot
          ))
        }
        vln <- NULL
        if (!is.null(attr(child, "edgetext"))) {
          edgeText <- asTxt(attr(child, "edgetext"))
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
        plotNode_result <- gg.plotNode(bx$limit[k], bx$limit[k +
          1],
        subtree = child, type, center, leaflab,
        dLeaf, nodePar, edgePar, horiz, ddsegments,
        ddlabels
        )
        ddsegments <- plotNode_result$segments
        ddlabels <- plotNode_result$labels
      }
    }
    return(list(segments = ddsegments, labels = ddlabels))
  }
  ret <- gg.plotNode(x1, x2, x,
    type = type, center = center,
    leaflab = leaflab, dLeaf = dLeaf, nodePar = nodePar,
    edgePar = edgePar, horiz = FALSE, ddsegments = NULL,
    ddlabels = NULL
  )
  names(ret$segments) <- c("x", "y", "xend", "yend")
  names(ret$labels) <- c("x", "y", "label")



  #    dev.off() # Turn off device!

  ret
}






#' @title Creates completely blank theme in ggplot
#' @export
#' @description
#' Sets most of the ggplot options to blank, by returning blank theme elements for the panel grid, panel background, axis title, axis text, axis line and axis ticks.
#' @author Andrie de Vries
#' @seealso \link{ggdend}
#' @source
#'
#' This function is from Andrie de Vries's ggdendro package.
#'
#' The motivation for this fork is the need to add more graphical parameters to the plotted tree.
#' This required a strong mixter of functions from ggdendro and dendextend (to the point that
#' it seemed better to just fork the code into its current form)
#'
theme_dendro <- function() {
  # library(ggplot2)
  element_blank <- ggplot2::element_blank
  element_text <- ggplot2::element_text
  theme <- ggplot2::theme

  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.title.x = element_text(colour = NA),
    axis.title.y = element_blank(), axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.line = element_blank(),
    axis.ticks = element_blank()
  )
}
