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










### An internal function for plot_horiz.dendrogram
### This is basically "plotNode", but with options to make it work
### with horizontal trees.
plotNode_horiz <- function(x1, x2, subtree, type, center, leaflab, dLeaf, nodePar,
                           edgePar, horiz = FALSE,
                           text_pos = 2, text_offset = 0) {
  .midDend <- stats_.midDend
  plotNodeLimit <- stats_plotNodeLimit
  plotNode <- stats_plotNode

  inner <- !is.leaf(subtree) && x1 != x2
  yTop <- attr(subtree, "height")
  bx <- plotNodeLimit(x1, x2, subtree, center)
  xTop <- bx$x
  hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
  if (!hasP) {
    nPar <- nodePar
  }
  if (getOption("verbose")) {
    cat(if (inner) {
      "inner node"
    } else {
      "leaf"
    }, ":")
    if (!is.null(nPar)) {
      cat(" with node pars\n")
      str(nPar)
    }
    cat(if (inner) {
      paste(" height", formatC(yTop), "; ")
    }, "(x1,x2)= (",
    formatC(x1, width = 4), ",", formatC(x2, width = 4),
    ")", "--> xTop=", formatC(xTop, width = 8), "\n",
    sep = ""
    )
  }
  Xtract <- function(nam, L, default, indx) rep(if (nam %in%
      names(L)) {
      L[[nam]]
    } else {
      default
    }, length.out = indx)[indx]
  asTxt <- function(x) if (is.character(x) || is.expression(x) ||
      is.null(x)) {
      x
    } else {
      as.character(x)
    }
  i <- if (inner || hasP) {
    1
  } else {
    2
  }
  if (!is.null(nPar)) {
    pch <- Xtract("pch", nPar, default = 1L:2, i)
    cex <- Xtract("cex", nPar, default = c(1, 1), i)
    col <- Xtract("col", nPar, default = par("col"), i)
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
  if (leaflab == "textlike") {
    p.col <- Xtract("p.col", nPar, default = "white", i)
  }
  lab.col <- Xtract("lab.col", nPar,
    default = par("col"),
    i
  )
  lab.cex <- Xtract("lab.cex", nPar, default = c(1, 1), i)
  lab.font <- Xtract("lab.font", nPar,
    default = par("font"),
    i
  )
  lab.xpd <- Xtract("xpd", nPar, default = c(TRUE, TRUE), i)
  if (is.leaf(subtree)) {
    if (leaflab == "perpendicular") {
      if (horiz) {
        if (text_pos == 2) {
          X <- yTop - dLeaf * lab.cex ##############
        } else {
          X <- yTop + dLeaf * lab.cex ##############
        }
        Y <- xTop
        srt <- 0
        adj <- c(0, 0.5)
      }
      else {
        Y <- yTop - dLeaf * lab.cex
        X <- xTop
        srt <- 90
        adj <- 1
      }
      nodeText <- asTxt(attr(subtree, "label"))
      text(X, Y, nodeText,
        xpd = lab.xpd, srt = srt, adj = adj,
        cex = lab.cex, col = lab.col, font = lab.font,
        pos = text_pos, offset = text_offset
      ) ###########
    }
  }
  else if (inner) {
    segmentsHV <- function(x0, y0, x1, y1) {
      if (horiz) {
        segments(y0, x0, y1, x1,
          col = col, lty = lty,
          lwd = lwd
        )
      } else {
        segments(x0, y0, x1, y1,
          col = col, lty = lty,
          lwd = lwd
        )
      }
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
      hasE <- !is.null(ePar <- attr(child, "edgePar"))
      if (!hasE) {
        ePar <- edgePar
      }
      i <- if (!is.leaf(child) || hasE) {
        1
      } else {
        2
      }
      col <- Xtract("col", ePar,
        default = par("col"),
        i
      )
      lty <- Xtract("lty", ePar,
        default = par("lty"),
        i
      )
      lwd <- Xtract("lwd", ePar,
        default = par("lwd"),
        i
      )
      if (type == "triangle") {
        segmentsHV(xTop, yTop, xBot, yBot)
      }
      else {
        segmentsHV(xTop, yTop, xBot, yTop)
        segmentsHV(xBot, yTop, xBot, yBot)
      }
      vln <- NULL
      if (is.leaf(child) && leaflab == "textlike") {
        nodeText <- asTxt(attr(child, "label"))
        if (getOption("verbose")) {
          cat("-- with \"label\"", format(nodeText))
        }
        hln <- 0.6 * strwidth(nodeText, cex = lab.cex) / 2
        vln <- 1.5 * strheight(nodeText, cex = lab.cex) / 2
        rect(xBot - hln, yBot, xBot + hln, yBot + 2 *
          vln, col = p.col)
        text(xBot, yBot + vln, nodeText,
          xpd = lab.xpd,
          cex = lab.cex, col = lab.col, font = lab.font
        ) # , pos = text_pos)
      }
      if (!is.null(attr(child, "edgetext"))) {
        edgeText <- asTxt(attr(child, "edgetext"))
        if (getOption("verbose")) {
          cat("-- with \"edgetext\"", format(edgeText))
        }
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
        p.col <- Xtract("p.col", ePar,
          default = "white",
          i
        )
        p.border <- Xtract("p.border", ePar,
          default = par("fg"),
          i
        )
        p.lwd <- Xtract("p.lwd", ePar,
          default = lwd,
          i
        )
        p.lty <- Xtract("p.lty", ePar,
          default = lty,
          i
        )
        t.col <- Xtract("t.col", ePar,
          default = col,
          i
        )
        t.cex <- Xtract("t.cex", ePar, default = 1, i)
        t.font <- Xtract("t.font", ePar,
          default = par("font"),
          i
        )
        vlm <- strheight(c(edgeText, "h"), cex = t.cex) / 2
        hlm <- strwidth(c(edgeText, "m"), cex = t.cex) / 2
        hl3 <- c(hlm[1L], hlm[1L] + hlm[2L], hlm[1L])
        if (horiz) {
          polygon(my + c(-hl3, hl3), mx + sum(vlm) *
            c(-1L:1L, 1L:-1L),
          col = p.col, border = p.border,
          lty = p.lty, lwd = p.lwd
          )
          text(my, mx, edgeText,
            cex = t.cex, col = t.col,
            font = t.font
          ) # , pos = text_pos)
        }
        else {
          polygon(mx + c(-hl3, hl3), my + sum(vlm) *
            c(-1L:1L, 1L:-1L),
          col = p.col, border = p.border,
          lty = p.lty, lwd = p.lwd
          )
          text(mx, my, edgeText,
            cex = t.cex, col = t.col,
            font = t.font
          )
        }
      }
      # plotNode_horiz
      Recall(bx$limit[k], bx$limit[k + 1],
        subtree = child, #########
        type, center, leaflab, dLeaf, nodePar, edgePar,
        horiz, text_pos = text_pos, text_offset = text_offset
      ) ########
    }
  }
  invisible()
}







#' @title Plotting a left-tip-adjusted horizontal dendrogram
#' @export
#' @description
#' The default \code{plot(dend, horiz = TRUE)}, gives us a dendrogram tree plot
#' with the tips turned right. The current function enables the creation of
#' the same tree, but with the tips turned left. The main challange in doing this
#' is finding the distance of the labels from the leaves tips - which is solved
#' with this function.
#' @param x tree object (dendrogram)
#' @param type a character vector with either "rectangle" or "triangle" (passed to \link{plot.dendrogram})
#' @param center logical; if TRUE, nodes are plotted centered with respect to
#' the leaves in the branch. Otherwise (default), plot them in the
#'  middle of all direct child nodes.
#' @param edge.root logical; if true, draw an edge to the root node.
#' @param dLeaf a number specifying the distance in user coordinates between
#' the tip of a leaf and its label. If NULL as per default, 3/4 of a letter
#' width is used.
#' @param horiz logical indicating if the dendrogram should be
#' drawn horizontally or not. In this function it MUST be TRUE!
#' @param xaxt graphical parameters, or arguments for other methods.
#' @param yaxt graphical parameters, or arguments for other methods.
#' @param xlim (NULL) optional x- and y-limits of the plot, passed to plot.default.
#' The defaults for these show the full dendrogram.
#' @param ylim (NULL) optional x- and y-limits of the plot, passed to plot.default.
#' The defaults for these show the full dendrogram.
#' @param nodePar NULL.
#' @param edgePar list()
#' @param leaflab c("perpendicular", "textlike", "none")
#' @param side logical (TRUE). Should the tips of the drawn tree be facing
#' the left side. This is the important feature of this function.
#' @param text_pos integer from either 1 to 4 (2). Two relevant values
#' are 2 and 4. 2 (default) means that the labels are alligned to the
#' tips of the tree leaves. 4 will have the labels allign to the left,
#' making them look like they were when the tree was on the left side
#' (with leaves tips facing to the right).
#' @param ... passed to \link{plot}.
#' @return The invisiable dLeaf value.
#' @source
#' This function is based on replicating \link{plot.dendrogram}.
#' In fact, I'd be happy if in the future, some tweaks could be make to
#' \link{plot.dendrogram}, so that it would replace the need for this function.
#'
#' @seealso \link{plot.dendrogram}, \link{tanglegram}
#' @examples
#' \dontrun{
#' dend <- USArrests[1:10, ] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#'
#' par(mfrow = c(1, 2), mar = rep(6, 4))
#' plot_horiz.dendrogram(dend, side = FALSE)
#' plot_horiz.dendrogram(dend, side = TRUE)
#' # plot_horiz.dendrogram(dend, side=TRUE, dLeaf= 0)
#' # plot_horiz.dendrogram(dend, side=TRUE, nodePar = list(pos = 1))
#' # sadly, lab.pos is not implemented yet,
#' ## so the labels can not be right aligned...
#'
#'
#' plot_horiz.dendrogram(dend, side = F)
#' plot_horiz.dendrogram(dend, side = TRUE, dLeaf = 0, xlim = c(100, -10)) # bad
#' plot_horiz.dendrogram(dend, side = TRUE, text_offset = 0)
#' plot_horiz.dendrogram(dend, side = TRUE, text_offset = 0, text_pos = 4)
#' }
plot_horiz.dendrogram <- function(x,
                                  type = c("rectangle", "triangle"),
                                  center = FALSE,
                                  edge.root = is.leaf(x) || !is.null(attr(x, "edgetext")),
                                  dLeaf = NULL,
                                  horiz = TRUE,
                                  xaxt = "n", yaxt = "s",
                                  xlim = NULL, ylim = NULL,
                                  nodePar = NULL, edgePar = list(),
                                  leaflab = c(
                                    "perpendicular",
                                    "textlike", "none"
                                  ),
                                  side = TRUE,
                                  text_pos = 2,
                                  ...) {
  # reproduces plot.dendrogram in order to set the correct
  # strwidth for the labels when using revers horiz!
  # @param side logical (FALSE). To which direction should the dendrogram turn.
  #        if FALSE (default) then we will get the standard left side dendrogram.
  #        if TRUE, then we will have a right turning dendrogram.

  if (!is.dendrogram(x)) x <- as.dendrogram(x)
  if (!horiz) stop("This function was created ONLY for horiz==TRUE.")

  # if NOT side - then plot as usual
  if (!side) {
    plot(x,
      center = center,
      type = type, nodePar = nodePar, edgePar = edgePar, leaflab = leaflab,
      edge.root = edge.root,
      dLeaf = dLeaf,
      horiz = horiz,
      xaxt = xaxt, yaxt = yaxt,
      xlim = xlim, ylim = ylim, ...
    )
    return(invisible(NULL))
  }

  #######################
  ### The same as:
  ####  plot.dendrogram
  type <- match.arg(type)
  leaflab <- match.arg(leaflab)
  hgt <- attr(x, "height")
  if (edge.root && is.logical(edge.root)) {
    edge.root <- 0.0625 * if (is.leaf(x)) {
      1
    } else {
      hgt
    }
  }
  mem.x <- stats_.memberDend(x)
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
  if (horiz) {
    tmp <- xl.
    xl. <- rev(yl.)
    yl. <- tmp
    tmp <- xaxt
    xaxt <- yaxt
    yaxt <- tmp
  }
  if (missing(xlim) || is.null(xlim)) {
    xlim <- xl.
  }
  if (missing(ylim) || is.null(ylim)) {
    ylim <- yl.
  }
  dev.hold()
  on.exit(dev.flush())

  #######################
  ### NEW code

  # for right_side
  #    if(side)  {

  xlim <- rev(xlim)

  plot(0,
    xlim = xlim, ylim = ylim, type = "n", xlab = "",
    ylab = "", xaxt = xaxt, yaxt = yaxt, frame.plot = FALSE, ...
  ) # , axes = FALSE)


  labels_x <- labels(x)
  max_labels_x <- labels_x[which.max(nchar(labels_x))]
  base_dLeaf <- strwidth(max_labels_x)

  if (is.null(dLeaf)) {
    if (text_pos == 2) {
      dLeaf <- 0.75 * strwidth("w")
    } else {
      dLeaf <- -0.75 * strwidth("w") - base_dLeaf
    }
    ### (if (horiz) else strheight("x"))   ## this function gives ONLY horiz= TRUE
  } else {
    if (text_pos != 2) dLeaf <- dLeaf - base_dLeaf
  }

  #       par(new=TRUE)
  #    } # else {
  #       # the usual stuff...

  #   plot(x,
  #        horiz=horiz,
  #        center = center,
  #        edge.root = edge.root,
  #        dLeaf = dLeaf,
  #        xaxt = yaxt, yaxt = xaxt, # these are reversed in order to be re-reversed later due to the horiz=TRUE in this function...
  #        xlim=xlim, ylim=ylim,
  #        ...)
  #    if(side) par(new=FALSE)

  ###### back to the function as usual:
  if (edge.root) {
    x0 <- stats_plotNodeLimit(x1, x2, x, center)$x
    if (horiz) {
      segments(hgt, x0, yTop, x0)
    } else {
      segments(x0, hgt, x0, yTop)
    }
    if (!is.null(et <- attr(x, "edgetext"))) {
      my <- mean(hgt, yTop)
      if (horiz) {
        text(my, x0, et)
      } else {
        text(x0, my, et)
      }
    }
  }

  #    stats_plotNode
  # @param text_offset Numeric (NULL). This value gives the offset of the label
  # from the specified coordinate in fractions of a character width.
  # If NULL (default) then 3/4 of a letter width is used.
  #
  # This number overrides dLeaf.


  plotNode_horiz(x1, x2, x,
    type = type, center = center, leaflab = leaflab,
    dLeaf = dLeaf, nodePar = nodePar, edgePar = edgePar,
    horiz = horiz, text_pos = text_pos
  )

  return(invisible(dLeaf))
}

# # stats_plot.dendrogram
# # stats_plotNode












#' @title Tanglegram plot
#' @export
#' @rdname tanglegram
#' @description
#' Plots a tanglegram plot of a side by side trees.
#'
#' @author Tal Galili, Johan Renaudie
#'
#'
#' @param dend1 tree object (dendrogram/dendlist/hclust/phylo), plotted on the left
#' @param dend2 tree object (dendrogram/hclust/phylo), plotted on the right
#' @param which an integer vector of length 2, indicating
#' which of the trees in the dendlist object should be plotted
#' @param sort logical (FALSE). Should the dendrogram's labels be "sorted"?
#' (might give a better tree in some cases).
#' @param color_lines a vector of colors for the lines connected the labels.
#' If the colors are shorter than the number of labels, they are recycled
#' (and a warning is issued).
#' The colors in the vector are applied on the lines from the bottom up.
#' @param lwd width of the lines connecting the labels. (default is 3.5)
#' @param edge.lwd width of the dendrograms lines. Default is NULL.
#' If set, then it switches `highlight_branches_lwd` to FALSE. If you want thicker
#' lines which reflect the height, please use \link{highlight_branches_lwd} on the
#' dendrograms/dendlist.
#' @param columns_width a vector with three elements, giving the relative
#' sizes of the the three plots (left dendrogram, connecting lines,
#' right dendrogram). This is passed to \link{layout} if parameter just_one is TRUE.
#' The default is: c(5,3,5)
#' @param margin_top  the number of lines of margin to be specified on the top
#' of the plots.
#' @param margin_bottom the number of lines of margin to be specified on the
#' bottom of the plots.
#' @param margin_inner margin_bottom the number of lines of margin
#' to be specified on the inner distence between the dendrograms
#' and the connecting lines.
#' @param margin_outer margin_bottom the number of lines of margin
#' to be specified on the outer distence between the dendrograms
#' and the connecting lines.
#' @param left_dendo_mar mar parameters of the left dendrgoram.
#' @param right_dendo_mar mar parameters of the right dendrgoram.
#' @param intersecting logical (TRUE). Should the leaves of the two dendrograms
#' be pruned so that the two trees will have the same labels?
#' @param dLeaf a number specifying the distance in user coordinates between
#' the tip of a leaf and its label. If NULL, as per default,
#' 3/4 of a letter width or height is used.
#'
#' Notice that if we are comparing two dendrograms with different
#' heights, manually changing dLeaf will affect both trees differently.
#' In such a case, it is recommanded to manually change dLeaf_left
#' and dLeaf_right.
#' This can be especially important when changing the lab.cex of the
#' dendrogram's labels.
#' Alternatively, one could manually set the xlim parameter for both
#' trees, which will force the proportion of distances of the
#' labels from the trees to remain the same.
#'
#' @param dLeaf_left dLeaf of the left dendrogram, by default it is equal to dLeaf (often negative).
#' @param dLeaf_right dLeaf of the right dendrogram, by default it is equal to minus dLeaf (often positive).
#' @param axes logical (TRUE). Should plot axes be plotted?
#' @param type type of plot ("t"/"r" = triangle or rectangle)
#' @param lab.cex numeric scalar, influanicing the cex size of the labels.
#' @param remove_nodePar logical (FALSE). Should the nodePar of the leaves be
#' removed? (useful when the trees' leaves has too many parameters on them)
#' @param main Character. Title above the connecting lines.
#' @param main_left Character. Title of the left dendrogram.
#' @param main_right Character. Title of the right dendrogram.
#' @param sub Character. Title below the connecting lines.
#' @param k_labels integer. Number of groups by which to color the leaves.
#' @param k_branches integer. Number of groups by which to color the branches.
#' @param rank_branches logical (FALSE). Should the branches heights be adjusted?
#' (setting this to TRUE - can make it easier for
#' comparing topological differences)
#' @param hang logical (FALSE). Should we hang the leaves of the trees?
#' @param match_order_by_labels logical (TRUE). Should the leaves value order
#' be matched between the two trees based on labels? This is a MUST in order
#' to have the lines connect the correct labels. Set this to FALSE if you
#' want to make the plotting a bit faster, and only after you are sure
#' the labels and orders are correctly aligned.
#' @param cex_main A numerical value giving the amount by which plotting title
#' should be magnified relative to the default.
#' @param cex_main_left see cex_main.
#' @param cex_main_right see cex_main.
#' @param cex_sub see cex_main.
#' @param highlight_distinct_edges logical (default is TRUE). If to highlight distinct edges in each tree (by changing their line types to 2).
#' (notice that this can be slow on large trees)
#'
#' This parameter will automatically be turned off if the tree already comes with a "lty" edgePar
#' (this is checked using \link{has_edgePar}). A "lty" can be removed by using set("clear_branches"), by
#' removing all of the edgePar parameters of the dendrogram.
#'
#' @param common_subtrees_color_lines logical (default is TRUE). color the connecting line based on the common subtrees of both dends.
#' This only works if
#' (notice that this can be slow on large trees)
#' @param common_subtrees_color_lines_default_single_leaf_color When representing edges between common subtrees 
#' (i.e. common_subtrees_color_branches = TRUE), this parameter sets the color of edges for subtrees that are NOT common.
#' Default is "grey"
#' @param common_subtrees_color_branches logical (default is FALSE).
#' Color the branches of both dends based on the common subtrees.
#' (notice that this can be slow on large trees)
#' This is FALSE by default since it will override the colors of the existing tree.
#' @param highlight_branches_col logical (default is FALSE). Should \link{highlight_branches_col} be used on the dendrograms.
#'
#' This parameter will automatically be turned off if the tree already comes with a "col" edgePar
#' (this is checked using \link{has_edgePar}). A "lty" can be removed by using set("clear_branches"), by
#' removing all of the edgePar parameters of the dendrogram.
#'
#' @param highlight_branches_lwd logical (default is TRUE). Should \link{highlight_branches_lwd} be used on the dendrograms.
#'
#' This parameter will automatically be turned off if the tree already comes with a "lwd" edgePar
#' (this is checked using \link{has_edgePar}). A "lty" can be removed by using set("clear_branches"), by
#' removing all of the edgePar parameters of the dendrogram.
#'
#' @param faster logical (FALSE). If TRUE, it overrides some other parameters to
#' have them turned off so that the plotting will go a tiny bit faster.
#' @param just_one logical (TRUE). If FALSE, it means at least two tanglegrams
#' will be plotted on the same page and so \link{layout} is not passed.
#' See: \url{https://stackoverflow.com/q/39784746/4137985}
#' @param ... not used.
#' @details
#' Notice that tanglegram does not "resize" well. In case you are resizing your
#' window you would need to re-run the function.
#'
#' @return An invisible \link{dendlist}, with two trees after being
#' modified during the creation of the tanglegram.
#' @source
#' The function is based on code from Johan Renaudie (plannapus), after major revisions. See:
#' \url{https://stackoverflow.com/questions/12456768/duelling-dendrograms-in-r-placing-dendrograms-back-to-back-in-r}
#'
#' As far as I could tell, this code was originally inspired by Dylan Beaudette
#' function \code{dueling.dendrograms} from the sharpshootR package:
#' \url{https://CRAN.R-project.org/package=sharpshootR}
#' tanglegram
#' @seealso \link{remove_leaves_nodePar}, \link{plot_horiz.dendrogram}, \link{rank_branches},
#' \link{hang.dendrogram}
#' @examples
#' \dontrun{
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#' dend1 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("com") %>%
#'   as.dendrogram()
#' dend2 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("sin") %>%
#'   as.dendrogram()
#' dend12 <- dendlist(dend1, dend2)
#'
#' dend12 %>% tanglegram()
#'
#' tanglegram(dend1, dend2)
#' tanglegram(dend1, dend2, sort = TRUE)
#' tanglegram(dend1, dend2, remove_nodePar = TRUE)
#' tanglegram(dend1, dend2, k_labels = 6, k_branches = 4)
#'
#' tanglegram(dend1, dend2,
#'   lab.cex = 2, edge.lwd = 3,
#'   margin_inner = 5, type = "t", center = TRUE
#' )
#'
#'
#' ## works nicely:
#' tanglegram(dend1, dend2,
#'   lab.cex = 2, edge.lwd = 3,
#'   margin_inner = 3.5, type = "t", center = TRUE,
#'   dLeaf = -0.1, xlim = c(7, 0),
#'   k_branches = 3
#' )
#'
#'
#' # using rank_branches can make the comparison even easier
#' tanglegram(rank_branches(dend1), rank_branches(dend2),
#'   lab.cex = 2, edge.lwd = 3,
#'   margin_inner = 3.5, type = "t", center = TRUE,
#'   dLeaf = -0.1, xlim = c(5.1, 0), columns_width = c(5, 1, 5),
#'   k_branches = 3
#' )
#'
#'
#'
#' ########
#' ## Nice example of some colored trees
#'
#' # see the coloring of common sub trees:
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#' dend1 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("com") %>%
#'   as.dendrogram()
#' dend2 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("sin") %>%
#'   as.dendrogram()
#' dend12 <- dendlist(dend1, dend2)
#' # dend12 %>% untangle %>% tanglegram
#' dend12 %>% tanglegram(common_subtrees_color_branches = TRUE)
#'
#'
#' set.seed(22133513)
#' ss <- sample(1:150, 10)
#' dend1 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("com") %>%
#'   as.dendrogram()
#' dend2 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust("sin") %>%
#'   as.dendrogram()
#' dend12 <- dendlist(dend1, dend2)
#' # dend12 %>% untangle %>% tanglegram
#' dend12 %>% tanglegram(common_subtrees_color_branches = TRUE)
#' dend12 %>% tanglegram()
#' }
tanglegram <- function(dend1, ...) {
  UseMethod("tanglegram")
}

#' @export
#' @rdname tanglegram
tanglegram.default <- function(dend1, ...) {
  stop("No default function for tanglegram - must use a dendrogram/hclust/phylo object")
}

#' @export
#' @rdname tanglegram
tanglegram.hclust <- function(dend1, ...) {
  tanglegram.dendrogram(dend1 = dend1, ...)
}

#' @export
#' @rdname tanglegram
tanglegram.phylo <- function(dend1, ...) {
  tanglegram.dendrogram(dend1 = dend1, ...)
}

#' @export
#' @rdname tanglegram
tanglegram.dendlist <- function(dend1, which = c(1L, 2L), main_left, main_right, just_one = TRUE, ...) {
  # many things can go wrong here (which we might wish to fix):
  # we could have parameter just_one set to FALSE but no layout predefined, in which case we can't plot
  if (!just_one & identical(par("mfrow"), c(1, 1))) stop("A layout must be defined when just_one is FALSE")
  # we could get a dendlist with a length of 1 - in which case, we can't plot
  if (length(dend1) == 1) stop("Your dendlist has only 1 dendrogram - a tanglegram can not be plotted")
  # we could get a dendlist with a length of >2 - in which case, should we only plot the first two items?
  if (all(which %in% seq_len(length(dend1)))) {
    l1 <- which[1]
    l2 <- which[2]

    if (!is.null(names(dend1))) {
      if (missing(main_left)) main_left <- names(dend1)[l1]
      if (missing(main_right)) main_right <- names(dend1)[l2]
    } else {
      if (missing(main_left)) main_left <- ""
      if (missing(main_right)) main_right <- ""
    }

    tanglegram.dendrogram(dend1[[l1]], dend1[[l2]], main_left = main_left, main_right = main_right, just_one = just_one, ...)
  } else {
    stop("You are trying to plot trees which are outside the range of trees in your dendlist")
  }
}


#' @export
#' @rdname tanglegram
tanglegram.dendrogram <- function(dend1, dend2, sort = FALSE,
                                  color_lines,
                                  lwd = 3.5,
                                  edge.lwd = NULL,
                                  # columns_width = c(5, 2, 3, 2, 5),
                                  columns_width = c(5, 3, 5),
                                  margin_top = 3,
                                  margin_bottom = 2.5,
                                  margin_inner = 3,
                                  margin_outer = 0.5,
                                  left_dendo_mar = c(margin_bottom, margin_outer, margin_top, margin_inner),
                                  right_dendo_mar = c(margin_bottom, margin_inner, margin_top, margin_outer),
                                  intersecting = TRUE,
                                  dLeaf = NULL, # -.3,
                                  dLeaf_left = dLeaf,
                                  dLeaf_right = dLeaf,
                                  axes = TRUE,
                                  type = "r", # can also be "t"
                                  lab.cex = NULL,
                                  remove_nodePar = FALSE,
                                  main = "",
                                  main_left = "",
                                  main_right = "",
                                  sub = "",
                                  k_labels = NULL,
                                  k_branches = NULL,
                                  rank_branches = FALSE,
                                  hang = FALSE,
                                  match_order_by_labels = TRUE,
                                  cex_main = 2,
                                  cex_main_left = cex_main,
                                  cex_main_right = cex_main,
                                  cex_sub = cex_main,
                                  highlight_distinct_edges = TRUE,
                                  common_subtrees_color_lines = TRUE,
                                  common_subtrees_color_lines_default_single_leaf_color = "grey",
                                  common_subtrees_color_branches = FALSE,
                                  highlight_branches_col = FALSE,
                                  highlight_branches_lwd = TRUE,
                                  faster = FALSE,
                                  just_one = TRUE,
                                  ...) {
  if (faster) {
    highlight_distinct_edges <- FALSE
    common_subtrees_color_lines <- FALSE
    common_subtrees_color_branches <- FALSE
    highlight_branches_lwd <- FALSE
  }


  if (just_one) {
    # save default, for resetting...
    def_par <- par(no.readonly = TRUE)
  }



  # characters_to_prune = the number of characters to leave after pruning the labels.
  # remove_nodePar = makes sure that we won't have any dots at the end of leaves

  if (!is.dendrogram(dend1)) dend1 <- as.dendrogram(dend1)
  if (!is.dendrogram(dend2)) dend2 <- as.dendrogram(dend2)

  # remove colors from the tips of leaves
  if (remove_nodePar) {
    dend1 <- remove_leaves_nodePar(dend1)
    dend2 <- remove_leaves_nodePar(dend2)
  }
  # sort them for better graph
  if (sort == TRUE) { # based on the "rotate.dendrogram" function
    dend1 <- sort(dend1)
    dend2 <- sort(dend2)
  }
  if (intersecting) {
    dend12 <- intersect_trees(dend1, dend2, warn = TRUE)
    dend1 <- dend12[[1]]
    dend2 <- dend12[[2]]
  }
  # adjust labels cex:
  if (!is.null(lab.cex)) {
    dend1 <- assign_values_to_leaves_nodePar(dend1, lab.cex, "lab.cex", warn = dendextend_options("warn"))
    dend2 <- assign_values_to_leaves_nodePar(dend2, lab.cex, "lab.cex", warn = dendextend_options("warn"))
  }
  if (!is.null(edge.lwd)) {
    highlight_branches_lwd <- FALSE # so that it does not override this parameter
    dend1 <- assign_values_to_branches_edgePar(dend1, edge.lwd, "lwd")
    dend2 <- assign_values_to_branches_edgePar(dend2, edge.lwd, "lwd")
  }
  if (!is.null(k_labels)) {
    dend1 <- color_labels(dend1, k = k_labels)
    dend2 <- color_labels(dend2, k = k_labels)
  }
  if (!is.null(k_branches)) {
    dend1 <- color_branches(dend1, k = k_branches)
    dend2 <- color_branches(dend2, k = k_branches)
  }
  if (rank_branches) {
    dend1 <- rank_branches(dend1)
    dend2 <- rank_branches(dend2)
  }
  # MUST
  if (hang) {
    dend1 <- hang.dendrogram(dend1)
    dend2 <- hang.dendrogram(dend2)
  }

  if (highlight_distinct_edges) {
    ##
    if (!has_edgePar(dend1, "lty")) dend1 <- highlight_distinct_edges(dend1, dend2, edgePar = "lty", value = 3)
    if (!has_edgePar(dend2, "lty")) dend2 <- highlight_distinct_edges(dend2, dend1, edgePar = "lty", value = 3)
  }


  if (common_subtrees_color_branches) {
    clusters1 <- common_subtrees_clusters(dend1, dend2)
    # clusters2 <- common_subtrees_clusters(dend2, dend1)
    dend1 <- color_branches(dend1, clusters = clusters1)
    # dend2 <- color_branches(dend2, clusters = clusters2)
    dend1_leaves_colors <- get_leaves_branches_col(dend1)

    # in cases when lwd is defined, the NAs are not provided and the two vectors
    # (clusters1 and dend1_leaves_colors) have different lengths.
    # I repeat this to all cases by removing the NAs. Knowing where they should be,
    # based on the 0s in clusters1, I am able to fit the colors properly in both cases.
    dend1_leaves_colors <- as.vector(na.omit(dend1_leaves_colors))
    tmp <- clusters1
    tmp[tmp != 0] <- dend1_leaves_colors
    dend1_leaves_colors <- tmp
    dend1_leaves_colors[tmp == 0] <- "black"

    # match_1_to_be_2
    ss <- match(labels(dend2), labels(dend1))
    #       labels(dend1)[ss]
    #       labels(dend2)
    the_leaves_colors <- dend1_leaves_colors[ss]
    # the_leaves_colors[is.na(the_leaves_colors)] <- 1
    dend2_clusters <- rank_values_with_clusters(clusters1[ss], ignore0 = TRUE)
    dend2 <- branches_attr_by_clusters(dend2, dend2_clusters,
      values = the_leaves_colors, attr = "col",
      branches_changed_have_which_labels = "all"
    )

    # tanglegram(dend1,dend2)
    #       a <- clusters1[ss]
    #       a[c(1,2,6)] <- 4:6
    #       b <- dend1_leaves_colors[ss]
    #       b[is.na(b)] <- "black"
    #       branches_attr_by_clusters(dend2, a,
    #                                          values = b, attr = "col")



    # If I know I am using the common_subtrees_color_branches
    # I might as well match them to the lines:
    if (common_subtrees_color_lines) {
      color_lines <- dend1_leaves_colors
      color_lines[is.na(color_lines)] <- common_subtrees_color_lines_default_single_leaf_color
    }
  }

  # if we didn't resolve color_lines yet - let's figure it out now:
  if (missing(color_lines)) {
    if (common_subtrees_color_lines) {
      #          color_lines <- rep("black", nleaves(dend1))
      #          lines_color_clusters <- common_subtrees_clusters(dend1, dend2, leaves_get_0_cluster = TRUE)
      #          ss_not_0s <- lines_color_clusters != 0
      #          colors_for_lines_color <- lines_color_clusters[ss_not_0s] %>% unique %>% length %>% rainbow_fun
      #          color_lines[ss_not_0s] <- colors_for_lines_color[lines_color_clusters[ss_not_0s]]

      lines_color_clusters <- common_subtrees_clusters(dend1, dend2, leaves_get_0_cluster = FALSE)
      colors_for_lines_color <- lines_color_clusters %>%
        unique() %>%
        length() %>%
        rainbow_fun()
      color_lines <- colors_for_lines_color[lines_color_clusters]

      ss_0s <- replace_unique_items_with_0_and_rank(lines_color_clusters) == 0
      color_lines[ss_0s] <- common_subtrees_color_lines_default_single_leaf_color
    } else {
      color_lines <- "darkgrey"
    }
  }

  if (highlight_branches_col) {
    if (!has_edgePar(dend1, "col")) dend1 <- highlight_branches_col(dend1)
    if (!has_edgePar(dend2, "col")) dend2 <- highlight_branches_col(dend2)
  }
  if (highlight_branches_lwd) {
    if (!has_edgePar(dend1, "lwd")) dend1 <- highlight_branches_lwd(dend1)
    if (!has_edgePar(dend2, "lwd")) dend2 <- highlight_branches_lwd(dend2)
  }


  l <- nleaves(dend1)

  # makes sure that dLeaf gives a symmetric result.
  if (!is.null(dLeaf) && dLeaf_right == dLeaf_left) {
    dLeaf_right <- -dLeaf_left
  }

  ##########################################
  #####  Plotting.
  ##########################################

  labels_dend1 <- labels(dend1)
  max_labels_dend1 <- labels_dend1[which.max(nchar(labels_dend1))]


  # The matrix to draw the arrows:
  if (match_order_by_labels) dend1 <- match_order_by_labels(dend1, dend2)
  ord_arrow <- cbind((1:l)[order(order.dendrogram(dend1))], (1:l)[order(order.dendrogram(dend2))])

  # Set the layout of the plot elements
  if (just_one) layout(matrix(1:3, nrow = 1), widths = columns_width)

  #################
  # The first dendrogram:
  #################
  par(mar = left_dendo_mar)
  plot(dend1,
    horiz = TRUE, ylim = c(0, l),
    dLeaf = dLeaf_left,
    type = type, axes = axes,
    main = main_left,
    cex.main = cex_main_left,
    # ...)
    #         leaflab="none",
    yaxs = "r", xaxs = "i", ...
  ) # this might be causing bugs when refreshing a resized window.

  # now that I have a plot to use, I can calculate strwidth
  # this ASSUMES that both tree plots are of the same size/shape...

  #################
  # The arrows:
  #################
  # arros colors:
  if (length(color_lines) < l) color_lines <- rep.int(color_lines, l)
  color_lines <- color_lines[ord_arrow[, 1]]

  par(mar = c(margin_bottom, 0, margin_top, 0))
  plot(NA,
    bty = "n", axes = FALSE, xlim = c(0, 1), ylim = c(0, l), ylab = "", xlab = "", # )#,
    yaxs = "r", xaxs = "i"
  )
  col_indx <- 0
  apply(
    ord_arrow, 1,
    function(x) {
      col_indx <<- col_indx + 1
      arrows(0, x[1], 1, x[2], code = 0, length = 0.05, col = color_lines[col_indx], lwd = lwd)
    }
  )
  mtext(main, side = 3, cex = cex_main)
  mtext(sub, side = 1, cex = cex_sub)

  #################
  # And the second dendrogram (to reverse it I reversed the xlim vector:
  #################
  par(mar = right_dendo_mar)
  plot_horiz.dendrogram(dend2,
    side = TRUE, dLeaf = dLeaf_right,
    type = type, axes = axes,
    ylim = c(0, l),
    cex.main = cex_main_left,
    main = main_right,
    # ...)
    #         leaflab="none",
    yaxs = "r", xaxs = "i", ...
  )

  # layout(matrix(1)) # not required

  if (just_one) {
    par(def_par) #- reset to default
  }



  return(invisible(dendlist(dend1 = dend1, dend2 = dend2)))
}



# add an "abbreviation" parameter - to prune the labels of two trees.



#' @export
#' @rdname tanglegram
dendbackback <- tanglegram.dendrogram # another name for the same function.
# hclustbackback <- tanglegram.hclust # another name for the same function.
