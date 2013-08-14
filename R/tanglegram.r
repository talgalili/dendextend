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
plotNode_horiz <- function (x1, x2, subtree, type, center, leaflab, dLeaf, nodePar, 
                            edgePar, horiz = FALSE, 
                            text_pos = 2, text_offset = 0) 
{
   .midDend <- stats:::.midDend
   plotNodeLimit <- stats:::plotNodeLimit
   plotNode <- stats:::plotNode
   
   inner <- !is.leaf(subtree) && x1 != x2
   yTop <- attr(subtree, "height")
   bx <- plotNodeLimit(x1, x2, subtree, center)
   xTop <- bx$x
   hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
   if (!hasP) 
      nPar <- nodePar
   if (getOption("verbose")) {
      cat(if (inner) 
         "inner node"
          else "leaf", ":")
      if (!is.null(nPar)) {
         cat(" with node pars\n")
         str(nPar)
      }
      cat(if (inner) 
         paste(" height", formatC(yTop), "; "), "(x1,x2)= (", 
          formatC(x1, width = 4), ",", formatC(x2, width = 4), 
          ")", "--> xTop=", formatC(xTop, width = 8), "\n", 
          sep = "")
   }
   Xtract <- function(nam, L, default, indx) rep(if (nam %in% 
                                                        names(L)) L[[nam]] else default, length.out = indx)[indx]
   asTxt <- function(x) if (is.character(x) || is.expression(x) || 
                               is.null(x)) 
      x
   else as.character(x)
   i <- if (inner || hasP) 
      1
   else 2
   if (!is.null(nPar)) {
      pch <- Xtract("pch", nPar, default = 1L:2, i)
      cex <- Xtract("cex", nPar, default = c(1, 1), i)
      col <- Xtract("col", nPar, default = par("col"), i)
      bg <- Xtract("bg", nPar, default = par("bg"), i)
      points(if (horiz) 
         cbind(yTop, xTop)
             else cbind(xTop, yTop), pch = pch, bg = bg, col = col, 
             cex = cex)
   }
   if (leaflab == "textlike") 
      p.col <- Xtract("p.col", nPar, default = "white", i)
   lab.col <- Xtract("lab.col", nPar, default = par("col"), 
                     i)
   lab.cex <- Xtract("lab.cex", nPar, default = c(1, 1), i)
   lab.font <- Xtract("lab.font", nPar, default = par("font"), 
                      i)
   lab.xpd <- Xtract("xpd", nPar, default = c(TRUE, TRUE), i)
   if (is.leaf(subtree)) {
      if (leaflab == "perpendicular") {
         if (horiz) {
            if(text_pos==2) {
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
         text(X, Y, nodeText, xpd = lab.xpd, srt = srt, adj = adj, 
              cex = lab.cex, col = lab.col, font = lab.font, 
              pos = text_pos, offset = text_offset) ###########
      }
   }
   else if (inner) {
      segmentsHV <- function(x0, y0, x1, y1) {
         if (horiz) 
            segments(y0, x0, y1, x1, col = col, lty = lty, 
                     lwd = lwd)
         else segments(x0, y0, x1, y1, col = col, lty = lty, 
                       lwd = lwd)
      }
      for (k in seq_along(subtree)) {
         child <- subtree[[k]]
         yBot <- attr(child, "height")
         if (getOption("verbose")) 
            cat("ch.", k, "@ h=", yBot, "; ")
         if (is.null(yBot)) 
            yBot <- 0
         xBot <- if (center) 
            mean(bx$limit[k:(k + 1)])
         else bx$limit[k] + .midDend(child)
         hasE <- !is.null(ePar <- attr(child, "edgePar"))
         if (!hasE) 
            ePar <- edgePar
         i <- if (!is.leaf(child) || hasE) 
            1
         else 2
         col <- Xtract("col", ePar, default = par("col"), 
                       i)
         lty <- Xtract("lty", ePar, default = par("lty"), 
                       i)
         lwd <- Xtract("lwd", ePar, default = par("lwd"), 
                       i)
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
            if (getOption("verbose")) 
               cat("-- with \"label\"", format(nodeText))
            hln <- 0.6 * strwidth(nodeText, cex = lab.cex)/2
            vln <- 1.5 * strheight(nodeText, cex = lab.cex)/2
            rect(xBot - hln, yBot, xBot + hln, yBot + 2 * 
                    vln, col = p.col)
            text(xBot, yBot + vln, nodeText, xpd = lab.xpd, 
                 cex = lab.cex, col = lab.col, font = lab.font) # , pos = text_pos)
         }
         if (!is.null(attr(child, "edgetext"))) {
            edgeText <- asTxt(attr(child, "edgetext"))
            if (getOption("verbose")) 
               cat("-- with \"edgetext\"", format(edgeText))
            if (!is.null(vln)) {
               mx <- if (type == "triangle") 
                  (xTop + xBot + ((xTop - xBot)/(yTop - yBot)) * 
                      vln)/2
               else xBot
               my <- (yTop + yBot + 2 * vln)/2
            }
            else {
               mx <- if (type == "triangle") 
                  (xTop + xBot)/2
               else xBot
               my <- (yTop + yBot)/2
            }
            p.col <- Xtract("p.col", ePar, default = "white", 
                            i)
            p.border <- Xtract("p.border", ePar, default = par("fg"), 
                               i)
            p.lwd <- Xtract("p.lwd", ePar, default = lwd, 
                            i)
            p.lty <- Xtract("p.lty", ePar, default = lty, 
                            i)
            t.col <- Xtract("t.col", ePar, default = col, 
                            i)
            t.cex <- Xtract("t.cex", ePar, default = 1, i)
            t.font <- Xtract("t.font", ePar, default = par("font"), 
                             i)
            vlm <- strheight(c(edgeText, "h"), cex = t.cex)/2
            hlm <- strwidth(c(edgeText, "m"), cex = t.cex)/2
            hl3 <- c(hlm[1L], hlm[1L] + hlm[2L], hlm[1L])
            if (horiz) {
               polygon(my + c(-hl3, hl3), mx + sum(vlm) * 
                          c(-1L:1L, 1L:-1L), col = p.col, border = p.border, 
                       lty = p.lty, lwd = p.lwd)
               text(my, mx, edgeText, cex = t.cex, col = t.col, 
                    font = t.font) # , pos = text_pos)
            }
            else {
               polygon(mx + c(-hl3, hl3), my + sum(vlm) * 
                          c(-1L:1L, 1L:-1L), col = p.col, border = p.border, 
                       lty = p.lty, lwd = p.lwd)
               text(mx, my, edgeText, cex = t.cex, col = t.col, 
                    font = t.font)
            }
         }
         # plotNode_horiz
         Recall(bx$limit[k], bx$limit[k + 1], subtree = child,  #########
                        type, center, leaflab, dLeaf, nodePar, edgePar, 
                        horiz, text_pos = text_pos, text_offset = text_offset) ########
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
#' @param type
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
#' @seealso \link{plot.dendrogram}, \link{tangelgram}
#' @examples
#' \dontrun{
#' dend <- as.dendrogram(hclust(dist(USArrests[1:10,])))
#' 
#' par(mfrow =c(1,2), mar = rep(6,4))
#' plot_horiz.dendrogram(dend, side=FALSE) 
#' plot_horiz.dendrogram(dend, side=TRUE) 
#' # plot_horiz.dendrogram(dend, side=TRUE, dLeaf= 0) 
#' # plot_horiz.dendrogram(dend, side=TRUE, nodePar = list(pos = 1)) 
#' # sadly, lab.pos is not implemented yet, 
#' ## so the labels can not be right aligned...
#' 
#' 
#' plot_horiz.dendrogram(dend, side=F)
#' plot_horiz.dendrogram(dend, side=TRUE, dLeaf=0, xlim = c(100,-10)) # bad
#' plot_horiz.dendrogram(dend, side=TRUE, text_offset = 0)
#' plot_horiz.dendrogram(dend, side=TRUE, text_offset = 0,text_pos=4)
#' 
#' }
plot_horiz.dendrogram <- function (x, 
                                   type = c("rectangle", "triangle"),
                                   center = FALSE, 
                                   edge.root = is.leaf(x) || !is.null(attr(x, "edgetext")),                                    
                                   dLeaf = NULL, 
                                   horiz = TRUE, 
                                   xaxt = "n", yaxt = "s", 
                                   xlim = NULL, ylim = NULL, 
                                   nodePar = NULL, edgePar = list(),
                                   leaflab = c("perpendicular", 
                                               "textlike", "none"),
                                   side = TRUE,
                                   text_pos = 2,
                                   ...) {
   # reproduces plot.dendrogram in order to set the correct 
   # strwidth for the labels when using revers horiz!
   # @param side logical (FALSE). To which direction should the dendrogram turn.
   #        if FALSE (default) then we will get the standard left side dendrogram.
   #        if TRUE, then we will have a right turning dendrogram.
   
   if(!is.dendrogram(x)) x <- as.dendrogram(x)
   if(!horiz) stop("This function was created ONLY for horiz==TRUE.")
   
   # if NOT side - then plot as usual
   if(!side) {
      plot(x, center = center, 
           type= type,nodePar=nodePar,edgePar=edgePar,leaflab=leaflab,
                  edge.root = edge.root,
                  dLeaf = dLeaf, 
                  horiz = horiz, 
                  xaxt = xaxt, yaxt = yaxt, 
                  xlim=xlim, ylim=ylim, ...)
      return(invisible(NULL))
   }
   
   #######################
   ### The same as:
   ####  plot.dendrogram
   type <- match.arg(type)
   leaflab <- match.arg(leaflab)
   hgt <- attr(x, "height")
   if (edge.root && is.logical(edge.root)) 
      edge.root <- 0.0625 * if (is.leaf(x)) {1} else {hgt}
   mem.x <- stats:::.memberDend(x)
   yTop <- hgt + edge.root
   if (center) {
      x1 <- 0.5
      x2 <- mem.x + 0.5
   }
   else {
      x1 <- 1
      x2 <- mem.x
   }
   xl. <- c(x1 - 1/2, x2 + 1/2)
   yl. <- c(0, yTop)
   if (horiz) {
      tmp <- xl.
      xl. <- rev(yl.)
      yl. <- tmp
      tmp <- xaxt
      xaxt <- yaxt
      yaxt <- tmp
   }
   if (missing(xlim) || is.null(xlim)) 
      xlim <- xl.
   if (missing(ylim) || is.null(ylim)) 
      ylim <- yl.
   dev.hold()
   on.exit(dev.flush())

   #######################
   ### NEW code
   
   # for right_side 
   #    if(side)  {
   
   xlim <- rev(xlim)
   
   plot(0, xlim = xlim, ylim = ylim, type = "n", xlab = "", 
        ylab = "", xaxt = xaxt, yaxt = yaxt, frame.plot = FALSE,...) # , axes = FALSE)
   
   
   labels_x <- labels(x)
   max_labels_x <- labels_x[which.max(nchar(labels_x))]
   base_dLeaf <- strwidth(max_labels_x)
   
   if (is.null(dLeaf)) {
      if(text_pos == 2) {
         dLeaf <- 0.75 * strwidth("w")
      } else {
         dLeaf <- -0.75 * strwidth("w") - base_dLeaf         
      }      
      ### (if (horiz) else strheight("x"))   ## this function gives ONLY horiz= TRUE
   } else {
      if(text_pos != 2) dLeaf <- dLeaf-base_dLeaf
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
      x0 <- stats:::plotNodeLimit(x1, x2, x, center)$x
      if (horiz) 
         segments(hgt, x0, yTop, x0)
      else segments(x0, hgt, x0, yTop)
      if (!is.null(et <- attr(x, "edgetext"))) {
         my <- mean(hgt, yTop)
         if (horiz) 
            text(my, x0, et)
         else text(x0, my, et)
      }
   }   
   
#    stats:::plotNode
   # @param text_offset Numeric (NULL). This value gives the offset of the label 
   # from the specified coordinate in fractions of a character width.
   # If NULL (default) then 3/4 of a letter width is used.
   # 
   # This number overrides dLeaf.   
   
   
   plotNode_horiz(x1, x2, x, type = type, center = center, leaflab = leaflab, 
            dLeaf = dLeaf, nodePar = nodePar, edgePar = edgePar, 
            horiz = horiz, text_pos = text_pos)
   
   return(invisible(dLeaf))
}

# # stats:::plot.dendrogram
# # stats:::plotNode












#' @title Tanglegram plot
#' @export
#' @aliases 
#' tanglegram.default
#' tanglegram.dendrogram
#' tanglegram.hclust
#' tanglegram.phylo
#' dendbackback
#' @description Counts the number of leaves in a tree (dendrogram or hclust).
#' 
#' @author Tal Galili, plannapus
#' 
#' @usage
#' tanglegram(tree1, ...)
#' 
#' \method{tanglegram}{dendrogram}(tree1, tree2 ,
#'    sort = FALSE, 
#'    color_lines = "darkgrey", 
#'    lwd = 3.5,
#'    columns_width = c(5,3,5),
#'    margin_top = 3,
#'    margin_bottom = 2.5,
#'    margin_inner = 1.8,
#'    margin_outer = 0.5,
#'    left_dendo_mar = c(margin_bottom,margin_outer,margin_top,margin_inner),
#'    right_dendo_mar = c(margin_bottom,margin_inner,margin_top,margin_outer),
#'    intersecting = TRUE,
#'    dLeaf = NULL,
#'    axes = TRUE, 
#'    type = "r", # can also be "t"
#'    lab.cex = 1,
#'    remove_nodePar =F,
#'    main_left = "",
#'    main_right = "",
#'    k_labels = NULL,
#'    k_branches = NULL,
#'    ...)
#' 
#' \method{tanglegram}{hclust}(tree1, ...)
#' 
#' \method{tanglegram}{phylo}(tree1, ...)
#' 
#' @param tree1 tree object (dendrogram/hclust/phylo), plotted on the left
#' @param tree2 tree object (dendrogram/hclust/phylo), plotted on the right
#' @param sort logical (FALSE). Should the dendrogram's labels be "sorted"?
#' (might give a better tree in some cases).
#' @param color_lines a vector of colors for the lines connected the labels.
#' If the colors are shorter than the number of labels, they are recycled 
#' (and a warning is issued).
#' @param lwd width of the lines connecting the labels.
#' @param edge.lwd width of the dendrograms lines.
#' @param columns_width a vector with three elements, giving the relative
#' sizes of the the three plots (left dendrogram, connecting lines, 
#' right dendrogram). This is passed to \link{layout}.
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
#' be trimmed so that the two trees will have the same labels?
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
#' removed? (useful when the trees' leaves have too many parameters on them)
#' @param main Character. Title above the connecting lines.
#' @param main_left Character. Title of the left dendrogram.
#' @param main_right Character. Title of the right dendrogram.
#' @param k_labels integer. Number of groups by which to color the leaves.
#' @param k_branches integer. Number of groups by which to color the branches.
#' @param rank_branches logical (FALSE). Should the branches heights be adjusted?
#' (setting this to TRUE - can make it easier for 
#' comparing topological differences)
#' @param ... not used.
#' @details 
#' Notice that tanglegram does not "resize" well. In case you are resizing your
#' window you would need to re-run the function.
#' 
#' @return invisible(NULL)
#' @source
#' The function is based on code from plannapus, after major revisions. See:
#' \url{http://stackoverflow.com/questions/12456768/duelling-dendrograms-in-r-placing-dendrograms-back-to-back-in-r}
#' @seealso \link{remove_leaves_nodePar}, \link{plot_horiz.dendrogram}, \link{rank_branches}
#' @examples
#' \dontrun{
#' set.seed(23235)
#' ss <- sample(1:150, 10 )
#' hc1 <- hclust(dist(iris[ss,-5]), "com")
#' hc2 <- hclust(dist(iris[ss,-5]), "single")
#' dend1 <- as.dendrogram(hc1)
#' dend2 <- as.dendrogram(hc2)
#' tanglegram(dend1 , dend2)
#' tanglegram(dend1 , dend2, sort = TRUE)
#' tanglegram(dend1 , dend2, remove_nodePar = TRUE)
#' tanglegram(dend1 , dend2, k_labels = 6, k_branches = 4)
#' 
#' tanglegram(dend1 , dend2, lab.cex = 2, edge.lwd = 3,
#' margin_inner= 5, type = "t", center = TRUE)
#' 
#' 
#' ## works nicely:
#' tanglegram(dend1 , dend2, lab.cex = 2, edge.lwd = 3,  
#' margin_inner= 3.5, type = "t", center = TRUE,
#' dLeaf = -0.1, xlim = c(7,0),
#' k_branches=3)
#' 
#' 
#' # using rank_branches can make the comparison even easier
#' tanglegram(rank_branches(dend1) , rank_branches(dend2), lab.cex = 2, edge.lwd = 3,  
#'  margin_inner= 3.5, type = "t", center = TRUE,
#'   dLeaf = -0.1, xlim = c(5.1,0), columns_width= c(5,1,5),
#'    k_branches=3)
#' 
#' }
tanglegram <- function (tree1, ...) {UseMethod("tanglegram")}

tanglegram.default <- function (tree1, ...) {stop("No default function for tanglegram - must use a dendrogram/hclust/phylo object")}

#' @S3method tanglegram hclust
tanglegram.hclust <- function(tree1, ...) {tanglegram.dendrogram(tree1 = tree1, ...)}

#' @S3method tanglegram phylo
tanglegram.phylo <- function(tree1, ...) {tanglegram.dendrogram(tree1 = tree1, ...)}


#' @S3method tanglegram dendrogram
tanglegram.dendrogram <- function(tree1,tree2 , sort = FALSE, 
                                  color_lines = "darkgrey", 
                                  lwd = 3.5,
                                  edge.lwd = NULL,
                                  # columns_width = c(5,2,3,2,5),
                                  columns_width = c(5,3,5),
                                  margin_top = 3,
                                  margin_bottom = 2.5,
                                  margin_inner = 3,
                                  margin_outer = 0.5,
                                  left_dendo_mar = c(margin_bottom,margin_outer,margin_top,margin_inner),
                                  right_dendo_mar=c(margin_bottom,margin_inner,margin_top,margin_outer),
                                  intersecting = TRUE,
                                  dLeaf = NULL, # -.3,
                                   dLeaf_left = dLeaf,
                                   dLeaf_right = dLeaf,
                                  axes = TRUE, 
                                  type = "r", # can also be "t"
                                  lab.cex = NULL,
                                  remove_nodePar =FALSE,
                                  main = "",
                                  main_left = "",
                                  main_right = "",
                                  k_labels = NULL,
                                  k_branches = NULL,
                                  rank_branches = FALSE, 
                                  ... )
{
   
   
   # characters_to_trim = the number of characters to leave after trimming the labels.		
   # remove_nodePar = makes sure that we won't have any dots at the end of leaves
   
   if(!is.dendrogram(tree1)) tree1 <- as.dendrogram(tree1)
   if(!is.dendrogram(tree2)) tree2 <- as.dendrogram(tree2)
   
   # remove colors from the tips of leaves
   if(remove_nodePar) {
      tree1 <- remove_leaves_nodePar(tree1)
      tree2 <- remove_leaves_nodePar(tree2)
   }
   # sort them for better graph
   if(sort == TRUE) {	# based on the "rotate.dendrogram" function
      tree1 <- sort(tree1)
      tree2 <- sort(tree2)
   }   
   if(intersecting) {
      tree12 <- intersect_trees(tree1, tree2, warn = TRUE)
      tree1 <- tree12[[1]]
      tree2 <- tree12[[2]]
   }
   # adjust labels cex:
   if(!is.null(lab.cex)) {
      tree1 <- assign_values_to_leaves_nodePar(tree1, lab.cex, "lab.cex")
      tree2 <- assign_values_to_leaves_nodePar(tree2, lab.cex, "lab.cex")
   }
   if(!is.null(edge.lwd)) {
      tree1 <- assign_values_to_branches_edgePar(tree1, edge.lwd, "lwd")
      tree2 <- assign_values_to_branches_edgePar(tree2, edge.lwd, "lwd")
   }
   if(!is.null(k_labels)) {
      tree1 <- color_labels(tree1, k_labels)
      tree2 <- color_labels(tree2, k_labels)
   }
   if(!is.null(k_branches)) {
      tree1 <- color_branches(tree1, k_branches)
      tree2 <- color_branches(tree2, k_branches)
   }
   if(rank_branches) {
      tree1 <- rank_branches(tree1)
      tree2 <- rank_branches(tree2)      
   }
   
   
   
   
   l <- nleaves(tree1)
   
   # makes sure that dLeaf gives a symmetric result.
   if(!is.null(dLeaf) && dLeaf_right==dLeaf_left) dLeaf_right <- -dLeaf_left 
   
   ##########################################
   #####  Plotting.
   ##########################################
   
   labels_tree1 <- labels(tree1)
   max_labels_tree1 <- labels_tree1[which.max(nchar(labels_tree1))]
   
   
   # The matrix to draw the arrows:
   ord_arrow <- cbind((1:l)[order(order.dendrogram(tree1))],(1:l)[order(order.dendrogram(tree2))]) 
   
   # Set the layout of the plot elements
   layout(matrix(1:3,nrow=1),width=columns_width)
      
   #################
   # The first dendrogram:	
   #################
   par(mar=left_dendo_mar)
   plot(tree1,horiz=TRUE, ylim=c(0,l),
        dLeaf = dLeaf_left, 
        type = type, axes = axes,
        main = main_left,
        # ...)
#         leaflab="none",
       yaxs = "r", xaxs = "i" ,...) # this might be causing bugs when refreshing a resized window.

   # now that I have a plot to use, I can calculate strwidth
   # this ASSUMES that both tree plots are of the same size/shape...
   
   #################
   # The arrows:
   #################
   # arros colors:   
   if(length(color_lines) < l) color_lines <- rep.int(color_lines, l)
   color_lines <- color_lines[ord_arrow[,1]]	
   
   par(mar=c(margin_bottom,0,margin_top,0))
   plot(NA, bty="n",axes=FALSE,xlim=c(0,1), ylim=c(0,l),ylab="",xlab="", # )#,
        yaxs = "r", xaxs = "i")
   col_indx <- 0
   apply(ord_arrow,1,
         function(x){
            col_indx <<- col_indx + 1
            arrows(0,x[1],1,x[2],code=0, length=0.05, col= color_lines[col_indx], lwd = lwd)
         }
   )
   mtext(main,side=3)
   
   #################
   # And the second dendrogram (to reverse it I reversed the xlim vector:
   #################
   par(mar=right_dendo_mar)
   plot_horiz.dendrogram(tree2, side=TRUE, dLeaf=dLeaf_right,
                         type = type, axes = axes,
                         ylim=c(0,l),
                         main = main_right,
                         # ...)
                         #         leaflab="none",        
                         yaxs = "r", xaxs = "i",...)

   
   
   
   return(invisible(NULL))
}



# add an "abbreviation" parameter - to trim the labels of two trees.



#' @export
dendbackback <- tanglegram.dendrogram # another name for the same function.
# hclustbackback <- tanglegram.hclust # another name for the same function.




