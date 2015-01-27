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
# source("R\\ggdendro.R")






#'
#' 
#' This is an extension of the function dendrogram_data, from Andrie de Vries's ggdendro package (which is a modified \link{plot.dendrogram}).
#' It was created basically to include segments and labels data so that they could be represented in the plot.


## Add this to the vignette!
# 
# library(dendextend)
# library(ggdendro)
# data(iris) # load data
# dend <- iris[1:30,-5] %>% dist %>% hclust %>% as.dendrogram %>%
#          set("branches_k_color",k=3)
# plot(dend)
# gg_dend <- ggdendro:::dendrogram_data(dend)
# 
# # Getting the colors ready
# cols <- unname(unlist(get_nodes_attr(dend, "edgePar")))
# cols <- tail(cols, -1) # Remove the invisible top branch
# cols <- rep(cols, each = 2) # duplicate it for each segment
# 
# p <- ggplot(gg_dend$segments) + 
#    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, colour = cols), size = 1.2) + 
#    coord_flip() + 
#    scale_y_reverse(expand = c(0.2, 0)) +
#    geom_text(data = gg_dend$labels, 
#              aes(x = x, y = y, label = label)) +
#    theme_dendro() + coord_polar(theta="x")
# p
# 
# as.ggdend.dendrogram(dend)
#    




# I will need this...
allNA <- function(x) all(is.na(x))




as.ggdend <- function(dend, ...) {
   UseMethod("as.ggdend")   
}

as.ggdend.dendrogram <- function (dend, type = c("rectangle", "triangle"), edge.root = FALSE, ...)  {
   if(!is.dendrogram(dend)) stop("dend is not a dendrogram (and it needs to be...)")
   if(nleaves(dend) == 0) stop("dend must have at least one node")
   if(edge.root) stop("edge.root is not supported at this point (this parameter is a place-holder for when it will)")
   
   ggdata <- dendrogram_data(dend, type = type) # ggdendro:::dendrogram_data(dend)
   
   
   # create nodes_xy 
   nodes_xy <- with(ggdata$segments, {
#       > ggdata$segments
#                   x         y     xend      yend
#       1    8.880859 2.0420578  3.25000 2.0420578
#       2    3.250000 2.0420578  3.25000 0.9219544
#       3    3.250000 0.9219544  1.50000 0.9219544
      # x=ggdata$segments$x
      id <- seq_along(x)
      id_even <- (id %% 2) == 0
      data.frame(x=c(x[1], xend[id_even]), y=c(y[1], yend[id_even]), 
                 pch = NA, cex = NA, col = NA,
                 members = NA, midpoint = NA, height = NA, leaf = NA)     
   })
   # nnodes(dend) == nrow(nodes_xy) # sanity check
   
   # add parameters to nodes_xy
   # graphical parameters
   nodes_attr <- get_nodes_attr(dend, "nodePar", simplify = TRUE)
   if(!allNA(nodes_attr)) {
      nodes_attr_names <- rownames(nodes_attr)
      if("pch" %in% nodes_attr_names) nodes_xy$pch <- nodes_attr["pch",] # should actually ALWAYS be here...
      if("cex" %in% nodes_attr_names) nodes_xy$cex <- nodes_attr["cex",]
      if("col" %in% nodes_attr_names) nodes_xy$col <- nodes_attr["col",]
   }
   # others: (won't be used for plotts, but in the future someone might want to use them in some graphical way...)
   nodes_xy$members <- get_nodes_attr(dend, "members")
   nodes_xy$midpoint <- get_nodes_attr(dend, "midpoint")
   nodes_xy$height <- get_nodes_attr(dend, "height")
   nodes_xy$leaf <- get_nodes_attr(dend, "leaf")

   # add nodes_xy
   ggdata$nodes <- nodes_xy

   # segments (edges) - add graphical parameters   
   edgePar_attr <- get_nodes_attr(dend, "edgePar", simplify = FALSE)
            # [[1]]
            # [1] NA
            # 
            # [[2]]
            # [[2]]$col
            # [1] "#CC476B"
            # 
            # 
            # [[3]]
            # [[3]]$col
            # [1] "#CC476B"
   if(!edge.root) edgePar_attr <- edgePar_attr[-1] # remove the first edgePar since we don't support root edge at this point...
#    default_par <-  rep(NA, nrow(ggdata$segments))
#    segments_par <- data.frame(col = default_par, lwd = default_par, lty = default_par)

   if(!allNA(edgePar_attr)) {
      #        edgePar_attr_names <- sapply(edgePar_attr, names)
      #        ss_col <- sapply(edgePar_attr_names, function(x) {"col" %in% x}) # no longer needed
      
      # par is a character of the par to get from edgePar_attr
      get_edgePar_attr_par <- function(par) rep(unlist(sapply(edgePar_attr,  `[`, name = par)), each = 2) # like doing edgePar_attr[[1]] ["col"]
      # The rep is because a segment has two lines. So we use each: rep(1:4, each = 2)
      
      ggdata$segments$col <- get_edgePar_attr_par("col") # like doing edgePar_attr[[1]] ["col"]
      ggdata$segments$lwd <- get_edgePar_attr_par("lwd")
      ggdata$segments$lty <- get_edgePar_attr_par("lty")
   }


   # labels - add graphical
   
   
   leaves_edgePar_attr <- get_leaves_nodePar(dend)
      # [[1]]
      # [[1]]$lab.col
      # [1] 2
      # 
      # [[1]]$pch
      # [1] NA   

   get_leaves_edgePar_attr_par <- function(par) unlist(sapply(leaves_edgePar_attr,  `[`, name = par)) # like doing edgePar_attr[[1]] ["col"]
   # The rep is because a segment has two lines. So we use each: rep(1:4, each = 2)
   
   ggdata$labels$col <- get_leaves_edgePar_attr_par("lab.col") # like doing edgePar_attr[[1]] ["col"]
   ggdata$labels$cex <- get_leaves_edgePar_attr_par("lab.cex") # like doing edgePar_attr[[1]] ["col"]

   class(ggdata) <- "ggdend"
   ggdata
}


# as.ggdend(dend)

# print.ggdend  # maybe say how many labels, nodes, splits, and graphical parameters it has








ggplot <- function (data = NULL, ...) {
   UseMethod("ggplot")
}




# ggplot.ggdend
ggplot.dendrogram <- function(data, ...) {
   ggplot(as.ggdend(data), ...)
}



























###############
## JUNK

# 
# labels_colors(dend2)
# plot(dend2)
# unclass(dend2)
# labels_colors(dend2) <- 2
# get_leaves_attr(dend2, "nodePar")
# get_leaves_attr(dend, "nodePar")
# 
# 
# x=get_nodes_attr(dend, "edgePar", simplify = FALSE)
# x=get_nodes_attr(set(dend, "clear_branches"), "edgePar", simplify = FALSE)
# "col" %in% sapply(x, names)
# 
# nodes_xy$fsaaf
# x=get_nodes_attr(dend, "edgePar", simplify = FALSE)
# unlist(x)
# head(x,3)
# sapply(x, names)
# sapply(get_nodes_attr(dend2, "edgePar", simplify = FALSE), names)
# do.call(rbind, )

