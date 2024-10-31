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




#' @title Creates dendrogram plot using ggplot.
#' @rdname ggdend
#' @export
#'
#' @description
#' Several functions for creating a dendrogram plot using ggplot2.
#' The core process is to transform a dendrogram into a ggdend object using as.ggdend,
#' and then plot it using ggplot. These two steps can be done in one command with either the function
#' ggplot or ggdend.
#'
#' The reason we want to have as.ggdend (and not only ggplot.dendrogram), is (1) so that you could
#' create your own mapping of ggdend and, (2) since as.ggdend might be slow for large trees,
#' it is probably better to be able to run it only once for such cases.
#'
#' A ggdend class object is a list with 3 componants: segments, labels, nodes.
#' Each one contains the graphical parameters from the original dendrogram, but in a tabular form that
#' can be used by ggplot2+geom_segment+geom_text to create a dendrogram plot.
#'
#'
#'
#' @param dend a \link{dendrogram} tree (to be turned into a ggdend object)
#' @param type The type of plot, indicating the shape of the dendrogram.  "rectangle" will draw
#' rectangular lines, while "triangle" will draw triangular lines.
#' @param edge.root currently ignored. One day it might do the following: logical; if true, draw an edge to the root node.
#' @param ... mostly ignored.
#' @param data,x a ggdend class object (passed to ggplot.dendrogram or print.ggdend).
#' @param segments a logical (TRUE) if to plot the segments (branches).
#' @param labels a logical (TRUE) if to plot the labels.
#' @param offset_labels a numeric value to offset the labels from the leaves
#' @param nodes a logical (TRUE) if to plot the nodes (points).
#' @param horiz a logical (TRUE) indicating if the dendrogram should be drawn horizontally or not.
#' @param theme the ggplot2 theme to use (default is \link{theme_dendro}, can also be NULL
#' for the default ggplot2 theme)
#' @param na.rm A logical (TRUE) to control removal of missing values. Passed to
#' \link[ggplot2]{geom_line} and \link[ggplot2]{geom_point}
#' 
#' @param mapping (passed in ggplot.ggdend) Default list of aesthetic mappings to use for plot. If not specified, must be supplied in each layer added to the plot.
#' @param environment (passed in ggplot.ggdend) deprecated / ignored.
#'
#' @details
#'
#' \code{prepare.ggdend} is used by \code{plot.ggdend} to take the \code{ggdend} object
#' and prepare it for plotting. This is because the defaults of various parameters in \link{dendrogram}'s
#' are not always stored in the object itself, but are built-in into the \link{plot.dendrogram} function.
#' For example, the color of the labels is not (by default) specified in the dendrogram (only if we change it
#' from black to something else). Hence, when taking the object into a different plotting engine (say ggplot2), we
#' want to prepare the object by filling-in various defaults.
#' This function is autmatically invoked within the \code{plot.ggdend} function. You would probably use
#' it only if you'd wish to build your own ggplot2 mapping.
#'
#' @author Tal Galili, using code modified from Andrie de Vries
#'
#' @seealso
#'
#' \link{dendrogram}, \link{get_nodes_attr}, \link{get_leaves_nodePar},
#' \link[ggplot2]{ggplot},
#' \link[ggdendro]{ggdendrogram}, \link[ggdendro]{dendro_data},
#'
#' @return
#'
#' \itemize{
#' \item{\code{as.ggdend} - returns an object of class ggdend which is a list with 3 componants: segments, labels, nodes.
#' Each one contains the graphical parameters from the original dendrogram, but in a tabular form that
#' can be used by ggplot2+geom_segment+geom_text to create a dendrogram plot.}
#' \item{\code{prepare.ggdend} - a \code{ggdend} object (after filling it with various default values)}
#' \item{\code{ggplot.ggdend} - a \link[ggplot2]{ggplot} object}
#' }
#'
#' @source
#'
#' These are extended versions of the functions \link[ggdendro]{ggdendrogram}, \link[ggdendro]{dendro_data} (and the hidden dendrogram_data)
#' from Andrie de Vries's ggdendro package. The motivation for this fork is the need to add more graphical parameters
#' to the plotted tree. This required a strong mixter of functions from ggdendro and dendextend (to the point that
#' it seemed better to just fork the code into its current form)
#'
#' @examples
#'
#' \dontrun{
#'
#' library(dendextend)
#' # library(ggdendro)
#' # Create a complex dend:
#' dend <- iris[1:30, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("branches_k_color", k = 3) %>%
#'   set("branches_lwd", c(1.5, 1, 1.5)) %>%
#'   set("branches_lty", c(1, 1, 3, 1, 1, 2)) %>%
#'   set("labels_colors") %>%
#'   set("labels_cex", c(.9, 1.2))
#' # plot the dend in usual "base" plotting engine:
#' plot(dend)
#' # Now let's do it in ggplot2 :)
#' ggd1 <- as.ggdend(dend)
#' library(ggplot2)
#' ggplot(ggd1) # reproducing the above plot in ggplot2 :)
#'
#' # Triangle version:
#' plot(dend, type = "triangle")
#' ggd2 <- as.ggdend(dend, type = "triangle")
#' ggplot(ggd2) 
#'
#'
#' # More modifications:
#' labels(dend) <- paste0(labels(dend), "00000")
#' ggd1 <- as.ggdend(dend)
#' # Use ylim to deal with long labels in ggplot2
#' ggplot(ggd1) + ylim(-.4, max(get_branches_heights(dend)))
#'
#'
#' ggplot(ggd1, horiz = TRUE) # horiz plot in ggplot2
#' # Adding some extra spice to it...
#' # creating a radial plot:
#' ggplot(ggd1) + scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta = "x")
#' # The text doesn't look so great, so let's remove it:
#' ggplot(ggd1, labels = FALSE) + scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta = "x")
#'
#' # This can now be sent to plot.ly - which adds zoom-in abilities, and more.
#' # Here is how it might look like: https://plot.ly/~talgalili/6/y-vs-x/
#'
#' ## Quick guide:
#' # install.packages("devtools")
#' # library("devtools")
#' # devtools::install_github("ropensci/plotly")
#' # library(plotly)
#' # set_credentials_file(...)
#' # you'll need to get it from here: https://plot.ly/ggplot2/getting-started/
#'
#' # ggplot(ggd1)
#' # py <- plotly()
#' # py$ggplotly()
#'
#' # And you'll get something like this: https://plot.ly/~talgalili/6/y-vs-x/
#'
#' # Another example: https://plot.ly/ggplot2/
#' }
ggdend <- function(...) {
  cat("Use either as.ggdend or ggplot (or both). \n")
}




# I will need this...
allNA <- function(x) all(is.na(x))

#' @export
#' @rdname ggdend
as.ggdend <- function(dend, ...) {
  UseMethod("as.ggdend")
}


#' @export
#' @rdname ggdend
as.ggdend.dendrogram <- function(dend, type = c("rectangle", "triangle"), edge.root = FALSE, ...) {


  # Warning: This function is NOT the most optimized function. We would rather use it once and then work with the output (rather than calling it over and over...)

  if (!is.dendrogram(dend)) stop("dend is not a dendrogram (and it needs to be...)")
  if (nleaves(dend) == 0) stop("dend must have at least one node")
  if (edge.root) stop("edge.root is not supported at this point (this parameter is a place-holder for when it will)")
  type <- match.arg(type, c("rectangle", "triangle"))
  # source('R/stats_imports.R', echo=FALSE)
  # ggdata <- dendextend:::dendrogram_data(dend, type = "rectangle")
  ggdata <- dendrogram_data(dend, type = type) # ggdendro:::dendrogram_data(dend)


  # create nodes_xy
  # ==========================
  nodes_xy <- with(ggdata$segments, {
    #       > ggdata$segments
    #                   x         y     xend      yend
    #       1    8.880859 2.0420578  3.25000 2.0420578
    #       2    3.250000 2.0420578  3.25000 0.9219544
    #       3    3.250000 0.9219544  1.50000 0.9219544
    # x=ggdata$segments$x
    id <- if(type == "rectangle") {
      (seq_along(x) %% 2) == 0
    } else {
      TRUE
    }
    data.frame(
      x = c(x[1], xend[id]), y = c(y[1], yend[id]),
      pch = NA, cex = NA, col = NA,
      members = NA, midpoint = NA, height = NA, leaf = NA,
      stringsAsFactors = TRUE
    )
  })
  # nnodes(dend) == nrow(nodes_xy) # sanity check




  # dend %>% unclass %>% str

  # add parameters to nodes_xy
  # graphical parameters
  nodes_attr <- get_nodes_attr(dend, "nodePar", simplify = FALSE)

  # nodes_attr <- get_nodes_attr(d1, "nodePar", simplify = FALSE)
  get_nodePar_attr_par <- function(par) {
    # The rep is because a segment has two lines. So we use each: rep(1:4, each = 2)
    # tmp <- rep(unlist(sapply(nodes_attr,  `[`, name = par)), each = 1) # like doing edgePar_attr[[1]] ["col"]
    values <- sapply(nodes_attr, `[`, name = par) # like doing edgePar_attr[[1]] ["col"]
    null2NA <- function(x) ifelse(is.null(x), NA, x)
    values <- sapply(values, null2NA) # in case the attr is missing, it fills the NULL with NA
    # if(is.null(tmp)) tmp <- rep(NA, length(nodes_attr))
    unlist(values)
  }



  nodes_xy$pch <- get_nodePar_attr_par("pch")
  nodes_xy$cex <- get_nodePar_attr_par("cex")
  nodes_xy$col <- get_nodePar_attr_par("col")

  ##########################################




  # others: (won't be used for plotts, but in the future someone might want to use them in some graphical way...)
  nodes_xy$members <- get_nodes_attr(dend, "members")
  nodes_xy$midpoint <- get_nodes_attr(dend, "midpoint")
  nodes_xy$height <- get_nodes_attr(dend, "height")
  nodes_xy$leaf <- get_nodes_attr(dend, "leaf")

  # add nodes_xy
  ggdata$nodes <- nodes_xy

  # segments (edges) - add graphical parameters
  # ==========================
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
  if (!edge.root) edgePar_attr <- edgePar_attr[-1] # remove the first edgePar since we don't support root edge at this point...
  #    default_par <-  rep(NA, nrow(ggdata$segments))
  #    segments_par <- data.frame(col = default_par, lwd = default_par, lty = default_par)

  #    if(!allNA(edgePar_attr)) {
  #        edgePar_attr_names <- sapply(edgePar_attr, names)
  #        ss_col <- sapply(edgePar_attr_names, function(x) {"col" %in% x}) # no longer needed

  # par is a character of the par to get from edgePar_attr
  get_edgePar_attr_par <- function(par) {
    values <- sapply(edgePar_attr, `[`, name = par)
    #       null2NA <- function(x) ifelse(is.na(x) || is.null(x), NA, x)
    null2NA <- function(x) ifelse(is.null(x), NA, x)
    values <- sapply(values, null2NA) # in case the attr is missing, it fills the NULL with NA
    rep(unlist(values), each = if(type == "rectangle") 2 else 1) # like doing edgePar_attr[[1]] ["col"]
  }

  # The rep is because a segment has two lines. So we use each: rep(1:4, each = 2)

  #    length(get_edgePar_attr_par("col"))
  # length(get_edgePar_attr_par("lwd"))
  #    dim(ggdata$segments)

  ggdata$segments$col <- get_edgePar_attr_par("col") # like doing edgePar_attr[[1]] ["col"]
  ggdata$segments$lwd <- get_edgePar_attr_par("lwd")
  ggdata$segments$lty <- get_edgePar_attr_par("lty")
  #    }


  # labels - add graphical
  # ==========================



  leaves_edgePar_attr <- get_leaves_nodePar(dend)
  # [[1]]
  # [[1]]$lab.col
  # [1] 2
  #
  # [[1]]$pch
  # [1] NA


  get_leaves_edgePar_attr_par <- function(par) {
    values <- sapply(leaves_edgePar_attr, `[`, name = par)
    null2NA <- function(x) ifelse(is.null(x), NA, x)
    values <- sapply(values, null2NA) # in case the attr is missing, it fills the NULL with NA
    unlist(values) # like doing edgePar_attr[[1]] ["col"]
  }



  the_lab.col <- get_leaves_edgePar_attr_par("lab.col") # like doing edgePar_attr[[1]] ["col"]
  the_lab.cex <- get_leaves_edgePar_attr_par("lab.cex") # like doing edgePar_attr[[1]] ["col"]
  the_lab.height <- get_leaves_attr(dend, "height")
  if (is.null(the_lab.col)) the_lab.col <- NA
  if (is.null(the_lab.cex)) the_lab.cex <- NA

  ggdata$labels$col <- the_lab.col
  ggdata$labels$cex <- the_lab.cex
  ggdata$labels$y <- the_lab.height
  # The above saves us from errors such as:
  # Error in eval(expr, envir, enclos) : object 'cex' not found
  # In addition: Warning message:
  #    In is.na(data$labels$cex) :
  #    is.na() applied to non-(list or vector) of type 'NULL'


  class(ggdata) <- "ggdend"
  ggdata
}


# as.ggdend(dend)

# print.ggdend  # maybe say how many labels, nodes, splits, and graphical parameters it has







# allows for the S3 to kick in, without calling ggplot2
# don't run this when testing! (it doesn't have the namespace of ggplot2 for finding ggplot.data.frame etc.)
# ggplot <- function (data = NULL, ...) {
#    UseMethod("ggplot")
# }


#
#
# data(iris) # load data
# dend <- iris[1:30,-5] %>% dist %>% hclust %>% as.dendrogram %>%
#    set("branches_k_color",k=3) %>% set("branches_lwd", c(1,2)) %>% set("branches_lty", c(1,2,1,3))
# plot(dend)
# data <- as.ggdend(dend)
#  rm(ggplot)
# ggplot.ggdend(data)


# ggplot(data)
# ggplot.ggdend(data, horiz = T) +
# coord_flip() +
#    scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta="x")






#' @export
#' @rdname ggdend
prepare.ggdend <- function(data, ...) {
  # Fix segements
  # ===============

  # Fix lty
  #-------------
  # Using scale_linetype_identity() fixed all of this!
  # Dealing with the linetypes:
  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
  # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
  #   linetypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash") # , "1F", "F1", "4C88C488", "12345678")
  #    # lty must be either numeric or character
  #    if(is.numeric(data$segments$lty)) {
  #       data$segments$lty[is.na(data$segments$lty)] <- 1
  #       data$segments$lty <- factor(data$segments$lty, levels = 0:6)
  #       levels(data$segments$lty) <- linetypes
  #       data$segments$lty <- as.character(data$segments$lty)
  #
  #    } else { # character
  #       # filling missing values (what ever is missing is solid)
  #       data$segments$lty[is.na(data$segments$lty)] <- "solid"
  #    }
  #    # if lty is NOT numeric or character == a problem...
  #    if(!is.numeric(data$segments$lty) & !is.character(data$segments$lty)) {
  #       warning("lty in segments should be either a number or a character. falls back to solid.")
  #       data$segments$lty <- "solid"
  #    }

  if (is.numeric(data$segments$lty)) {
    data$segments$lty[is.na(data$segments$lty)] <- 1
  } else { # character
    # filling missing values (what ever is missing is solid)
    data$segments$lty[is.na(data$segments$lty)] <- "solid"
  }



  # Fix lwd
  #-------------
  # filling missing values
  data$segments$lwd[is.na(data$segments$lwd)] <- 1

  #    Fix col
  #    -------------
  #    filling missing values
  if (is.numeric(data$segments$col)) {
    data$segments$col[is.na(data$segments$col)] <- 1
  } else { # character
    # filling missing values (what ever is missing is solid)
    data$segments$col[is.na(data$segments$col)] <- "black" # "#000000"
    #       data$segments$col <- "#000000"
  }


  # Fix labels
  # ===============

  #    Fix col
  #    -------------
  #    filling missing values
  if (is.numeric(data$labels$col)) {
    data$labels$col[is.na(data$labels$col)] <- 1
  } else { # character
    data$labels$col[is.na(data$labels$col)] <- "black" # "#000000"
  }

  #    Fix cex
  #    -------------
  #    filling missing values
  if (any(is.na(data$labels$cex))) data$labels$cex[is.na(data$labels$cex)] <- 1


  #    Fix the "nodes" table
  #    -------------
  # We fix a row only if we have some value for pch/cex/col in that row.
  ss_has_value <- !apply(data$nodes[, c("pch", "cex", "col")], 1, allNA)
  #    filling missing values
  data$nodes$pch <- ifelse(ss_has_value & is.na(data$nodes$pch), 1, data$nodes$pch)
  data$nodes$cex <- ifelse(ss_has_value & is.na(data$nodes$cex), 3.5, data$nodes$cex)
  data$nodes$col <- ifelse(ss_has_value & is.na(data$nodes$col), 1, data$nodes$col)


  data
}



# ' @ importFrom ggplot2 ggplot geom_segment geom_point aes guides scale_colour_identity scale_size_identity scale_linetype_identity scale_shape_identity geom_text coord_flip scale_y_reverse aes_string
# NULL

#' @import ggplot2


# ggplot2:::ggplot.data.frame
# based on ggdendrogram! from the ggdendro package
# polar cor is a problem with text: https://stackoverflow.com/questions/8468472/adjusting-position-of-text-labels-in-coord-polar-histogram-in-ggplot2

#' @export
#' @rdname ggdend
ggplot.ggdend <- function(data = NULL, mapping = aes(), ..., segments = TRUE, labels = TRUE, nodes = TRUE,
                          horiz = FALSE, theme = theme_dendro(),
                          offset_labels = 0, na.rm = TRUE,
                          environment = parent.frame())  {
  #    library(dendextend)
  #    library(ggdendro)
  # Get all the ggplot2 functions ready: (this could have been evoided if ggplot2 was imported...)
  # This saves us from the error: "ggplot.ggdend: no visible global function definition for ..."
  # library(ggplot2)
  # ggplot <- ggplot2::ggplot
  # geom_segment <- ggplot2::geom_segment
  # geom_point <- ggplot2::geom_point
  # aes <- ggplot2::aes
  # guides <- ggplot2::guides
  # scale_colour_identity <- ggplot2::scale_colour_identity
  # scale_size_identity <- ggplot2::scale_size_identity
  # scale_linetype_identity <- ggplot2::scale_linetype_identity
  # scale_shape_identity <- ggplot2::scale_shape_identity
  # geom_text <- ggplot2::geom_text
  # coord_flip <- ggplot2::coord_flip
  # scale_y_reverse <- ggplot2::scale_y_reverse
  # aes_string <- ggplot2::aes_string

  # By using "x" instead of x - we avoid the error:
  # ggplot.ggdend: no visible binding for global variable 'x'



  data <- prepare.ggdend(data)


  # turning off legends.
  # https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot

  angle <- ifelse(horiz, 0, 90)
  hjust <- ifelse(horiz, 0, 1)



  p <- ggplot(mapping = mapping, ...)


  # ggplot() +
  #    geom_segment(data = data$segments,
  #                 aes(x = x, y = y, xend = xend, yend = yend, colour = col, linetype = lty, size = lwd)) +
  #    guides(linetype = "none", col = "none") +
  #    scale_colour_identity() + scale_size_identity()  + scale_linetype_identity() +
  #    geom_text(data = data$labels,
  #              aes(x = x, y = y, label = label), angle = angle, hjust = 1) #  ,angle = 90


  if (segments) {
     p <- p + geom_segment(
        data = data$segments, na.rm = na.rm,
        aes(
           x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
           colour = .data$col, linetype = .data$lty, linewidth = .data$lwd
        ),
        lineend = "square"
     ) +
        guides(linetype = "none", col = "none") +
        scale_colour_identity() + scale_size_identity() + scale_linetype_identity()
  }
  
  if (nodes) {
     p <- p + geom_point(
        data = data$nodes, na.rm = na.rm,
        aes(x = .data$x, y = .data$y, colour = .data$col, shape = .data$pch, size = .data$cex)
     ) +
        guides(shape = "none", col = "none", size = "none") +
        # scale_colour_identity() + scale_size_identity()  +
        scale_shape_identity()
  }
  # http://docs.ggplot2.org/0.9.3.1/geom_point.html

  #    p +  geom_point(data = data$nodes,
  #                    aes_string(x = "x", y = "y", colour = "col", shape = "pch", size = 3.5)) +
  #       guides(shape = "none", col = "none", size = "none") +
  #       scale_shape_identity()


  if (labels) {
     # default size is 5!  http://sape.inf.usi.ch/quick-reference/ggplot2/geom_text
     data$labels$cex <- 5 * data$labels$cex
     data$labels$y <- data$labels$y + offset_labels
     p <- p + geom_text(
        data = data$labels, 
        aes(
           x = .data$x,
           y = .data$y, 
           label = .data$label, 
           colour = .data$col, 
           size = .data$cex
        ),
        hjust = hjust, 
        angle = angle
     )
  }
  # p <- p + scale_x_discrete(labels = data$labels$label)
  # if (horiz) {
  #    p <- p + scale_x_discrete(labels = data$labels$label)
  # }
  # else {
  #    p <- p + scale_x_discrete(labels = data$labels$label)
  # }
  if (horiz) {
    p <- p + coord_flip() + scale_y_reverse(expand = c(0.2, 0))
  }
  # p <- p + scale_y_reverse(expand = c(0.2, 0)) # scale_y_continuous() +

  # if (theme_dendro)
  #    p <- p + theme_dendro()
  # p <- p + theme(axis.text.x = element_text(angle = angle,
  #                                           hjust = 1))
  # p <- p + theme(axis.text.y = element_text(angle = angle,
  #                                          hjust = 1))

  if (!is.null(theme)) {
    p <- p + theme
  }

  p


  # + scale_colour_brewer()
  #       + scale_color_manual(values = unique(data$segments$col))


  #          scale_linetype_manual(guide=FALSE) # values=linetypes) #,
  #
  # + # Change linetypes
  #    scale_color_manual(, guide=FALSE)
  #
  #
  #    p <- ggplot() +
  #       geom_segment(data = data$segments,
  #                    aes(x = x, y = y, xend = xend, yend = yend, colour = col, linetype = "2", size =2)) +
  #       coord_flip() +
  #       scale_y_reverse(expand = c(0.2, 0)) +
  #       geom_text(data = data$labels,
  #                 aes(x = x, y = y, label = label))
  #    p
  #    + theme_dendro() + coord_polar(theta="x")
}


#' @export
#' @rdname ggdend
ggplot.dendrogram <- function(data, ...) {
  # library(ggplot2) # enough to use ggplot2::
  ggplot2::ggplot(as.ggdend(data), ...)
}


#' @export
#' @rdname ggdend
print.ggdend <- function(x, ...) {
  # library(ggplot2) # enough to use ggplot2::
  print(ggplot2::ggplot(x, ...))
}

























# ===
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

# ggplot.ggdend(data)
# ggplot.ggdend(data, horiz = T)
