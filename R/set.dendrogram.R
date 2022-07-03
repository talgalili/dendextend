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



#' @title Set (/update) features to a dendrogram
#' @export
#' @rdname set
#'
#' @description
#' a master function for updating various attributes and
#' features of dendrogram objects.
#'
#' @param dend a tree (\link{dendrogram}, or \link{dendlist})
#' @param what a character indicating what is the property of
#' the tree that should be set/updated. (see the usage and the example section
#' for the different options)
#' @param value an object with the value to set in the dendrogram tree.
#' (the type of the value depends on the "what")
#' @param order_value logical. Default is FALSE. If TRUE, it means the order of
#' the value is in the order of the data which produced the \link{hclust}
#' or \link{dendrogram} - and will reorder the value to conform with the order
#' of the labels in the dendrogram.
#' @param ... passed to the specific function for more options.
#' @param which an integer vector indicating, in the case "dend" is
#' a dendlist, on which of the trees should the modification be performed.
#' If missing - the change will be performed on all of dends in the dendlist.
#'
#' @details
#' This is a wrapper function for many of the main tasks we
#' might wish to perform on a dendrogram before plotting.
#'
#' The options of by_labels_branches_col, by_labels_branches_lwd, by_labels_branches_lty
#' have extra parameters: type, attr, TF_value, and by_lists_branches_col, by_lists_branches_lwd,
#' by_lists_branches_lty have extra parameters: attr, TF_value. You can read more about them here:
#' \link{branches_attr_by_labels} and \link{branches_attr_by_lists}
#'
#' The "what" parameter" can accept the following options:
#'
#' \itemize{
#' \item{labels - set the labels (\link{labels<-.dendrogram})}
#' \item{labels_colors - set the labels' colors (\link{color_labels})}
#' \item{labels_cex - set the labels' size (\link{assign_values_to_leaves_nodePar})}
#' \item{labels_to_character - set the labels' to be characters}
#' \item{leaves_pch - set the leaves' point type (\link{assign_values_to_leaves_nodePar}). A leave is the terminal node of the tree.}
#' \item{leaves_cex - set the leaves' point size (\link{assign_values_to_leaves_nodePar}). For using this you MUST also set leaves_pch, a good value to use is 19.}
#' \item{leaves_col - set the leaves' point color (\link{assign_values_to_leaves_nodePar}). For using this you MUST also set leaves_pch, a good value to use is 19.}
#' \item{leaves_bg - set the leaves' point fill color (\link{assign_values_to_leaves_nodePar}). For using this you MUST also set leaves_pch with values from 21-25.}
#' \item{nodes_pch - set the nodes' point type (\link{assign_values_to_nodes_nodePar})}
#' \item{nodes_cex - set the nodes' point size (\link{assign_values_to_nodes_nodePar})}
#' \item{nodes_col - set the nodes' point color (\link{assign_values_to_nodes_nodePar})}
#' \item{nodes_bg - set the nodes' point fill color (\link{assign_values_to_nodes_nodePar}). For using this you MUST also set leaves_pch with values from 21-25.}
#' \item{hang_leaves - hang the leaves (\link{hang.dendrogram})}
#' \item{branches_k_color - color the branches (\link{color_branches}), a \code{k} parameter needs to be supplied.}
#' \item{branches_k_lty - updates the lwd of the branches (similar to branches_k_color), a \code{k} parameter needs to be supplied.}
#' \item{branches_col - set the color of branches (\link{assign_values_to_branches_edgePar}) }
#' \item{branches_lwd - set the line width of branches (\link{assign_values_to_branches_edgePar}) }
#' \item{branches_lty - set the line type of branches (\link{assign_values_to_branches_edgePar}) }
#' \item{by_labels_branches_col - set the color of branches with specific labels (\link{branches_attr_by_labels}) }
#' \item{by_labels_branches_lwd - set the line width of branches with specific labels (\link{branches_attr_by_labels}) }
#' \item{by_labels_branches_lty - set the line type of branches with specific labels (\link{branches_attr_by_labels}) }
#' \item{by_lists_branches_col - set the color of branches from the root of the tree down to (possibly inner) nodes with specified members (\link{branches_attr_by_lists}) }
#' \item{by_lists_branches_lwd - set the line width of branches from the root of the tree down to (possibly inner) nodes with specified members (\link{branches_attr_by_lists}) }
#' \item{by_lists_branches_lty - set the line type of branches from the root of the tree down to (possibly inner) nodes with specified members (\link{branches_attr_by_lists}) }
#' \item{highlight_branches_col - highlight branches color based on branches' heights (\link{highlight_branches_col}) }
#' \item{highlight_branches_lwd - highlight branches line-width based on branches' heights (\link{highlight_branches_lwd}) }
#' \item{clear_branches - clear branches' attributes (\link{remove_branches_edgePar})}
#' \item{clear_leaves - clear leaves' attributes (\link{remove_branches_edgePar})}
#' }
#'
#'
#' @seealso
#'
#' \link{labels<-.dendrogram}, \link{labels_colors<-},
#' \link{hang.dendrogram}, \link{color_branches},
#' \link{assign_values_to_leaves_nodePar},
#' \link{assign_values_to_branches_edgePar},
#' \link{remove_branches_edgePar}, \link{remove_leaves_nodePar},
#' \link{noded_with_condition}, \link{branches_attr_by_labels}, \link{branches_attr_by_lists},
#' \link{dendrogram}
#'
#' @return
#' An updated dendrogram (or dendlist), with some change to
#' the parameters of it
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(23235)
#' ss <- sample(1:150, 10)
#'
#' # Getting the dend object
#' dend <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend %>% plot()
#'
#' dend %>% labels()
#' dend %>%
#'   set("labels", 1:10) %>%
#'   labels()
#' dend %>%
#'   set("labels", 1:10) %>%
#'   plot()
#' dend %>%
#'   set("labels_color") %>%
#'   plot()
#' dend %>%
#'   set("labels_col", c(1, 2)) %>%
#'   plot() # Works also with partial matching :)
#' dend %>%
#'   set("labels_cex", c(1, 1.2)) %>%
#'   plot()
#' dend %>%
#'   set("leaves_pch", NA) %>%
#'   plot()
#' dend %>%
#'   set("leaves_pch", c(1:5)) %>%
#'   plot()
#' dend %>%
#'   set("leaves_pch", c(19, 19, NA)) %>%
#'   set("leaves_cex", c(1, 2)) %>%
#'   plot()
#' dend %>%
#'   set("leaves_pch", c(19, 19, NA)) %>%
#'   set("leaves_cex", c(1, 2)) %>%
#'   set("leaves_col", c(1, 1, 2, 2)) %>%
#'   plot()
#' dend %>%
#'   set("hang") %>%
#'   plot()
#'
#' # using bg for leaves and nodes
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 25)
#' 
#' # Getting the dend object
#' dend25 <- iris[ss, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' 
#' dend25 %>%
#'   set("labels", 1:25) %>%
#'   set("nodes_pch", 21) %>% # set all nodes to be pch 21
#'   set("nodes_col", "darkred") %>%
#'   set("nodes_bg", "gold") %>%
#'   set("leaves_pch", 1:25) %>% # Change the leaves pch to move from 1 to 25
#'   set("leaves_col", "darkred") %>%
#'   set("leaves_bg", "gold") %>%
#'   plot(main = "pch 21 to 25 supports the\nnodes_bg and leaves_bg parameters")
#'   
#'   
#' dend %>%
#'   set("branches_k_col") %>%
#'   plot()
#' dend %>%
#'   set("branches_k_col", c(1, 2)) %>%
#'   plot()
#' dend %>%
#'   set("branches_k_col", c(1, 2, 3), k = 3) %>%
#'   plot()
#' dend %>%
#'   set("branches_k_col", k = 3) %>%
#'   plot()
#'
#' dend %>%
#'   set("branches_k_lty", k = 3) %>%
#'   plot()
#' dend %>%
#'   set("branches_k_col", k = 3) %>%
#'   set("branches_k_lty", k = 3) %>%
#'   plot()
#'
#' dend %>%
#'   set("branches_col", c(1, 2, 1, 2, NA)) %>%
#'   plot()
#' dend %>%
#'   set("branches_lwd", c(2, 1, 2)) %>%
#'   plot()
#' dend %>%
#'   set("branches_lty", c(1, 2, 1)) %>%
#'   plot()
#'
#' #    clears all of the things added to the leaves
#' dend %>%
#'   set("labels_color", c(19, 19, NA)) %>%
#'   set("leaves_pch", c(19, 19, NA)) %>% # plot
#'   set("clear_leaves") %>% # remove all of what was done until this point
#'   plot()
#' # Different order
#' dend %>%
#'   set("leaves_pch", c(19, 19, NA)) %>%
#'   set("labels_color", c(19, 19, NA)) %>%
#'   set("clear_leaves") %>%
#'   plot()
#'
#'
#' # doing this without chaining (%>%) will NOT be fun:
#' dend %>%
#'   set("labels", 1:10) %>%
#'   set("labels_color") %>%
#'   set("branches_col", c(1, 2, 1, 2, NA)) %>%
#'   set("branches_lwd", c(2, 1, 2)) %>%
#'   set("branches_lty", c(1, 2, 1)) %>%
#'   set("hang") %>%
#'   plot()
#'
#' par(mfrow = c(1, 3))
#' dend %>%
#'   set("highlight_branches_col") %>%
#'   plot()
#' dend %>%
#'   set("highlight_branches_lwd") %>%
#'   plot()
#' dend %>%
#'   set("highlight_branches_col") %>%
#'   set("highlight_branches_lwd") %>%
#'   plot()
#' par(mfrow = c(1, 1))
#'
#' #----------------------------
#' # Examples for: by_labels_branches_col, by_labels_branches_lwd, by_labels_branches_lty
#'
#' old_labels <- labels(dend)
#' dend %>%
#'   set("labels", seq_len(nleaves(dend))) %>%
#'   set("by_labels_branches_col", c(1:4, 7)) %>%
#'   set("by_labels_branches_lwd", c(1:4, 7)) %>%
#'   set("by_labels_branches_lty", c(1:4, 7)) %>%
#'   set("labels", old_labels) %>%
#'   plot()
#'
#' dend %>%
#'   set("labels", seq_len(nleaves(dend))) %>%
#'   set("by_labels_branches_col", c(1:4, 7), type = "any", TF_values = c(4, 2)) %>%
#'   set("by_labels_branches_lwd", c(1:4, 7), type = "all", TF_values = c(4, 1)) %>%
#'   set("by_labels_branches_lty", c(1:4, 7), TF_values = c(4, 1)) %>%
#'   plot()
#'
#' #---- using order_value
#' # This is probably not what you want, since cutree
#' # returns clusters in the order of the original data:
#' dend %>%
#'   set("labels_colors", cutree(dend, k = 3)) %>%
#'   plot()
#' # The way to fix it, is to use order_value = TRUE
#' # so that value is assumed to be in the order of the data:
#' dend %>%
#'   set("labels_colors", cutree(dend, k = 3), order_value = TRUE) %>%
#'   plot()
#'
#'
#' #----------------------------
#' # Example for: by_lists_branches_col, by_lists_branches_lwd, by_lists_branches_lty
#'
#' L <- list(c("109", "123", "126", "145"), "29", c("59", "67", "97"))
#' dend %>%
#'   set("by_lists_branches_col", L, TF_value = "blue") %>%
#'   set("by_lists_branches_lwd", L, TF_value = 4) %>%
#'   set("by_lists_branches_lty", L, TF_value = 3) %>%
#'   plot()
#'
#'
#' #----------------------------
#' # A few dendlist examples:
#' dendlist(dend, dend) %>%
#'   set("hang") %>%
#'   plot()
#' dendlist(dend, dend) %>%
#'   set("branches_k_col", k = 3) %>%
#'   plot()
#' dendlist(dend, dend) %>%
#'   set("labels_col", c(1, 2)) %>%
#'   plot()
#'
#' dendlist(dend, dend) %>%
#'   set("hang") %>%
#'   set("labels_col", c(1, 2), which = 1) %>%
#'   set("branches_k_col", k = 3, which = 2) %>%
#'   set("labels_cex", 1.2) %>%
#'   plot()
#'
#'
#' #----------------------------
#' # example of modifying the dendrogram in a heatmap:
#'
#' library(gplots)
#' data(mtcars)
#' x <- as.matrix(mtcars)
#' rc <- rainbow(nrow(x), start = 0, end = .3)
#' cc <- rainbow(ncol(x), start = 0, end = .3)
#'
#' ##
#' ##' demonstrate the effect of row and column dendrogram options
#' ##
#' Rowv_dend <- x %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("branches_k", k = 3) %>%
#'   set("branches_lwd", 2) %>%
#'   ladderize() # rotate_DendSer
#' Colv_dend <- t(x) %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("branches_k", k = 3) %>%
#'   set("branches_lwd", 2) %>%
#'   ladderize() # rotate_DendSer
#' heatmap.2(x, Rowv = Rowv_dend, Colv = Colv_dend)
#' }
set <- function(dend, ...) {
  UseMethod("set")
}


#' @export
#' @rdname set
set.dendrogram <-
  function(dend,
             what = c(
               "labels",
               "labels_colors",
               "labels_cex",
               "labels_to_character",
               "leaves_pch",
               "leaves_cex",
               "leaves_col",
               "leaves_bg",
               "nodes_pch",
               "nodes_cex",
               "nodes_col",
               "nodes_bg",
               "hang_leaves",
               "rank_branches",
               "branches_k_color",
               "branches_k_lty",
               "branches_col",
               "branches_lwd",
               "branches_lty",
               "by_labels_branches_col",
               "by_labels_branches_lwd",
               "by_labels_branches_lty",
               "by_lists_branches_col",
               "by_lists_branches_lwd",
               "by_lists_branches_lty",
               "highlight_branches_col",
               "highlight_branches_lwd",
               "clear_branches",
               "clear_leaves"
             ),
             value,
             order_value = FALSE,
             ...) {
    if (missing(what)) {
      if (dendextend_options("warn")) warning("'what' is missing, returning the dendrogram as is")
      return(dend)
    }

    if (order_value) value <- value[order.dendrogram(dend)]

    what <- match.arg(what)
    dend <- switch(what,
      #                     labels = dendextend:::`labels<-.dendrogram`(dend, value = value)
      labels = `labels<-.dendrogram`(dend, value = value, ...),
      labels_colors = color_labels(dend, col = value, ...),
      #      labels_colors = `labels_colors<-`(dend, value = value, ...),
      #      labels_colors = assign_values_to_leaves_nodePar(dend, value, "lab.col", ...),
      labels_cex = assign_values_to_leaves_nodePar(dend, value, "lab.cex", ...),
      labels_to_character = set(dend, what = "labels", value = as.character(labels(dend)), ...),
      leaves_pch = assign_values_to_leaves_nodePar(dend, value, "pch", ...),
      leaves_cex = assign_values_to_leaves_nodePar(dend, value, "cex", ...),
      leaves_col = assign_values_to_leaves_nodePar(dend, value, "col", ...),
      leaves_bg = assign_values_to_leaves_nodePar(dend, value, "bg", ...),
      nodes_pch = assign_values_to_nodes_nodePar(dend, value, "pch", ...),
      nodes_cex = assign_values_to_nodes_nodePar(dend, value, "cex", ...),
      nodes_col = assign_values_to_nodes_nodePar(dend, value, "col", ...),
      nodes_bg = assign_values_to_nodes_nodePar(dend, value, "bg", ...),
      hang_leaves = hang.dendrogram(dend, hang = ifelse(missing(value), .1, value), ...),
      rank_branches = rank_branches(dend, ...),
      branches_k_color = color_branches(dend, col = value, ...),
      branches_k_lty = lty_branches(dend, lty = value, ...),
      branches_col = assign_values_to_branches_edgePar(dend, value = value, edgePar = "col", ...),
      branches_lwd = assign_values_to_branches_edgePar(dend, value = value, edgePar = "lwd", ...),
      branches_lty = assign_values_to_branches_edgePar(dend, value = value, edgePar = "lty", ...),
      by_labels_branches_col = branches_attr_by_labels(dend, labels = value, attr = "col", ...),
      by_labels_branches_lwd = branches_attr_by_labels(dend, labels = value, attr = "lwd", ...),
      by_labels_branches_lty = branches_attr_by_labels(dend, labels = value, attr = "lty", ...),
      by_lists_branches_col = branches_attr_by_lists(dend, lists = value, attr = "col", ...),
      by_lists_branches_lwd = branches_attr_by_lists(dend, lists = value, attr = "lwd", ...),
      by_lists_branches_lty = branches_attr_by_lists(dend, lists = value, attr = "lty", ...),
      highlight_branches_col = highlight_branches_col(dend, values = value, ...),
      highlight_branches_lwd = highlight_branches_lwd(dend, values = value, ...),
      clear_branches = remove_branches_edgePar(dend, ...),
      clear_leaves = remove_leaves_nodePar(dend, ...)
    )
    dend
  }



#' @export
#' @rdname set
set.dendlist <- function(dend, ..., which) {
  if (missing(which)) which <- 1:length(dend)

  for (i in which) {
    dend[[i]] <- set(dend[[i]], ...)
  }
  dend
}


#' @export
#' @rdname set
set.data.table <- function(...) {
  warning("set function has been overridden from data.table - which means that data.table's set() is now slower. You may solve this by using the prefix 'data.table::'' in your for loop.")
  # library(data.table)
  data.table::set(...)
}
