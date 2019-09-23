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


normalize <- function(x) {
  # https://stat.ethz.ch/pipermail/r-help//2012-October/336676.html
  # http://stats.stackexchange.com/questions/70801/how-to-normalize-data-to-0-1-range
  # https://en.wikipedia.org/wiki/Feature_scaling
  # https://en.wikipedia.org/wiki/Normalization_(statistics)
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

map_values_to_scale <- function(values, scale) {
  location <- round(normalize(values) * (length(scale) - 1)) + 1
  scale[location]
}


#' @export
#' @rdname highlight_branches
highlight_branches_col <- function(dend, values = rev(viridis(1000, end = .9)), ...) {
  if (missing(values)) values <- rev(viridis(1000, end = .9))

  # library(viridis)
  # h <- get_branches_heights(dend, sort = FALSE, include_leaves = T)
  # h <- get_nodes_attr(dend, "height", include_leaves = TRUE, na.rm = TRUE) # works
  h <- get_branches_heights(dend, sort = FALSE, include_leaves = TRUE) # works
  # h <- round(normalize(h) * (length(cols)-1) ) + 1
  # library(scales)
  # f <- scales::col_numeric(rev(cols), h)
  # c(f(h))
  # cols <- cols[h]
  cols <- map_values_to_scale(h, values)
  dend2 <- set(dend, "branches_col", cols)
  # plot(dend2)

  dend2
}


#' @export
#' @rdname highlight_branches
highlight_branches_lwd <- function(dend, values = seq(1, 10, length.out = 1000), ...) {
  if (missing(values)) values <- seq(1, 10, length.out = 1000)

  # library(viridis)
  # h <- get_branches_heights(dend, sort = FALSE, include_leaves = T)
  # h <- get_nodes_attr(dend, "height", include_leaves = TRUE, na.rm = TRUE) # works
  h <- get_branches_heights(dend, sort = FALSE, include_leaves = TRUE) # works

  lwds <- map_values_to_scale(h, values)
  # dend2 <- set(dend, "branches_col", cols)

  dend2 <- set(dend, "branches_lwd", lwds)
  # plot(dend2)

  dend2
}






#' @title Highlight a dendrogram's branches heights via color and line-width
#' @rdname highlight_branches
#' @export
#'
#' @description
#' Highlights (update) the color (col) and/or line width (lwd) of each branch in
#' a dendrogram based on it's node's height.
#' This is a powerful pre-processing for a \link{tanglegram} plot of two dendrograms, as
#' it emphasizes the toplogical structure of each tree (and hence, their similarity and differences).
#'
#' The colors are based on the viridis pallette, and the line width is on the range of 1 to 10.
#' These can be manually changed when using highlight_branches_col and highlight_branches_lwd
#' respectively.
#'
#' @param dend a \link{dendrogram} tree (to be turned into a ggdend object)
#' @param type a character vector. Either "col", "lwd", or both. Based on
#' whichever is chosen the dendrogram's branches will be updated.
#' @param values the gradient of values to be used for each branch.
#' The colors are based on the viridis pallette, and the line width is on the range of 1 to 10.
#' These can be manually changed when using highlight_branches_col and highlight_branches_lwd
#' respectively.
#' @param ... Currently ignored.
#'
#' @seealso
#'
#' \link{set}, \link{color_branches}, \link{get_branches_heights},
#' \link[viridis]{viridis}
#'
#' @return
#' A modified \link{dendrogram}, with colors/line-width in the branches
#' that are proportional to each branche's height (measured by its lower tip).
#'
#' @examples
#'
#'
#' dat <- iris[1:20, -5]
#' hca <- hclust(dist(dat))
#' hca2 <- hclust(dist(dat), method = "single")
#' dend <- as.dendrogram(hca)
#' dend2 <- as.dendrogram(hca2)
#'
#' par(mfrow = c(1, 3))
#' dend %>%
#'   highlight_branches_col() %>%
#'   plot(main = "Coloring branches")
#' dend %>%
#'   highlight_branches_lwd() %>%
#'   plot(main = "Emphasizing line-width")
#' dend %>%
#'   highlight_branches() %>%
#'   plot(main = "Emphasizing color\n and line-width")
#'
#' library(viridis)
#' par(mfrow = c(1, 3))
#' dend %>%
#'   highlight_branches_col() %>%
#'   plot(main = "Coloring branches \n(default is reversed viridis)")
#' dend %>%
#'   highlight_branches_col(viridis(100)) %>%
#'   plot(main = "It is better to use\nlighter colors in the leaves")
#' dend %>%
#'   highlight_branches_col(rev(magma(1000))) %>%
#'   plot(main = "The magma color pallatte\n is also good")
#'
#' dl <- dendlist(dend, dend2)
#' tanglegram(dl,
#'   sort = TRUE, common_subtrees_color_lines = FALSE,
#'   highlight_distinct_edges = FALSE, highlight_branches_lwd = FALSE
#' )
#' tanglegram(dl)
#' tanglegram(dl, fast = TRUE)
#'
#' dl <- dendlist(highlight_branches(dend), highlight_branches(dend2))
#' tanglegram(dl, sort = TRUE, common_subtrees_color_lines = FALSE, highlight_distinct_edges = FALSE)
#'
#' dend %>%
#'   set("highlight_branches_col") %>%
#'   plot()
#'
#' dl <- dendlist(dend, dend2) %>% set("highlight_branches_col")
#' tanglegram(dl, sort = TRUE, common_subtrees_color_lines = FALSE, highlight_distinct_edges = FALSE)
#'
#'
#' # This is also useful for heatmaps
#' # --------------------------
#' # library(dendextend)
#'
#' x <- as.matrix(datasets::mtcars)
#'
#' Rowv <- x %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("branches_k_color", k = 3) %>%
#'   set("highlight_branches_lwd") %>%
#'   ladderize()
#' #    rotate_DendSer(ser_weight = dist(x))
#' Colv <- x %>%
#'   t() %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("branches_k_color", k = 2) %>%
#'   set("highlight_branches_lwd") %>%
#'   ladderize()
#' #    rotate_DendSer(ser_weight = dist(t(x)))
#'
#' library(gplots)
#' heatmap.2(x, Rowv = Rowv, Colv = Colv)
highlight_branches <- function(dend, type = c("col", "lwd"), ...) {
  if (!is.dendrogram(dend)) stop("dend is not a dendrogram (and it needs to be...)")
  # type <- match.arg(type)

  if ("col" %in% type) dend <- highlight_branches_col(dend)
  if ("lwd" %in% type) dend <- highlight_branches_lwd(dend)

  dend
}
