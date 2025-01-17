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





#' @title Plot a circlized dendrograms
#' @export
#' @author Zuguang Gu, Tal Galili
#' @description
#' Plot a circlized dendrograms using the circlize package (must be installed for the function to work).
#'
#' This type of plot is also sometimes called fan tree plot (although the name fan-plot is also used for a different
#' plot in time series analysis), radial tree plot, polar tree plot, circular tree plot, and probably other names as well.
#'
#' An advantage for using the circlize package directly is for plotting a
#' circular dendrogram so that you can add more graphics for the elements
#' in the tree just by adding more tracks using \link[circlize]{circos.track}.
#'
#' @param dend a \link{dendrogram} object
#' @param facing Is the dendromgrams facing inside to the circle or outside.
#' @param labels logical (TRUE) - should the labels be plotted as well.
#' @param labels_track_height a value for adjusting the room for the labels.
#' It is 0.2 by default, but if NULL or NA, it will adjust automatically based on
#' the max width of the labels. However, if this is too long, the plot will give an error:
#'       Error in check.track.position(track.index, track.start, track.height) :
#'       not enough space for cells at track index '2'.
#' @param dend_track_height a value for adjusting the room for the dendrogram.
#'
#' @param ... Ignored.
#'
#' @seealso
#' \link[circlize]{circos.dendrogram}
#'
#' @source
#' This code is based on the work of Zuguang Gu. If you use the function, please cite both
#' dendextend (see: \code{citation("dendextend")}), as well as the circlize package (see: \code{citation("circlize")}).
#'
#' @return
#' The dend that was used for plotting.
#'
#' @examples
#'
#' \dontrun{
#'
#' dend <- iris[1:40, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("branches_k_color", k = 3) %>%
#'   set("branches_lwd", c(5, 2, 1.5)) %>%
#'   set("branches_lty", c(1, 1, 3, 1, 1, 2)) %>%
#'   set("labels_colors") %>%
#'   set("labels_cex", c(.9, 1.2)) %>%
#'   set("nodes_pch", 19) %>%
#'   set("nodes_col", c("orange", "black", "plum", NA))
#'
#' circlize_dendrogram(dend)
#' circlize_dendrogram(dend, labels = FALSE)
#' circlize_dendrogram(dend, facing = "inside", labels = FALSE)
#'
#'
#' # In the following we get the dendrogram but can also get extra information on top of it
#' circos.initialize("foo", xlim = c(0, 40))
#' circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#'   circos.rect(1:40 - 0.8, rep(0, 40), 1:40 - 0.2, runif(40), col = rand_color(40), border = NA)
#' }, bg.border = NA)
#' circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#'   circos.text(1:40 - 0.5, rep(0, 40), labels(dend),
#'     col = labels_colors(dend),
#'     facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5)
#'   )
#' }, bg.border = NA, track.height = 0.1)
#' max_height <- attr(dend, "height")
#' circos.track(ylim = c(0, max_height), panel.fun = function(x, y) {
#'   circos.dendrogram(dend, max_height = max_height)
#' }, track.height = 0.5, bg.border = NA)
#' circos.clear()
#' }
circlize_dendrogram <- function(dend, facing = c("outside", "inside"), labels = TRUE,
                                labels_track_height = 0.1,
                                dend_track_height = 0.5,
                                ...) {
  if (!requireNamespace("circlize")) {
    stop("The 'circlize' package is not installed on your system.\nYou first need to run:\n install.packages('circlize') \nbefore you can use the circlize_dendrogram function.")
  }

  n_labaels <- nleaves(dend)
  dend_labels_colors <- labels_colors(dend)
  dend_labels_cex <- labels_cex(dend)
  labels_dend <- labels(dend)
  if (as.logical(anyDuplicated(labels_dend))) {
    warning("Not all labels are unique. Therefore, we pad the labels with a running number, so to be able to produce the plot.")
    labels(dend) <- paste0(seq_along(labels_dend), "_", labels_dend)
    labels_dend <- labels(dend)
  }



  if (is.null(dend_labels_cex)) dend_labels_cex <- rep(1, n_labaels)

  circlize::circos.initialize("dendrogram", xlim = c(0, n_labaels))

  if (is.null(labels_track_height) | is.na(labels_track_height)) labels_track_height <- max(strwidth(labels_dend))


  # plot labels
  if (labels) {
    circlize::circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
      circlize::circos.text(
        x = 1:n_labaels - 0.5,
        y = rep(0, n_labaels),
        labels = labels_dend,
        cex = dend_labels_cex,
        col = dend_labels_colors,
        facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5)
      )
    }, bg.border = NA, track.height = labels_track_height)
  }

  # plot tree
  max_height <- attr(dend, "height")
  circlize::circos.track(ylim = c(0, max_height), panel.fun = function(x, y) {
    circlize::circos.dendrogram(dend, facing = facing, max_height = max_height)
  }, track.height = dend_track_height, bg.border = NA)


  circlize::circos.clear()

  invisible(dend)
}

# labels_cex(dend) <- 3
# plot(dend)
# circlize_dendrogram(dend, labels_ylim = c(0,100))
#


#
# library(dendextend)
#
#  circlize_dendrogram(dend)
#
#
