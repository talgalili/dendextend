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









#' @title Rotates a dendrogram based on a seriation of a distance matrix
#' @export
#' @description
#' Rotates a dendrogram so it confirms to an order of a provided distance object.
#' The seriation algorithm is based on \link[seriation]{seriate}, which tries to find
#' a linear order for objects using data in form of a dissimilarity matrix (one mode data).
#'
#' This is useful for heatmap visualization.
#'
#' @param dend An object of class \link{dendrogram} or \link{hclust}
#' @param x a \link{dist} object.
#' @param method a character vector of either "OLO" or "GW":
#' "OLO" - Optimal leaf ordering, optimzes the Hamiltonian path length that is restricted by the dendrogram structure - works in O(n^4)
#' "GW" - Gruvaeus and Wainer heuristic to optimze the Hamiltonian path length that is restricted by the dendrogram structure
#'
#' @param ... parameters passed to \link[seriation]{seriate}
#' @return
#' A dendrogram that is rotated based on the optimal ordering of the distance matrix
#' @seealso \code{\link[dendextend]{rotate}}, \link[seriation]{seriate}
#' @examples
#' \dontrun{
#' # library(dendextend)
#' d <- dist(USArrests)
#' hc <- hclust(d, "ave")
#' dend <- as.dendrogram(hc)
#'
#' heatmap(as.matrix(USArrests))
#'
#' dend2 <- seriate_dendrogram(dend, d)
#' heatmap(as.matrix(USArrests), Rowv = dend)
#' }
seriate_dendrogram <- function(dend, x, method = c("OLO", "GW"), ...) {
  if (!requireNamespace("seriation")) stop("Please first install seriation:\n install.packages('seriaten') ")

  if (!is.dendrogram(dend) & !is.hclust(dend)) stop("dend must be either a dendrogram or an hclust object")
  if (!is.dist(x)) stop("x must be a dist object")
  if (!identical(sort(labels(x)), sort(labels(dend)))) stop("The labels of dend and x must be identical.")
  method <- match.arg(method)

  # o <- seriate(d, method = "GW", control = list(hclust = as.hclust(dend)) )
  # get_order(o)
  # library(seriation)
  # hmap(sqrt(d2), Colv = "none", trace = "none", col = viridis(200))
  # Error in (function (x, Rowv = TRUE, Colv = if (symm) "Rowv" else TRUE,  :
  #                       formal argument "Colv" matched by multiple actual arguments
  o <- seriation::seriate(x, method = method, control = list(hclust = as.hclust(dend)), ...)
  # library(dendextend)
  dend <- rotate(dend, order = rev(labels(x)[seriation::get_order(o)]))
  dend
}
