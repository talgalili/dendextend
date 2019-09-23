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




# as.dendrogram(as.hclust(as.phylo(hc)))
#' @export
as.dendrogram.phylo <- function(object, ...) {
  # library(ape)
  as.dendrogram(ape::as.hclust.phylo(object))
}


# Make it so that I can have as.phylo.dendrogram work, without forcing the user to have ape imported
as.phylo <- function(x, ...) {
  # library(ape)
  ape::as.phylo(x, ...)
  #    if (length(class(x)) == 1 && class(x) == "phylo")
  #       return(x)
  #    UseMethod("as.phylo")
}



# We can't use an S3 here without switching ape to be a depend/imports, and enhance is more fitting. So I must make this function visible... :\
# ' @method as.phylo dendrogram
# ' @S3method as.phylo dendrogram




#' @title Convert a dendrogram into phylo
#' @description
#' Based on \link{as.hclust.dendrogram} with \link[ape]{as.phylo.hclust}
#'
#' In the future I hope a more direct link will be made.
#'
#' @param x a dendrogram
#' @param ... ignored.
#' @return
#' A phylo class object
#' @seealso \link{as.dendrogram}, \link{as.hclust}, \link[ape]{as.phylo}
#' @examples
#' \dontrun{
#'
#' library(dendextend)
#' library(ape)
#' dend <- iris[1:30, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend2 <- as.phylo(dend)
#' plot(dend2, type = "fan")
#'
#'
#' # Also possible to with ggplot2 :)
#'
#' library(dendextend)
#' library(ggplot2)
#' library(ggdendro)
#' dend <- iris[1:30, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' # Rectangular lines
#' ddata <- dend %>% dendro_data(type = "rectangle")
#' p <- ggplot(segment(ddata)) +
#'   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
#'   coord_flip() +
#'   scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta = "x")
#' p
#' }
#'
#'
#' # see: https://github.com/klutometis/roxygen/issues/796
#' #
#' @rawNamespace
#' if(getRversion() >= "3.6.0") {
#'   S3method(ape::as.phylo, dendrogram)
#' } else {export(as.phylo.dendrogram)}
#'
as.phylo.dendrogram <- function(x, ...) {
  # library(ape)
  ape::as.phylo.hclust(as.hclust(x))
}
## http://stackoverflow.com/questions/13085481/namespace-dependencies-not-required
## I also added ape to "Imports" in DESCRIPTION in order to avoid the error:
##    Namespace dependency not required: 'ape'
## Update: I removed ape from the imports and NAMESPACE.
