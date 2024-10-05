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







#' @title Tries to run DendSer on a dendrogram
#' @export
#' @description
#' Implements dendrogram seriation.
#' The function tries to turn the dend into hclust, on
#' which it runs \link[DendSer]{DendSer}.
#'
#' Also, if a distance matrix is missing, it will try
#' to use the \link{cophenetic} distance.
#' @param dend An object of class dendrogram
#' @param ser_weight Used by cost function to evaluate
#'  ordering. For cost=costLS, this is a vector of
#'   object weights. Otherwise is a dist or symmetric matrix.
#' passed to DendSer.
#' If it is missing, the cophenetic distance is used instead.
#' @param ... parameters passed to \link[DendSer]{DendSer}
#' @return Numeric vector giving an optimal dendrogram order
#' @seealso \code{\link[DendSer]{DendSer}}, \link{DendSer.dendrogram} ,
#' \link{untangle_DendSer}, \link{rotate_DendSer}
#' @examples
#' \dontrun{
#' library(DendSer) # already used from within the function
#' hc <- hclust(dist(USArrests[1:4, ]), "ave")
#' dend <- as.dendrogram(hc)
#' DendSer.dendrogram(dend)
#' }
DendSer.dendrogram <- function(dend, ser_weight, ...) {
  h <- as.hclust(dend)
  if (missing(ser_weight)) ser_weight <- cophenetic(dend)
  # library(DendSer)
  DendSer::DendSer(h = h, ser_weight = ser_weight, ...)
}


#' @title Rotates dend based on DendSer
#' @export
#' @description
#' Rotates a dendrogram based on its seriation
#'
#' The function tries to turn the dend into hclust using
#' \link{DendSer.dendrogram} (based on \link[DendSer]{DendSer})
#'
#' Also, if a distance matrix is missing, it will try
#' to use the \link{cophenetic} distance.
#' @param dend An object of class dendrogram
#' @param ser_weight Used by cost function to evaluate
#'  ordering. For cost=costLS, this is a vector of
#'   object weights. Otherwise is a dist or symmetric matrix.
#' passed to \link{DendSer.dendrogram} and from
#' there to \link[DendSer]{DendSer}.
#'
#' If it is missing, the cophenetic distance is used instead.
#' @param ... parameters passed to \link[DendSer]{DendSer}
#' @return Numeric vector giving an optimal dendrogram order
#' @seealso \code{\link[DendSer]{DendSer}}, \link{DendSer.dendrogram} ,
#' \link{untangle_DendSer}, \link{rotate_DendSer}
#' @examples
#' \dontrun{
#' library(DendSer) # already used from within the function
#'
#' dend <- USArrests[1:4, ] %>%
#'   dist() %>%
#'   hclust("ave") %>%
#'   as.dendrogram()
#' DendSer.dendrogram(dend)
#'
#' tanglegram(dend, rotate_DendSer(dend))
#' }
rotate_DendSer <- function(dend, ser_weight, ...) {
  ord <- tryCatch(DendSer.dendrogram(dend, ser_weight = ser_weight), error = function(e) seq_len(nleaves(dend)))
  #    print(ord)
  rotate(dend, ord)
}

#' @title Tries to run DendSer on a dendrogram
#' @export
#' @description
#' The function tries to turn the dend into hclust.
#' It then uses the \link{cophenetic} distance matrix
#' for optimizing the tree's rotation.
#'
#' This is a good (and fast) starting point for \link{untangle_step_rotate_2side}
#' @param dend An object of class \link{dendlist}
#' @param ... NOT USED
#' @return A dendlist object with ordered dends
#' @seealso \code{\link[DendSer]{DendSer}}, \link{DendSer.dendrogram} ,
#' \link{untangle_DendSer}, \link{rotate_DendSer}
#' @examples
#' \dontrun{
#' set.seed(232)
#' ss <- sample(1:150, 20)
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
#' # bad solutions
#' dend12 %>% tanglegram()
#' dend12 %>%
#'   untangle("step2") %>%
#'   tanglegram()
#' dend12 %>%
#'   untangle_DendSer() %>%
#'   tanglegram()
#' # but the combination is quite awsome:
#' dend12 %>%
#'   untangle_DendSer() %>%
#'   untangle("step2") %>%
#'   tanglegram()
#' }
untangle_DendSer <- function(dend, ...) {
  dendlist(
    rotate_DendSer(dend[[1]]),
    rotate_DendSer(dend[[2]])
  )
}
