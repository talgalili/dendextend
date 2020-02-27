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

# Creating the "dendlist" class for more easily dealing with tanglegrams and so on


# Update the labels of a dist:

#' @export
`labels<-.dist` <- function(object, ..., value) {
  attr(object, "Labels") <- value
  object
}




Size.dist <- function(d) {
  attr(d, "Size")
}



#' @title Turns a dist object to a "long" table
#' @description Turns a dist object from a "wide" to a "long" table
#' @export
#' @param d a distance object
#' @param ... not used
#' @return
#' A data.frame with two columns of rows and column names of the dist object
#' and a third column (distance) with the distance between the two.
#' @examples
#' data(iris)
#' iris[2:6, -5] %>%
#'   dist() %>%
#'   data.matrix()
#' iris[2:6, -5] %>%
#'   dist() %>%
#'   as.vector()
#' iris[2:6, -5] %>%
#'   dist() %>%
#'   dist_long()
#' # This can later be used to making a network plot based on the distances.
dist_long <- function(d, ...) {
  n_seq <- seq_len(Size.dist(d))
  rows_cols_grid <- expand.grid(rows = n_seq, cols = n_seq)
  labels_grid <- expand.grid(rows = labels(d), cols = labels(d))
  ss_keep <- rows_cols_grid[, 1] > rows_cols_grid[, 2]
  # rows_cols[ss_keep,]
  data.frame(labels_grid[ss_keep, ], distance = as.vector(d), stringsAsFactors = TRUE)
}
