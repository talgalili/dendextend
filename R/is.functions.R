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



#' @title Is the object of some class
#' @name is_some_class
#' @description
#' Returns TRUE if some class (based on the name of the function).
#' @param x an object.
#' @return
#' Returns TRUE if some class (based on the name of the function).
#' @examples
#' # TRUE:
#' is.dendlist(dendlist())
#' # FALSE
#' is.dendlist(1)
#' # TRUE:
#' is.dist(dist(mtcars))
#' # FALSE
#' is.dist(mtcars)
NULL

#' @export
#' @rdname is_some_class
is.hclust <- function(x) {
  inherits(x, "hclust")
}

#' @export
#' @rdname is_some_class
is.dendrogram <- function(x) {
  inherits(x, "dendrogram")
}

#' @export
#' @rdname is_some_class
is.phylo <- function(x) {
  inherits(x, "phylo")
}




#' @export
#' @rdname is_some_class
is.dendlist <- function(x) {
  inherits(x, "dendlist")
}





#' @export
#' @rdname is_some_class
is.dist <- function(x) {
  inherits(x, "dist")
}



#' @title Turns a factor into a number
#' @export
#' @description
#' Turning a factor into a number is not trivial.
#' Using \code{as.numeric} would only return to us the indicator numbers
#' and NOT the factor levels turned into a number.
#' fac2num simply turns a factor into a number, as we often need.
#' @param x an object.
#' @param force_integer logical (FALSE). Should the values returned be integers?
#' @param keep_names logical (TRUE). Should the values returned keep the \link{names}
#' of the original vector?
#' @param ... ignored.
#' @return if x is an object - it returns logical - is the object of class dendrogram.
#' @examples
#'
#' x <- factor(3:5)
#' as.numeric(x) # 1 2 3
#' fac2num(x) # 3 4 5
fac2num <- function(x, force_integer = FALSE, keep_names = TRUE, ...) {
  if (!is.factor(x)) stop("x must be a factor (in order to turn it into a number)")
  new_x <- as.numeric(as.character(x))
  if (force_integer) new_x <- as.integer(new_x)
  if (keep_names) names(new_x) <- names(x)
  return(new_x)
}
