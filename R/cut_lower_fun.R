# Copyright (C) Tal Galili
#
# This file is part of dendextendRcpp.
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



# ' @aliases
# ' old_cut_lower_fun


#' @title Cut a dendrogram - and run a function on the output
#' @export
#' @aliases
#' dendextend_cut_lower_fun
#' @description
#' Cuts the dend at height h and returns a list with the FUN function
#' implemented on all the sub trees created by cut at height h.
#' This is used for creating a \link[dendextend]{cutree.dendrogram} function,
#' by using the \code{labels} function as FUN.
#'
#' This is the Rcpp version of the function, offering a 10-60 times improvement
#' in speed (depending on the tree size it is used on).
#'
#' @param dend a dendrogram object.
#' @param h a scalar of height to cut the dend by.
#' @param FUN a function to run. (default is "labels")
#' @param warn logical (default from dendextend_options("warn") is FALSE).
#' Set if warning are to be issued, it is safer to keep this at TRUE,
#' but for keeping the noise down, the default is FALSE.
#' Should the user be warned if reverting to default?
#' @param ... passed to FUN.
#' @return A list with the output of running FUN on each of the
#' sub dends derived from cutting "dend"
#' @author Tal Galili
#' @seealso \code{\link{labels}}, \code{\link{dendrogram}},
#' \link[dendextend]{cutree.dendrogram}
#' @examples
#'
#' dend <- as.dendrogram(hclust(dist(iris[1:4, -5])))
#' # this is really cool!
#' cut_lower_fun(dend, .4, labels)
#' lapply(cut(dend, h = .4)$lower, labels)
#' cut_lower_fun(dend, .4, order.dendrogram)
#' \dontrun{
#' # library(dendextend)
#' library(dendextendRcpp)
#' dend_big <- as.dendrogram(hclust(dist(iris[1:150, -5])))
#' library(microbenchmark)
#' microbenchmark(old_cut_lower_fun(dend_big, .1),
#'   dendextendRcpp::dendextendRcpp_cut_lower_fun(dend_big, .1),
#'   times = 100
#' )
#' # about 7-15 times faster. It is faster the larger the tree is, and the lower h is.
#' }
#'
cut_lower_fun <- function(dend, h, FUN = labels, warn = dendextend_options("warn"), ...) {
  if (!is.dendrogram(dend)) stop("'dend' needs to be a dendrogram. Aborting the function 'cut_lower_labels'.")

  if (is.leaf(dend)) {
    return(list(FUN(dend)))
  }
  # else:
  #    dend_and_FUN <- function(x) {
  #       class(x) = "dendrogram"
  #       FUN(x,...)
  #    }
  #    return(lapply(Rcpp_cut_lower(dend, h), dend_and_FUN))

  return(lapply(cut(dend, h = h)$lower, FUN)) # If the proper labels are not important, this function is around 10 times faster than using labels (so it is much better for some other algorithms)
}


# cut(dend, h = .14)$lower

# detach( 'package:dendextendRcpp', unload=TRUE )
# library( 'dendextendRcpp' )
# labels(dend)
