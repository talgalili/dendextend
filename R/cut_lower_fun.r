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
#' Cuts the a tree at height h and returns a list with the FUN function
#' implemented on all the sub trees created by cut at height h.
#' This is used for creating a \link[dendextend]{cutree.dendrogram} function,
#' by using the \code{labels} function as FUN.
#' 
#' This is the Rcpp version of the function, offering a 10-60 times improvement
#' in speed (depending on the tree size it is used on).
#' 
#' @param tree a dendrogram object.
#' @param h a scalar of height to cut the tree by.
#' @param FUN a function to run. (default is "labels")
#' @param warn logical (FALSE) - should the user be warned if reverting to
#' default? (I set it to FALSE since it can be very noisy sometimes...)
#' @param ... passed to FUN.
#' @return A list with the output of running FUN on each of the 
#' sub trees derived from cutting "tree"
#' @author Tal Galili
#' @seealso \code{\link{labels}}, \code{\link{dendrogram}},
#' \link[dendextend]{cutree.dendrogram}
#' @examples
#' 
#' dend = as.dendrogram(hclust(dist(iris[1:4,-5])))
#' # this is really cool!
#' cut_lower_fun(dend, .4, labels)
#' lapply(cut(dend, h = .4)$lower, labels)   
#' cut_lower_fun(dend, .4, order.dendrogram)
#' 
#' 
#' \dontrun{
#'    require(dendextend)
#'    require(dendextendRcpp)
#'    dend_big = as.dendrogram(hclust(dist(iris[1:150,-5])))
#'    require(microbenchmark)
#'    microbenchmark(old_cut_lower_fun(dend_big,4),
#'                   dendextendRcpp::cut_lower_fun(dend_big,4),
#'                      times = 100)
#'    # about 20 times faster. It is faster the larger the tree is.
#' }
#' 
cut_lower_fun <- function(tree, h, FUN = labels, warn = FALSE, ...) {
#    fo <- options()$dendextend_cut_lower_fun
#    if(is.null(fo)) fo <- dendextend::cut_lower_fun_dendextend
   # the above is NOT faster then what is below
   fo <- dendextend_options("cut_lower_fun")   
   fo(tree=tree, h=h, FUN = FUN, warn = warn, ...)
}



#' @export
dendextend_cut_lower_fun <- function(tree, h, FUN = labels, warn = FALSE, ...) {
   
   if(!is.dendrogram(tree)) stop("'tree' needs to be a dendrogram. Aborting the function 'cut_lower_labels'.")
   
   if(is.leaf(tree)) return(list(FUN(tree)))
   # else:
#    dend_and_FUN <- function(x) {
#       class(x) = "dendrogram"
#       FUN(x,...)
#    }
#    return(lapply(Rcpp_cut_lower(tree, h), dend_and_FUN))
   
   return(lapply(cut(tree, h = h)$lower, FUN))   # If the proper labels are not important, this function is around 10 times faster than using labels (so it is much better for some other algorithms)
   
}


# cut(tree, h = .14)$lower

# detach( 'package:dendextendRcpp', unload=TRUE )
# require( 'dendextendRcpp' )
# labels(dend)



