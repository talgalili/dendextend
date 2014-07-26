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




#' @title Last Observation Carried Forward
#' @export
#' @description
#' A function for replacing each NA with the most recent non-NA prior to it.
#' @param x some vector
#' @param first_na_value If the first observation is NA, fill it with "first_na_value"
#' @param recursive logical (TRUE). Should na_locf be re-run until all NA values are filled?
#' @param ... ignored.
#' @return 
#' The original vector, but with all the missing values filled by the value 
#' before them.
#' @seealso 
#' \link[zoo]{na.locf}
#' 
#' @source
#' \url{https://stat.ethz.ch/pipermail/r-help/2003-November/042126.html}
#' \url{http://stackoverflow.com/questions/5302049/last-observation-carried-forward-na-locf-on-panel-cross-section-time-series}
#' 
#' This could probably be solved MUCH faster using Rcpp.
#' 
#' @examples
#' na_locf(c(NA, NA))
#' na_locf(c(1, NA))
#' na_locf(c(1, NA, NA, NA))
#' na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4))
#' na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), recursive = FALSE)
#' 
#' \dontrun{
#' 
#' require(microbenchmark)
#' require(zoo)
#' 
#' microbenchmark(
#'    na_locf = na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4)),
#'    na.locf = na.locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4))
#' ) # my implementation is 6 times faster :)
#' 
#' microbenchmark(
#'    na_locf = na_locf(rep(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), 1000)),
#'    na.locf = na.locf(rep(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), 1000))
#' ) # my implementation is 3 times faster 
#' 
#' }
#' 
#' 
na_locf <- function(x, first_na_value = 0, recursive = TRUE, ...) {   
   # where are the NA's:
   x_na <- is.na(x)
   
   # IF we have no NA's, return x:
   if(!any(x_na)) {
      return(x)
      # Else - fill one next observation:
   } else {
      # If the first observation is NA, fill it with "first_na_value"
      if(x_na[1]) x[1] <- first_na_value
      
      x_na_loc <- which(x_na)
      x[x_na_loc] <- x[x_na_loc-1]
      
      if(recursive) {
         Recall(x)         
      } else {
         return(x)
      }      
   }   
}




