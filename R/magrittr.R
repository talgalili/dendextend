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




#' Pipe an object forward into a function call/expression.
#' @export
#' @rdname pipe
#' @param lhs The value to be piped
#' @param rhs A function or expression
#' @source
#' This is just the imported function 
#' from the magrittr package. The documentation you should
#' read for the \%>\% function can be found here: \link[magrittr]{pipe}
#' 
#' Adding the function in the package while importing it 
#' is a trick taken from the {dplyr} package
#' (in the file chain.r)
#' 
#' The package could have also been placed in "Depends",
#' But I wanted to keep it robust if at some point some other 
#' package will try to take over the symbol (not likely, but still).
#' 
#' @seealso 
#' \link[magrittr]{pipe}
`%>%` <- magrittr::`%>%`

