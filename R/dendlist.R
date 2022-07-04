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



#' @title Checks if the value is and empty list()
#' @description Checks if the value is and empty list(). Can be useful.
#' @param x whatever object to check
#' @return
#' logical
#' @examples
#' # I can run this only if I'd make is_null_list exported
#' \dontrun{
#' # TRUE:
#' is_null_list(list())
#' # FALSE
#' is_null_list(list(1))
#' is_null_list(1)
#'
#' x <- list(1, list(), 123)
#' ss_list <- sapply(x, is_null_list)
#' x <- x[!ss_list]
#' x
#'
#' x <- list(1, list(), 123)
#' ss_list <- sapply(x, is_null_list)
#' x <- list(list())
#' x
#' }
#'
#' \dontrun{
#' # error
#' is_null_list()
#' }
is_null_list <- function(x) {
  identical(x, list())
}




#' @title Creating a dendlist object from several dendrograms
#' @rdname dendlist
#' @export
#'
#' @description
#' It accepts several dendrograms and or dendlist objects
#' and chain them all together.
#' This function aim to help with the usability of
#' comparing two or more dendrograms.
#'
#' @param ... several dendrogram/hclust/phylo or dendlist objects
#' If an object is hclust or phylo - it will be converted
#' into a dendrogram.
#' @param x a dendlist object
#' @param which an integer vector of length 2, indicating
#' which of the trees in the dendlist object should be plotted (relevant for dendlist)
#'
#' When used inside dendlist, which is still an integer, but it can be of any length,
#' and it can be used to create a smaller dendlist.
#'
#' @details
#' It there are list() in the ..., they are omitted.
#' If ... is missing, it returns an empty dendlist.
#'
#' @return
#' A list of class dendlist where each item
#' is a dendrogram
#' @examples
#'
#' \dontrun{
#'
#' dend <- iris[, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend2 <- iris[, -5] %>%
#'   dist() %>%
#'   hclust(method = "single") %>%
#'   as.dendrogram()
#' dendlist(1:4, 5, a = dend) # Error
#' # dendlist <- function (...) list(...)
#' dendlist(dend)
#' dendlist(dend, dend)
#' dendlist(dend, dend, dendlist(dend))
#' #  notice how the order of
#' dendlist(dend, dend2)
#' dendlist(dend) %>% dendlist(dend2)
#' dendlist(dend) %>%
#'   dendlist(dend2) %>%
#'   dendlist(dend)
#' dendlist(dend, dend2) %>% tanglegram()
#' tanglegram(tree1 = dendlist(dend, dend2))
#'
#' dend <- iris[1:20, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend2 <- iris[1:20, -5] %>%
#'   dist() %>%
#'   hclust(method = "single") %>%
#'   as.dendrogram()
#'
#' x <- dendlist(dend, dend2)
#' plot(x)
#' }
#'
dendlist <- function(..., which) {
  x <- list(...)

  if (!missing(which)) {
    the_names <- names(x[[1]])[which]
  }

  # dendlist on a list will return a length 0 dendlist
  # First, let's remove all "list()" from the object
  ss_list <- sapply(x, is_null_list)
  x <- x[!ss_list]
  # if we are left with nothing, return an empty dendlist
  if (identical(x, list())) {
    # warning("You entered an empty (or no) list to dendlist, a dendlist of length 0 is created.")
    x_final <- list()
    class(x_final) <- "dendlist"
    return(x_final)
  }

  #    object <- as.list(substitute(x))[-1L]
  n <- length(x)

  # If we have objects which are not dend or dendlist - STOP!
  x_classes <- sapply(x, class)

  if (!all(x_classes %in% c("dendrogram", "hclust", "phylo", "dendlist"))) {
    print_x_classes <- paste(x_classes, collapse = ", ")
    stop(
      paste(
        "Some of your object's classes are not of the type dendrogram/hclust/phylo/dendlist. Please review and fix. Their classes are:\n",
        print_x_classes
      )
    )
  }

  # if some objects are hclust/phylo - then turn them into dend:
  x_classes_hclust_phylo <- x_classes %in% c("hclust", "phylo")
  if (any(x_classes_hclust_phylo)) {
    for (i in seq_len(n)) {
      if (x_classes_hclust_phylo[i]) {
        x[[i]] <- as.dendrogram(x[[i]])
      }
    }
  }

  # If all objects are dend - just list them, and return
  x_classes_dend <- x_classes == "dendrogram"
  if (all(x_classes_dend)) {
    # skip...
    x_final <- x
  } else {
    n_final <- sum(x_classes == "dendrogram") +
      x[x_classes == "dendlist"] %>%
      sapply(length) %>%
      sum()
    x_final <- vector("list", n_final)

    i_counter <- 1
    for (i in seq_len(n)) {
      xi <- x[[i]]

      if (is.dendrogram(xi)) {
        x_final[[i_counter]] <- xi
        i_counter <- i_counter + 1
      } else {
        for (j in seq_len(length(xi))) {
          x_final[[i_counter]] <- xi[[j]]
          i_counter <- i_counter + 1
        }
      }
    }
  }


  # else - everything is either c("dendrogram", "dendlist"))
  # let's make it

  if (!missing(which)) {
    x_final <- x_final[which]
    names(x_final) <- the_names
  }

  class(x_final) <- "dendlist"
  return(x_final)
}



# @title A plot S3 method for dendlist
# @export
# @description
# If the dendlist if of length 1 - then use a normal plotting method.
# If it is of length 2 or more, use a tangelgram.
# @param x a dendlist
# @param which an integer vector of length 2, indicating
# which of the trees in the dendlist object should be plotted
# @param ... parameters to be passed to the plot/tanglegram function
# @return
# A dendlist object
# @examples
#
# \dontrun{
#
# dend <- iris[1:20, -5] %>% dist %>% hclust %>% as.dendrogram
# dend2 <- iris[1:20, -5] %>% dist %>% hclust(method = "single") %>% as.dendrogram
#
# x <- dendlist(dend, dend2)
# plot(x)
#
# }
#


#' @export
#' @rdname dendlist
plot.dendlist <- function(x, which = c(1L, 2L), ...) {
  if (!is.dendlist(x)) stop("x is not a dendlist")

  x_length <- length(x)
  if (x_length == 1) {
    plot(x[[1]], ...)
    return(x)
  }
  if (x_length >= 2) tanglegram(x[[which[1]]], x[[which[2]]], ...)
}



#' @title Try to coerce something into a dendlist
#' @export
#' @description
#' It removes stuff that are not dendgrogram/dendlist
#' and turns what is left into a dendlist
#' @param x a list with several dendrogram/hclust/phylo or dendlist objects
#' and other junk that should be omitted.
#' @param ... NOT USED
#' @return
#' A list of class dendlist where each item
#' is a dendrogram
#' @examples
#'
#' \dontrun{
#'
#' dend <- iris[, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend2 <- iris[, -5] %>%
#'   dist() %>%
#'   hclust(method = "single") %>%
#'   as.dendrogram()
#'
#' x <- list(dend, 1, dend2)
#' as.dendlist(x)
#' }
#'
as.dendlist <- function(x, ...) {
  #    x <- list(...)
  # If we have objects which are not dend or dendlist - STOP!
  x_classes <- sapply(x, class)
  ss <- x_classes %in% c("dendrogram", "hclust", "phylo", "dendlist")
  # keep only the parts which are relevant
  x <- x[ss]
  x_final <- list()
  for (i in seq_len(length(x))) {
    x_final <- dendlist(x_final, x[[i]])
  }

  return(x_final)
}





#' @export
head.dendlist <- function(x, ...) {
  for (i in seq_len(length(x))) {
    cat("============\n", "dend ", i, "\n", "---------\n")
    head(x[[i]], ...)
  }
}
