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







# create_dendextend_options <- function() {
#    # assigns the functions which could later be replaced by the FASTER dendextendRcpp functions
#
#
# }
# # create_dendextend_options()
# # dendextend_options
# # dendextend_options()





dendextend_options_env <- new.env()





#' @title Access to dendextend_options
#' @export
#' @description
#' This is a function inside its own environment. This enables a bunch of
#' functions to be manipulated outside the package, even when they are called
#' from function within the dendextend package.
#'
#'
#'
#' TODO: describe options.
#'
#' A new "warn" dendextend_options parameter. logical (FALSE). Should warning be issued?
#'
#' @author Kurt Hornik
#' @param option a character scalar of the value of the options we would
#' like to access or update.
#' @param value any value that we would like to update into the "option"
#' element in dendextend_options
#' @return a list with functions
#' @examples
#'
#' dendextend_options("a")
#' dendextend_options("a", 1)
#' dendextend_options("a")
#' dendextend_options("a", NULL)
#' dendextend_options("a")
#' dendextend_options()
dendextend_options <- local({
  options <- list()
  function(option, value) {
    #          ellipsis <- list(...)
    if (missing(option)) {
      return(options)
    }

    if (missing(value)) {
      options[[option]]
    } else {
      options[[option]] <<- value
    }
  }
}, envir = dendextend_options_env)
# a=2
# dendextend_options()
# dendextend_options(a=3)  # sadly, this will fail, and ellipsis does not help
# dendextend_options(a<-3)
# dendextend_options("a",3)
# dendextend_options("a")
# dendextend_options()




#' @title Populates dendextend functions into dendextend_options
#' @description
#' Populates dendextend functions into dendextend_options
#' @export
#' @return NULL
assign_dendextend_options <- function() {
  # assigns the functions which could later be replaced by the FASTER dendextendRcpp functions

  # dendextend_options("get_branches_heights" , dendextend::dendextend_get_branches_heights)
  # dendextend_options("heights_per_k.dendrogram" , dendextend::dendextend_heights_per_k.dendrogram)
  # dendextend_options("cut_lower_fun" , dendextend::dendextend_cut_lower_fun)
  # dendextend_options("labels.dendrogram" , dendextend::dendextend_labels.dendrogram)
  dendextend_options("warn", FALSE)
}

# dendextend_options("cut_lower_fun")
#










remove_dendextend_options <- function() {
  # assigns the functions which could later be replaced by the FASTER dendextendRcpp functions
  #    options(dendextend_get_branches_heights = NULL)
  #    options(dendextend_heights_per_k.dendrogram = NULL)
  #    options(dendextend_cut_lower_fun = NULL)
  #    rm(dendextend_options)

  # thid doesn't quite remove it - it just empties it.
  #    x <- names(dendextend::dendextend_options())
  #    for(i in x) dendextend::dendextend_options(i, NULL)

  # assign("options", list(), envir = dendextend_options_env)
}



#
#
# ## Too risky, leave as is...
#
# search()
# library(magrittr)
# dend <- USArrests %>% dist %>% hclust(method = "ave") %>% as.dendrogram
# dendextend::color_branches(dend, k = 2)
# search()
#
# dendextend::assign_dendextend_options()
# detach("package:dendextend")
# dendextend:::dendextend_options()
# dendextend:::remove_dendextend_options()
# ls(envir = dendextend:::dendextend_options_env)
# rm(list = ls(envir = dendextend:::dendextend_options_env))
# assign("options", list(), envir = dendextend:::dendextend_options_env)
# get("options", envir = dendextend:::dendextend_options_env)
# rm("options", envir = dendextend:::dendextend_options_env)
# dendextend::assign_dendextend_options()
# dendextend:::dendextend_options()
# ls(envir = dendextend:::dendextend_options_env)
# assign("options", NULL, envir = dendextend:::dendextend_options_env)
#
