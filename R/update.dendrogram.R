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



#' @title Add (/update) features to a dendrogram
#' @export
#' @aliases 
#' add.dendrogram
#' add.dendlist
#'
#' @usage 
#' 
#' add(object, ...)
#' 
#' \method{add}{dendrogram}(object,
#'    what = c("labels",
#'             "labels_colors",
#'             "labels_cex",
#'             "leaves_pch",
#'             "leaves_cex",
#'             "leaves_col",
#'             "hang_leaves",
#'             "branches_k_color",
#'             "branches_col",
#'             "branches_lwd",
#'             "branches_lty",
#'             "clear_branches",
#'             "clear_leaves"
#'    ),
#'    value, ...)
#'    
#' \method{add}{dendlist}(object, ..., which)
#'
#' @description
#' a master function for updating various attributes and 
#' features of dendrogram objects.
#' 
#' @param object a tree (\link{dendrogram}, or \link{dendlist})
#' @param what a character indicating what is the property of
#' the tree that should be added/updated. (see the usage and the example section
#' for the different options)
#' @param value an object with the value to add to the tree.
#' (the type of the value depends on the "what")
#' @param ... passed to the specific function for more options.
#' @param which an integer vector indicating, in the case "object" is
#' a dendlist, on which of the trees should the modification be performed.
#' If missing - the change will be performed on all of objects in the dendlist.
#' 
#' @details
#' This is a wrapper function for many of the main tasks we 
#' might wish to perform on a dendrogram before plotting.
#' 
#' @seealso
#' 
#' \link{labels<-.dendrogram}, \link{labels_colors<-},
#' \link{hang.dendrogram}, \link{color_branches},
#' \link{assign_values_to_leaves_nodePar},
#' \link{assign_values_to_branches_edgePar},
#' \link{remove_branches_edgePar}, \link{remove_leaves_nodePar},
#' \link{dendrogram}
#' 
#' @return 
#' An updated dendrogram (or dendlist), with some change to
#' the parameters of it
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 10 )
#' 
#' # Getting the dend object
#' dend <- iris[ss,-5] %>% dist %>% hclust %>% as.dendrogram
#' dend %>% plot
#' 
#' dend %>% labels
#' dend %>% add("labels", 1:10) %>% labels
#' dend %>% add("labels", 1:10) %>% plot 
#' dend %>% add("labels_color") %>% plot 
#' dend %>% add("labels_col", c(1,2)) %>% plot # Works also with partial matching :)
#' dend %>% add("labels_cex", c(1, 1.2)) %>% plot 
#' dend %>% add("leaves_pch", NA) %>% plot 
#' dend %>% add("leaves_pch", c(1:5)) %>% plot    
#' dend %>% add("leaves_pch", c(19,19, NA)) %>% 
#'    add("leaves_cex", c(1,2)) %>% plot 
#' dend %>% add("leaves_pch", c(19,19, NA)) %>% 
#'    add("leaves_cex", c(1,2)) %>%
#'    add("leaves_col", c(1,1,2,2)) %>% 
#'    plot 
#' dend %>% add("hang") %>% plot 
#' 
#' dend %>% add("branches_k_col") %>% plot 
#' dend %>% add("branches_k_col", c(1,2)) %>% plot 
#' dend %>% add("branches_k_col", c(1,2,3), k=3) %>% plot
#' dend %>% add("branches_k_col", k=3) %>% plot 
#' 
#' dend %>% add("branches_col", c(1,2, 1, 2, NA)) %>% plot
#' dend %>% add("branches_lwd", c(2,1,2)) %>% plot
#' dend %>% add("branches_lty", c(1,2,1)) %>% plot
#' 
#' #    clears all of the things added to the leaves
#' dend %>% 
#'    add("labels_color", c(19,19, NA)) %>% 
#'    add("leaves_pch", c(19,19, NA))  %>%  # plot  
#'    add("clear_leaves") %>% # remove all of what was done until this point
#'    plot
#' # Different order
#' dend %>% 
#'    add("leaves_pch", c(19,19, NA)) %>% 
#'    add("labels_color", c(19,19, NA)) %>% 
#'    add("clear_leaves") %>% plot
#' 
#' 
#' # doing this without chaining (%>%) will NOT be fun:
#' dend %>% 
#'    add("labels", 1:10) %>%
#'    add("labels_color") %>%
#'    add("branches_col", c(1,2, 1, 2, NA)) %>%
#'    add("branches_lwd", c(2,1,2)) %>%
#'    add("branches_lty", c(1,2,1)) %>%
#'    add("hang") %>%
#'    plot 
#' 
#' # A few dendlist examples:
#' dendlist(dend,dend) %>% add("hang") %>% plot
#' dendlist(dend,dend) %>% add("branches_k_col", k=3) %>% plot
#' dendlist(dend,dend) %>% add("labels_col", c(1,2)) %>% plot
#' 
#' dendlist(dend,dend) %>% 
#'    add("hang") %>%
#'    add("labels_col", c(1,2), which = 1) %>% 
#'    add("branches_k_col", k=3, which = 2) %>%
#'    add("labels_cex", 1.2) %>%
#'    plot
#' 
#' }
add <- function (object, ...) {
   UseMethod("add")
}

add.default <- function (object, ...) {
   magrittr::add(e1 = object, ...)
}

#' @export
add.dendrogram <- 
   function(object,
            what = c("labels",
                     "labels_colors",
                     "labels_cex",
                     "leaves_pch",
                     "leaves_cex",
                     "leaves_col",
                     "hang_leaves",
                     "branches_k_color",
                     "branches_col",
                     "branches_lwd",
                     "branches_lty",
                     "clear_branches",
                     "clear_leaves"
            ),
            value, ...){
      what <- match.arg(what)
      object <- switch(what, 
                       #                     labels = dendextend:::`labels<-.dendrogram`(object, value = value)
                       labels = `labels<-.dendrogram`(object, value = value, ...),
                       labels_colors = `labels_colors<-`(object, value = value, ...),
                       #      labels_colors = assign_values_to_leaves_nodePar(object, value, "lab.col", ...),
                       labels_cex = assign_values_to_leaves_nodePar(object, value, "lab.cex", ...),
                       leaves_pch = assign_values_to_leaves_nodePar(object, value, "pch", ...),
                       leaves_cex =assign_values_to_leaves_nodePar(object, value, "cex", ...),
                       leaves_col =assign_values_to_leaves_nodePar(object, value, "col", ...),
                       hang_leaves = hang.dendrogram(dend = object, hang = ifelse(missing(value), .1, value),...),
                       branches_k_color = color_branches(tree = object, col = value,  ...),
                       branches_col = assign_values_to_branches_edgePar(object = object, value = value, edgePar = "col", ...),
                       branches_lwd = assign_values_to_branches_edgePar(object = object, value = value, edgePar = "lwd", ...),
                       branches_lty = assign_values_to_branches_edgePar(object = object, value = value, edgePar = "lty", ...),
                       clear_branches = remove_branches_edgePar(object, ...),
                       clear_leaves = remove_leaves_nodePar(object, ...)
      )
      object
   }



# ' @S3method add dendlist
#' @export
add.dendlist <- function(object, ..., which) {
   
   if(missing(which)) which <- 1:length(object)
   
   for(i in which) {
      object[[i]] <- add(object[[i]],...)      
   }
   object
}


