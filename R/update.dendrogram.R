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



#' @title Update a dendrogram
#' @export
#' @aliases 
#' update.dendlist
#'
#' @usage 
#' \method{update}{dendrogram}(object,
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
#'    with, ...)
#'    
#' \method{update}{dendlist}(object, ..., which)
#'
#' @description
#' a master function for updating various attributes and 
#' features of dendrogram objects.
#' 
#' @param object a tree (\link{dendrogram}, or \link{dendlist})
#' @param what a character indicating what property of
#' the tree should be updated. (see the usage and the example section
#' for the different options)
#' @param with a varying object (it depends on the "what"),
#' with it we will update the tree.
#' @param ... passed to the specific function for more options.
#' @param which an integer vector indicating, in the case "object" is
#' a dendlist, which of the trees should have 
#' their "what" updated "with" something. If missing - the update
#' will be performed on all of objects in the dendlist.
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
#' dend %>% update("labels", 1:10) %>% labels
#' dend %>% update("labels", 1:10) %>% plot 
#' dend %>% update("labels_color") %>% plot 
#' dend %>% update("labels_col", c(1,2)) %>% plot # Works also with partial matching :)
#' dend %>% update("labels_cex", c(1, 1.2)) %>% plot 
#' dend %>% update("leaves_pch", NA) %>% plot 
#' dend %>% update("leaves_pch", c(1:5)) %>% plot    
#' dend %>% update("leaves_pch", c(19,19, NA)) %>% 
#'    update("leaves_cex", c(1,2)) %>% plot 
#' dend %>% update("leaves_pch", c(19,19, NA)) %>% 
#'    update("leaves_cex", c(1,2)) %>%
#'    update("leaves_col", c(1,1,2,2)) %>% 
#'    plot 
#' dend %>% update("hang") %>% plot 
#' 
#' dend %>% update("branches_k_col") %>% plot 
#' dend %>% update("branches_k_col", c(1,2)) %>% plot 
#' dend %>% update("branches_k_col", c(1,2,3), k=3) %>% plot
#' dend %>% update("branches_k_col", k=3) %>% plot 
#' 
#' dend %>% update("branches_col", c(1,2, 1, 2, NA)) %>% plot
#' dend %>% update("branches_lwd", c(2,1,2)) %>% plot
#' dend %>% update("branches_lty", c(1,2,1)) %>% plot
#' 
#' #    clears all of the things added to the leaves
#' dend %>% 
#'    update("labels_color", c(19,19, NA)) %>% 
#'    update("leaves_pch", c(19,19, NA))  %>%  # plot  
#'    update("clear_leaves") %>% # remove all of what was done until this point
#'    plot
#' # Different order
#' dend %>% 
#'    update("leaves_pch", c(19,19, NA)) %>% 
#'    update("labels_color", c(19,19, NA)) %>% 
#'    update("clear_leaves") %>% plot
#' 
#' 
#' # doing this without chaining (%>%) will NOT be fun:
#' dend %>% 
#'    update("labels", 1:10) %>%
#'    update("labels_color") %>%
#'    update("branches_col", c(1,2, 1, 2, NA)) %>%
#'    update("branches_lwd", c(2,1,2)) %>%
#'    update("branches_lty", c(1,2,1)) %>%
#'    update("hang") %>%
#'    plot 
#' 
#' # A few dendlist examples:
#' dendlist(dend,dend) %>% update("hang") %>% plot
#' dendlist(dend,dend) %>% update("branches_k_col", k=3) %>% plot
#' dendlist(dend,dend) %>% update("labels_col", c(1,2)) %>% plot
#' 
#' dendlist(dend,dend) %>% 
#'    update("hang") %>%
#'    update("labels_col", c(1,2), which = 1) %>% 
#'    update("branches_k_col", k=3, which = 2) %>%
#'    update("labels_cex", 1.2) %>%
#'    plot
#' 
#' }
update.dendrogram <- 
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
            with, ...){
      what <- match.arg(what)
      object <- switch(what, 
                       #                     labels = dendextend:::`labels<-.dendrogram`(object, value = with)
                       labels = `labels<-.dendrogram`(object, value = with, ...),
                       labels_colors = `labels_colors<-`(object, value = with, ...),
                       #      labels_colors = assign_values_to_leaves_nodePar(object, with, "lab.col", ...),
                       labels_cex = assign_values_to_leaves_nodePar(object, with, "lab.cex", ...),
                       leaves_pch = assign_values_to_leaves_nodePar(object, with, "pch", ...),
                       leaves_cex =assign_values_to_leaves_nodePar(object, with, "cex", ...),
                       leaves_col =assign_values_to_leaves_nodePar(object, with, "col", ...),
                       hang_leaves = hang.dendrogram(dend = object, hang = ifelse(missing(with), .1, with),...),
                       branches_k_color = color_branches(tree = object, col = with,  ...),
                       branches_col = assign_values_to_branches_edgePar(object = object, value = with, edgePar = "col", ...),
                       branches_lwd = assign_values_to_branches_edgePar(object = object, value = with, edgePar = "lwd", ...),
                       branches_lty = assign_values_to_branches_edgePar(object = object, value = with, edgePar = "lty", ...),
                       clear_branches = remove_branches_edgePar(object, ...),
                       clear_leaves = remove_leaves_nodePar(object, ...)
      )
      object
   }



# ' @S3method update dendlist
#' @export
update.dendlist <- function(object, ..., which) {
   
   if(missing(which)) which <- 1:length(object)
   
   for(i in which) {
      object[[i]] <- update(object[[i]],...)      
   }
   object
}


