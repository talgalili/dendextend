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



#' @title Update dendrogram features
#' @export
#' @aliases 
#' update.dendrogram
#' update.dendlist
#' @description
#' a master function for updating various attributes and 
#' features of dendrogram objects.
#' 
#' @usage
#' 
#' \method{update}{dendrogram}(object, ...) 
#' 
#' \method{update}{dendlist}(object, which, ...) 
#' 
#' @param object a tree (dendrogram, or \link{dendlist})
#' @param what a character indicating what property of
#' the tree should be updated.
#' @param with a varying object (it depends on the "what"),
#' with it we will update the tree.
#' @param which an integer indicating, in the case "object" is
#' a dendlist, which of the trees should have 
#' their "what" updated "with" something. If missing - the update
#' will be performed on all of objects in the dendlist.
#' @param ... passed to the specific function for more options.
#' 
#' @details
#' ddd
#' 
#' @seealso
#' \link{cophenetic}, \link{cor_bakers_gamma}
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
#' hc1 <- iris[ss,-5] %>% dist %>% hclust("com")
#' hc2 <- iris[ss,-5] %>% dist %>% hclust("single")
#' dend1 <- as.dendrogram(hc1)
#' dend2 <- as.dendrogram(hc2)
#' #    cutree(dend1)   
#' 
#' cophenetic(hc1)
#' cophenetic(hc2)
#' # notice how the dist matrix for the dendrograms have different orders:
#' cophenetic(dend1)
#' cophenetic(dend2)
#' 
#' cor(cophenetic(hc1), cophenetic(hc2)) # 0.874
#' cor(cophenetic(dend1), cophenetic(dend2))  # 0.16
#' # the difference is becasue the order of the distance table in the case of
#' # stats:::cophenetic.dendrogram will change between dendrograms!
#' 
#' # however, this is consistant (since I force-sort the rows/columns): 
#' cor_cophenetic(hc1, hc2)
#' cor_cophenetic(dend1, dend2)
#' 
#' cor_cophenetic(dendlist(dend1, dend2))
#' 
#' # we can also use different cor methods (almost the same result though): 
#' cor_cophenetic(hc1, hc2, method = "spearman") # 0.8456014
#' cor_cophenetic(dend1, dend2, method = "spearman") # 
#' 
#' 
#' # cophenetic correlation is about 10 times (!) faster than bakers_gamma cor:
#' require(microbenchmark)
#' microbenchmark(
#'    cor_bakers_gamma = cor_bakers_gamma(dend1, dend2, try_cutree_hclust=FALSE),
#'    cor_cophenetic = cor_cophenetic(dend1, dend2)   ,
#'    times=10
#' )
#' 
#' # but only because of the cutree for dendrogram. When allowing hclust cutree
#' # it is only about twice as fast:
#' microbenchmark(
#'    cor_bakers_gamma = cor_bakers_gamma(dend1, dend2, try_cutree_hclust=TRUE),
#'    cor_cophenetic = cor_cophenetic(dend1, dend2)   ,
#'    times=10
#' )
#' 
#' }


update.dendrogram <- function(object, 
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



if(FALSE) {

   set.seed(23235)
   ss <- sample(1:150, 10 )
   dend <- iris[ss,-5] %>% dist %>% hclust %>% as.dendrogram
   
   dend %>% labels
   dend %>% update("labels", 1:10) %>% labels
   dend %>% update("labels", 1:10) %>% plot # Works :)
   dend %>% update("labels_color") %>% plot # Works :)
   dend %>% update("labels_col", c(1,2)) %>% plot # Works :)
   dend %>% update("labels_cex", c(1, 1.2)) %>% plot # Works :)
   dend %>% update("branches_k_col") %>% plot # Works :)
   dend %>% update("branches_k_col", c(1,2)) %>% plot # Works :)
   dend %>% update("branches_k_col", c(1,2,3), k=3) %>% plot # Works :)
   dend %>% update("branches_k_col", k=3) %>% plot # Works :)
   dend %>% update("hang") %>% plot # Works :)
   
   
   
   dend %>% update("leaves_pch", NA) %>% plot # Works :)
   dend %>% update("leaves_pch", c(1:5)) %>% plot # Works :)   
   dend %>% update("leaves_pch", c(19)) %>% update("leaves_cex", c(1,2)) %>% plot # Works :)
   dend %>% update("leaves_pch", c(19,19, NA)) %>% 
      update("leaves_cex", c(1,2)) %>%
      update("leaves_col", c(1,1,2,2)) %>% 
      plot # Works :)
   
   #    clears all of the things added to the leaves
   dend %>% 
      update("labels_color", c(19,19, NA)) %>% 
      update("leaves_pch", c(19,19, NA))  %>%  # plot  
      update("clear_leaves") %>% # remove all of what was done until this point
      plot
   
   dend %>% 
      update("leaves_pch", c(19,19, NA)) %>%    
      update("labels_color", c(2,19, NA)) %>% 
      plot
   
   dend %>% 
      update("leaves_pch", c(19,19, NA)) %>% 
      update("labels_color", c(19,19, NA)) %>% 
      update("clear_leaves") %>% plot
   
   
   dend %>% 
      update("labels", 1:10) %>%
      update("labels_color") %>%
      update("branches_col", c(1,2, 1, 2, NA)) %>%
      update("branches_lwd", c(2,1,2)) %>%
      update("branches_lty", c(1,2,1)) %>%
      update("hang") %>%
      plot # Works :)
   
}


