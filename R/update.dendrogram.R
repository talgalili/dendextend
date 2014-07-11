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
#' \method{update}{dendrogram}(object, which = c(1L,2L), method = c("pearson", "kendall", "spearman"), ...) 
#' 
#' \method{update}{dendlist}(object, which = c(1L,2L), method = c("pearson", "kendall", "spearman"), ...) 
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
#' @param ... Ignored.
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
#' 
update.dendrogram <- function(object, 
                              what = c("labels"),
                              with, ...){
   what <- match.arg(what)
   object <- switch(what, 
#                     labels = dendextend:::`labels<-.dendrogram`(object, value = with)
                    labels = `labels<-.dendrogram`(object, value = with)
      )
   
   object
}



if(FALSE) {
   set.seed(23235)
   ss <- sample(1:150, 10 )
   dend <- iris[ss,-5] %>% dist %>% hclust %>% as.dendrogram
   labels(dend)
   update(dend, "labels", 1:10)
   dend %>% update("labels", 1:10) %>% plot # Works :)
}


