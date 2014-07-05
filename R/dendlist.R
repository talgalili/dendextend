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


#' @title Creating a dendlist object from several dendrograms
#' @export
#' @description
#' It accepts several dendrograms and or dendlist objects
#' and chain them all together.
#' This function aim to help with the usability of 
#' comparing two or more dendrograms
#' 
#' @param ... several dendrogram/hclust/phylo or dendlist objects
#' If an object is hclust or phylo - it will be converted
#' into a dendrogram.
#' @return 
#' A list of class dendlist where each item
#' is a dendrogram
#' @examples
#' 
#' \dontrun{
#' 
#' dend <- iris[,-5] %>% dist %>% hclust %>% as.dendrogram
#' dend2 <- iris[,-5] %>% dist %>% hclust(method = "single") %>% as.dendrogram
#' dendlist(1:4, 5, a=dend) # Error
#' # dendlist <- function (...) list(...)
#' dendlist(dend)
#' dendlist(dend, dend)
#' dendlist(dend, dend, dendlist(dend))
#' #  notice how the order of 
#' dendlist(dend, dend2)
#' dendlist(dend) %>% dendlist(dend2)
#' dendlist(dend) %>% dendlist(dend2) %>% dendlist(dend)
#' dendlist(dend, dend2) %>% tanglegram
#' tanglegram(tree1 = dendlist(dend, dend2))
#' 
#' }
#' 
dendlist <- function (...) {
   x <- list(...)
#    object <- as.list(substitute(x))[-1L]
   n <- length(x)

   # If we have objects which are not dend or dendlist - STOP!   
   x_classes <- sapply(x, class)

   if(!all(x_classes %in% c("dendrogram", "hclust", "phylo", "dendlist"))) {
      print_x_classes <- paste(x_classes, collapse = ", ")
      stop(
         paste("Some of your object's classes are not of the type dendrogram/hclust/phylo/dendlist. Please review and fix. Their classes are:\n",
               print_x_classes))
   }
   
   # if some objects are hclust/phylo - then turn them into dend:
   x_classes_hclust_phylo <- x_classes %in% c("hclust","phylo")
   if(any(x_classes_hclust_phylo)) {
      for (i in seq_len(n)) {
         if(x_classes_hclust_phylo[i]) {
            x[[i]] <- as.dendrogram(x[[i]])
         }
      }
   }

   # If all objects are dend - just list them, and return
   x_classes_dend <- x_classes == "dendrogram"
   if(all(x_classes_dend)) {
      # skip...
      x_final <- x
      
   } else {
      n_final <- sum(x_classes == "dendrogram") +
         x[x_classes == "dendlist"] %>% sapply(length) %>% sum
      x_final <- vector("list", n_final)
      
      i_counter <- 1   
      for (i in seq_len(n)) {
         xi <- x[[i]]
         
         if(class(xi) == "dendrogram") {
            x_final[[i_counter]] <- xi   
            i_counter <- i_counter + 1
         } else {
            for(j in seq_len(length(xi))) {
               x_final[[i_counter]] <- xi[[j]]
               i_counter <- i_counter + 1
            }            
         }
         
      }
   }
   
   
   # else - everything is either c("dendrogram", "dendlist"))
   # let's make it
   
   class(x_final) <- "dendlist"
   return(x_final)   
}




