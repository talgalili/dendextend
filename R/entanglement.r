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











#' @title Adjust the order of one dendrogram based on another (using labels)
#' @export
#' @description 
#' Takes one dendrogram and adjusts its order leaves valeus based on the order
#' of another dendrogram. The values are matached based on the labels of the
#' two dendrograms.
#' 
#' This allows for faster \link{entanglement} running time, since we can be 
#' sure that the leaves order is just as using their labels.
#' 
#' @param dend_change tree object (dendrogram)
#' @param dend_template tree object (dendrogram)
#' @param check_that_labels_match logical (TRUE). If to check that the labels
#' in the two dendrogram match. (if they do not, the function aborts)
#' @return Returns dend_change after adjusting its order values to
#'  be like dend_template.
#' @seealso \link{untangle}, \link{entanglement} , \link{tangelgram}
#' @examples
#' \dontrun{
#' 
#' dend <- as.dendrogram(hclust(dist(USArrests[1:4,])))
#' order.dendrogram(dend) #  c(4L, 3L, 1L, 2L)
#' 
#' dend_changed <- dend
#' order.dendrogram(dend_changed) <- 1:4
#' order.dendrogram(dend_changed) # c(1:4)
#' 
#' # now let's fix the order of the new object to be as it was:
#' dend_changed <- match_order_by_labels(dend_changed, dend)
#' # these two are now the same:
#' order.dendrogram(dend_changed)
#' order.dendrogram(dend))
#' 
#' }
match_order_by_labels <- function(dend_change, dend_template , check_that_labels_match = TRUE) {   
   
   #let's put the leaves' numbers and labels in two data.frames 
   #    tree_to_change_labels_order <- data.frame(labels = labels(dend_change), values = order.dendrogram(dend_change))
   # 	tree_template_labels_order <- data.frame(labels = labels(dend_template), values = order.dendrogram(dend_template))
   tree_to_change_labels = labels(dend_change)
   tree_template_labels = labels(dend_template)
   
   if(check_that_labels_match) { 
      if(!identical(sort(tree_to_change_labels) , sort(tree_template_labels))) { 
         stop("labels do not match in both trees.  Please make sure to fix the labels names!") 
      }
   }	
   
   tree_template_order <- order.dendrogram(dend_template)
   
   # this gives us how to order y so it would be in the order of x.
   # y_to_order_like_x <- c(2,3,1,4)
   # y_to_order_like_x[match(c(1:4), y_to_order_like_x)]
   # I want to order the numbers in yoav_tree so that they would match the needed order in dans_tree
   
   ss_order_change_leaf_numbers_to_match_template <- match(x= tree_to_change_labels, table= tree_template_labels)
   tree_new_leaf_numbers <- tree_template_order[ss_order_change_leaf_numbers_to_match_template]
   order.dendrogram(dend_change) <- tree_new_leaf_numbers
   
   return(dend_change)
}






#' @title Adjust the order of one dendrogram based on another (using order)
#' @export
#' @description 
#' Takes one dendrogram and adjusts its order leaves valeus based on the order
#' of another dendrogram. The values are matached based on the order of the
#' two dendrograms.
#' 
#' This allows for faster \link{entanglement} running time, since we can be 
#' sure that the leaves order is just as using their labels.
#' 
#' This is a function is FASTER than \link{match_order_by_labels}, but it 
#' assumes that the order and the labels of the two trees are matching!!
#' 
#' This will allow for a faster calculation of \link{entanglement}.
#' 
#' @param dend_change tree object (dendrogram)
#' @param dend_template tree object (dendrogram)
#' @param dend_change_old_order a numeric vector with the order of leaves in
#' dend_change (at least before it was changes for some reason).
#' This is the vector based on which we adjust the new values of dend_change.
#' @param check_that_labels_match logical (FALSE). If to check that the labels
#' in the two dendrogram match. (if they do not, the function aborts)
#' @param check_that_leaves_order_match logical (FALSE). If to check that 
#' the order in the two dendrogram match. (if they do not, the function aborts)
#' 
#' @return Returns dend_change after adjusting its order values to
#'  be like dend_template.
#' @seealso \link{untangle}, \link{entanglement} , \link{tangelgram},
#' \link{match_order_by_labels}
#' @examples
#' \dontrun{
#' 
#' dend <- as.dendrogram(hclust(dist(USArrests[1:4,])))
#' order.dendrogram(dend) #  c(4L, 3L, 1L, 2L)
#' 
#' 
#' # Watch this!
#' dend_changed <- dend
#' dend_changed <- rev(dend_changed)
#' expect_false(identical(order.dendrogram(dend_changed), order.dendrogram(dend)))
#' # we keep the order of dend_change, so that the leaves order are synced
#' # with their labels JUST LIKE dend:
#' old_dend_changed_order <- order.dendrogram(dend_changed)   
#' # now we change dend_changed leaves order values:
#' order.dendrogram(dend_changed) <- 1:4
#' # and we can fix them again, based on their old kept leaves order:
#' dend_changed <- match_order_dendrogram_by_old_order(dend_changed, dend, 
#'                                                      old_dend_changed_order)
#' expect_identical(order.dendrogram(dend_changed), order.dendrogram(dend))
#' 
#' 
#' }
match_order_dendrogram_by_old_order <- function(dend_change, dend_template , 
                                                 dend_change_old_order,
                                                 check_that_labels_match = FALSE, 
                                                 check_that_leaves_order_match =FALSE
                                                 ) {	
   
   # this function was made to help make entanglement.dendrogram faster.
   # But it relies on some important assumptions (otherwise, its results will be nonsense!)
   
   if(check_that_labels_match) { # I am turning this check to FALSE since it takes 0.03 sec from the function (which is a long time when running this function a lot)
      if(any(sort(labels(dend_change)) != sort(labels(dend_template)))) stop("labels do not match in both trees.  Please make sure to fix the labels names!")
   }
   if(check_that_leaves_order_match) { # I am turning this check to FALSE since it takes 0.03 sec from the function (which is a long time when running this function a lot)
      if(any(sort(order.dendrogram(dend_change)) != sort(order.dendrogram(dend_template)))) stop("order.dendrogram do not match in both trees.  Please make sure to fix the labels names!")
   }
   
#    if(print_NOTE) cat("NOTE:
#                       Make sure that the values in dend_change_old_order match the labels of dend1 in the same way
#                       as the values and labels of the dend_change!
#                       ")
   
   
   #let's put the leaves' numbers and labels in two data.frames
   # 	tree_to_change_labels_order <- data.frame(labels = labels(dend_change), values = order.dendrogram(dend_change))
   # 	tree_template_labels_order <- data.frame(labels = labels(dend_template), values = order.dendrogram(dend_template))
   tree_to_change_order = order.dendrogram(dend_change)
   tree_template_order = order.dendrogram(dend_template) # these should be values after some change was done outside the function (and dend_change_old_order are the values before the change)
   
   # this gives us how to order y so it would be in the order of x.
   # y_to_order_like_x <- c(2,3,1,4)
   # y_to_order_like_x[match(c(1:4), y_to_order_like_x)]
   # I want to order the numbers in yoav_tree so that they would match the needed order in dans_tree
   
   ss_order_change_leaf_numbers_to_match_template <- match(x= tree_to_change_order, table= dend_change_old_order)
   
   new_leaves_order <- tree_template_order[ss_order_change_leaf_numbers_to_match_template]
   order.dendrogram(dend_change) <- new_leaves_order
   
   return(dend_change)
}













# untangle.dendrogram # A function to take two dendrograms and rotate them so to minimize some penalty on entanglement 

# entanglement

entanglement <- function (...) { UseMethod("entanglement") }


entanglement.default <- function (object, ...) { stop("no default function for entanglement") }


entanglement.dendrogram <- function(dend1,dend2, L = 1.5, leaves_matching_method = c("order", "labels")) {
   # One day, one might think of other measures of entanglement.  But for now, we have only one method ("cor.spearman").  Which is the 1-absolute value of the tanks of the values in the two dendrograms.
   # A level close to 1 is bad (very entangled).  A number close to 0 is good (low entanglement)
   # leaves(dend1),leaves(dend2)
   # L tells us which panelty level we are at (L0, L1, L2, partial L's etc).  L>1 means that we give a big panelty for sharp angles.  While L->0 means that any time something is not a streight horizontal line, it gets a large penalty
   # If L=0.1 it means that we much prefer streight lines over non streight lines
   
   if(L==0) L <- L + 1e-50 # this is in order to make sure L is not ==0.  Because that would just create nonsical meaning.
   
   
   n_leaves <- nleaves(dend1) # how many leaves do we have? (number of leaves)
   one_to_n_leaves <- seq_len(n_leaves)
   
   if(leaves_matching_method[1] == "order") {   
      dend1_old_order <- order.dendrogram(dend1)
      order.dendrogram(dend1) <- one_to_n_leaves # change the leaves of dend1 to be 1:n	
      dend2 <- match_order_dendrogram_by_old_order(dend2	, dend1, dend1_old_order) # make sure that the numbers if the 
   } else { # "labels" - this method is "safer" (since we can easily see if the labels on the two trees match or not
      # however, this is twice as slow (which adds up quite a bit with the functions that rely on this)
      # Hence, it is best to make sure that the trees used here have the same labels and the SAME values matched to these values
      order.dendrogram(dend1) <- one_to_n_leaves # change the leaves of dend1 to be 1:n
      dend2 <- match_order_by_labels(dend2	, dend1) # This one is "slow"
   }
   
   sum_abs_diff_L <- function(x,y,L) sum(abs(x-y)**L)
   
   entanglement_result <- sum_abs_diff_L(order.dendrogram(dend1), order.dendrogram(dend2), L)	
   worse_entanglement_result <- sum_abs_diff_L(one_to_n_leaves, rev(one_to_n_leaves), L)		
   normalized_entanglement_result <- entanglement_result/worse_entanglement_result # should range between 0 (no etnaglement) and 1 (max entangelment
   
   normalized_entanglement_result
}










# ### OLD entanglement concept.
# entanglement.dendrogram <- function(dend1,dend2 , method = c("absolute.rank.sum", "cor.spearman") )
# {
# 	# One day, one might think of other measures of entanglement.  But for now, we have only one method ("cor.spearman").  Which is the 1-absolute value of the tanks of the values in the two dendrograms.
# 	# A level close to 1 is bad (very entangled).  A number close to 0 is good (low entanglement)
# 	# leaves(dend1),leaves(dend2)
# 	
# 	n_leaves <- nleaves(dend1) # how many leaves do we have? (number of leaves)
# 	order.dendrogram(dend1) <- seq_len(n_leaves) # change the leaves of dend1 to be 1:n
# 	dend2 <- match_order_by_labels(dend2	, dend1) # make sure that the numbers if the 
# 
# 	if(method[1] == "cor.spearman") {
# 		order_cor <- cor(order.dendrogram(dend1),order.dendrogram(dend2), method = "spearman")
# 		entanglement_result <- (1-order_cor)/2 # cor=1 is best (0 entanglement), cor = 0 is bad (0.5 entanglement), cor = -1 is worst (1 entanglament)		
# 	}
# 	if(method[1] == "absolute.rank.sum") {					
# 		entanglement_result <- sum(abs(order.dendrogram(dend1)-order.dendrogram(dend2)))
# 	}
# 	
# 	entanglement_result
# }



# tanglegram(dend1,dend2)
# tanglegram(sort(dend1),sort(dend2))
# entanglement(dend1,dend2, L = 0)
# entanglement(dend1,dend2, L = 0.25)
# entanglement(dend1,dend2, L = 1)
# entanglement(dend1,dend2, L = 2)
# entanglement(dend1,dend2, L = 10)
# entanglement(sort(dend1),sort(dend2), L=0)
# entanglement(sort(dend1),sort(dend2), L=0.25)
# entanglement(sort(dend1),sort(dend2), L=1)
# entanglement(sort(dend1),sort(dend2), L=2)
# entanglement(sort(dend1),sort(dend2), L=10)

# OLD and SLOW
entanglement.dendrogram <- function(dend1,dend2, L = 1.5)
{
   # One day, one might think of other measures of entanglement.  But for now, we have only one method ("cor.spearman").  Which is the 1-absolute value of the tanks of the values in the two dendrograms.
   # A level close to 1 is bad (very entangled).  A number close to 0 is good (low entanglement)
   # leaves(dend1),leaves(dend2)
   # L tells us which panelty level we are at (L0, L1, L2, partial L's etc).  L>1 means that we give a big panelty for sharp angles.  While L->0 means that any time something is not a streight horizontal line, it gets a large penalty
   # If L=0.1 it means that we much prefer streight lines over non streight lines
   
   if(L==0) L <- L + 1e-50 # this is in order to make sure L is not ==0.  Because that would just create nonsical meaning.
   
   
   n_leaves <- nleaves(dend1) # how many leaves do we have? (number of leaves)
   one_to_n_leaves <- seq_len(n_leaves)
   order.dendrogram(dend1) <- one_to_n_leaves # change the leaves of dend1 to be 1:n
   dend2 <- match_order_by_labels(dend2	, dend1) # make sure that the numbers if the 
   
   sum_abs_diff_L <- function(x,y,L) sum(abs(x-y)**L)
   
   entanglement_result <- sum_abs_diff_L(order.dendrogram(dend1), order.dendrogram(dend2), L)	
   worse_entanglement_result <- sum_abs_diff_L(one_to_n_leaves, rev(one_to_n_leaves), L)		
   normalized_entanglement_result <- entanglement_result/worse_entanglement_result # should range between 0 (no etnaglement) and 1 (max entangelment
   
   normalized_entanglement_result
}






