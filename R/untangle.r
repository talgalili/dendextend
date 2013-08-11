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




# get("sort")
#' 'shuffle' is a function that randomilly rotates ("shuffles") a tree.
#' a dendrogram leaves order (by means of rotation)


#' @title Random rotation of trees
#' @export
#' @aliases 
#' shuffle.default
#' @description 
#' 'shuffle' randomilly rotates ("shuffles") a tree, changing its presentation 
#' while preserving its topolgoy.
#' 'shuffle' is based on \link[dendextend]{rotate} and through its methods can
#' work for any of the major tree objects in R (\link{dendrogram}/\link{hclust}/\link[ape]{phylo}).
#' 
#' This function is useful in combination with \link{tanglegram} and \link{entanglement}.
#' 
#' @usage
#' shuffle(object, ...)
#' 
#' @param object a tree object (\link{dendrogram}/\link{hclust}/\link[ape]{phylo})
#' @param ... Ignored.
#'  
#' @return A randomlly rotated tree object
#' @seealso \code{\link{tanglegram}},  \code{\link{entanglement}}, 
#' \code{\link[dendextend]{rotate}}
#' @examples
#' dend <- as.dendrogram(hclust(dist(USArrests)))
#' set.seed(234238)
#' dend2 <- shuffle(dend)
#' 
#' tanglegram(dend, dend2, margin_inner=7)
#' entanglement(dend, dend2) # 0.3983
#' 
#' # although these ARE the SAME tree:
#' tanglegram(sort(dend), sort(dend2), margin_inner=7)
#' 
#' 
shuffle <- function (object, ...) {UseMethod("shuffle")}

shuffle.default <- function(object, ...) {
   # takes a dendrogram object and shuffles its branches in a random fashion
   # 	n_leaves <- length(labels(object))	# leaves.value is faster then labels!
   n_leaves <- nleaves(object)
   random_weights <- sample(seq_len(n_leaves)) # a random ordaring of 1:n_leaves weights
   rotate(object, random_weights) # since we have a method here for dend/hclust/phylo - this makes this function rather generic...
}






#' @title Untangle - random search
#' @export
#' @description 
#' Searches for two untangled dendrogram by randomlly shuflling them and each 
#' time checking if their entanglement was improved.
#' 
#' @param tree1 a tree object (of class dendrogram/hclust/phylo).
#' @param tree2 a tree object (of class dendrogram/hclust/phylo).
#' @param R numeric (default is 100). The number of shuffles to perform.
#' @param L the distance norm to use for measuring the distance between the
#' two trees. It can be any positive number, often one will want to
#'  use 0, 1, 1.5, 2 (see 'details' for more). 
#'  It is passed to \link{entanglement}.
#' @param leaves_matching_method a character scalar passed to \link{entanglement}.
#' It can be either "order" (default) or "labels". If using "labels", 
#' then we use the labels for matching the leaves order value.
#' And if "order" then we use the old leaves order value for matching the 
#' leaves order value.
#' 
#' Using "order" is faster, but "labels" is safer. "order" will assume that 
#' the original two trees had their labels and order values MATCHED.
#' 
#' Hence, it is best to make sure that the trees used here have the same labels 
#' and the SAME values matched to these values - and then use "order" (for
#' fastest results).
#' 
#' If "order" is used, the function first calls \link{match_order_by_labels}
#' in order to make sure that the two trees have their labels synced with 
#' their leaves order values.
#' 
#' @param ... not used
#' 
#' @details 
#' 
#' Untangaling two trees is a hard combinatorical problem without a closed
#' form solution. One way for doing it is to run through a random spectrom
#' of options and look for the "best" two trees. This is what this function
#' offers.
#' 
#' @return A list with two trees with the best entanglement that was found.
#' @seealso \link{untangle}, \link{tanglegram}, \link{match_order_by_labels},
#' \link{entanglement}.
#' @examples
#' 
#' \dontrun{
#' hc1 <- hclust(dist(iris[,-5]), "com")
#' hc2 <- hclust(dist(iris[,-5]), "single")
#' dend1 <- as.dendrogram(hc1)
#' dend2 <- as.dendrogram(hc2)
#' tanglegram(dend1,dend2)
#' 
#' set.seed(65168)
#' dend12 <- untangle_random_search(dend1, dend2)
#' tanglegram(dend12[[1]],dend12[[2]])
#' 
#' entanglement(dend1,dend2, L = 2) # 0.8894
#' entanglement(dend12[[1]],dend12[[2]], L = 2) # 0.0998
#' 
#' 
#' 
#' }
untangle_random_search <- function(tree1, tree2, R = 100L, L = 1, leaves_matching_method = c("order", "labels"), ...) {
   # this is a simple random search algorithm for the optimal tanglegram layout problem.
   # it shufflers the trees, and see if we got a better entanglement or not
   
   
   if(leaves_matching_method[1] == "order") {
      old_tree2 <- tree2
      tree2 <- match_order_by_labels(old_tree2, tree1)   
      if(!identical(tree2,old_tree2)) warning("The leaves order in 'tree2' were changed. If you want to avoid that, use leaves_matching_method = 'labels'.")
   }
   
   
   best_ordaring_entanglement <- entanglement(tree1, tree2, L, leaves_matching_method)
   
   for(i in 1:R) {
      s_tree1 <- shuffle(tree1)
      s_tree2 <- shuffle(tree2)
      current_entanglement <- entanglement(s_tree1, s_tree2, L,leaves_matching_method)
      
      # if we came across a better ordaring, then update the "Best" treerograms 
      if(current_entanglement < best_ordaring_entanglement) {
         best_ordaring_entanglement<- current_entanglement
         optimal_tree1 <- s_tree1
         optimal_tree2 <- s_tree2			
      }
   }
   
   return(list(tree1 = optimal_tree1, tree2 = optimal_tree2))
}




flip.strings <- function(STRING, str1, str2) {
   # gets a string which includes str1 and str2, and makes sure to flip them in the string
   STRING <- sub(str1, "_____1_", STRING,fixed=T) # substitutes the first string with a place holder (1)
   STRING <- sub(str2, "_____2_", STRING,fixed=T) # substitutes the second string with a place holder (2)
   STRING <- sub("_____1_",str2, STRING,fixed=T)  # substitutes the place holder (1) with the second string
   STRING <- sub("_____2_",str1, STRING,fixed=T)	# substitutes the place holder (2) with the first string
   return(STRING)	
}
# flip.strings("abcdefgh", "ab", "fgh") # "fghcdeab"

add.zzz <- function(x) {
   # this function adds a"_" character to the end of every element of the vector.
   # this is used to make numeric values unique (so to not confuse 1 and 10 or 17 and 7 !)
   x <- as.character(x)
   x <- paste("zzz", x, "zzz", sep ="")
   x
}
remove.zzz <- function(x) {
   gsub("zzz","", x, fixed = T)
}
# remove.zzz(add.zzz(1:6))
collapse.with.pipes <- function(x) paste(x, collapse = "||")
collapse.pipes.zzz <- function(x) paste(add.zzz(x), collapse = "||")
remove.pipes.and.zzz <- function(x) strsplit(remove.zzz(x), "||",fixed=T)[[1]]



flip.leaves <- function(dend, leaves1, leaves2) {
   # flip a node in a tree based on the leaves in each branch in the node:
   # this function gets a dendgram with two vector of leaves that needs to be flipped with one another on the tree
   # we assume here unique values of leaves.
   # also notice that this is based on the values of the leaves and NOT their labels.
   leaves_order <- order.dendrogram(dend)
   weights <- seq_along(leaves_order)
   
   #turn the values of leaves and leaves1/2 to strings with || delim:
   leaves_order_string <- collapse.pipes.zzz(leaves_order)
   leaves1_string <- collapse.pipes.zzz(leaves1)
   leaves2_string <- collapse.pipes.zzz(leaves2)
   # then flips the locations of leaves1 and 2 in the string
   flipped_leaves_order_string <- flip.strings(leaves_order_string, leaves1_string, leaves2_string)
   # and turn the string back to a vector of flipped leaves values:
   flipped_leaves_order <- as.numeric(remove.pipes.and.zzz(flipped_leaves_order_string))
   
   new_order_weights <- match(flipped_leaves_order, leaves_order) # order the leaves_order to be like flipped_leaves_order
   # leaves_order[new_order_weights]
   # now use this order to order the weights!
   new_weights <- weights[new_order_weights]
   
   flipped_dend <- rotate(dend, new_weights) # and lastly - rotate the dend by the leaves to flip.
   
   return(flipped_dend)
}

if(F) { # example
   order.dendrogram(dend)
   plot(dend)
   flip.leaves(dend, c(10), c(1))
   plot(flip.leaves(dend, c(2), c(3,5)))
   plot(flip.leaves(dend, c(9), c(2,3,5)))
   plot(flip.leaves(dend, c(7), c( 1L, 8L, 4L, 6L, 10L, 9L, 2L, 3L, 5L)))
}


# I didn't use this evantually:
# 	require(combinat)
# source for this package: http://stackoverflow.com/questions/7906332/how-to-calculate-combination-and-permutation-in-r	






# NEW and FASTER version!!!
all.couple.rotations.at.k <- function(dendro, k, dendrogram_heights_per_k) {
   # This function gets the dendro tree, and a k number of clusters
   # and returns all of the permutated dendrogram trees, rotating only two of the k clusters at each permutation
   # if this was done for ALL permutation, the algorithm would not be feasable.
   # practically, for a binary tree - this only gives two trees as an output (the original, and the flipped new k'th cluster)
   
   if(k==1) return(dendro) # there are no possible rotations for k==1
   
   if(missing(dendrogram_heights_per_k)) dendrogram_heights_per_k <- dendrogram.heights.per.k(dendro) # since this function takes a looong time, I'm running it here so it will need to run only once!	
   # And I would MUCH rather give this vector upfront - so the entire thing will be faster...			
   
   leaves_order <- order.dendrogram(dendro)
   k_cluster_leaves <- cutree(dendro, k, 
                              order_clusters_using_tree =F,
                              dendrogram_heights_per_k = dendrogram_heights_per_k, # makes it faster
                              use_labels_not_values=F) # makes it 10 times faster (and we don't use the labels of the clusters, only the cluster vector)
   km1_cluster_leaves <- cutree(dendro, k-1, 
                                order_clusters_using_tree =F, 
                                dendrogram_heights_per_k = dendrogram_heights_per_k,
                                use_labels_not_values=F)
   
   # if we can't cut the current stage (for example, because we have more than 2 branches, than return the original tree
   if(is.null(k_cluster_leaves)) return(list(dendro))
   # If we can't cut the tree above us, then loop up until you find a k for which you can cut.
   # there might be bugs for this code, more careful thought should be made in such cases...
   while(is.null(km1_cluster_leaves)) {
      k <- k-1
      km1_cluster_leaves <- cutree(dendro, k-1, 
                                   order_clusters_using_tree =F, 
                                   dendrogram_heights_per_k = dendrogram_heights_per_k,
                                   use_labels_not_values=F) # makes it 10 times faster (and we don't use the labels of the clusters, only the cluster vector)
      warning(paste("couldn't cut tree at k-1, trying it for", k-1))
   }	
   
   # 	if(cutree(Dan_arc_tree, 1)
   # weights <- seq_along(k_cluster_leaves)
   # 	leaf_cluster_id <- cutree(dendro, k, order_clusters_using_tree =F)
   # 	leaf_cluster_id <- factor(leaf_cluster_id) # turns it into a factor so I'd be able to easily permutate the values.
   
   # kkm1_df <-
   # data.frame(km1_cluster_leaves, k_cluster_leaves)
   
   permutated_dendro <- list(dendro)# this one will hold all of the permutations
   permutation_indx <- 1 # this one will tell us at what stage of the permutation we are at	
   
   for(i in unique(km1_cluster_leaves)) {
      ss <- i==km1_cluster_leaves
      unique_clusters_in_branch <- unique(k_cluster_leaves[ss])
      
      if(length(unique_clusters_in_branch) > 1) { # the only way there is a reason to do permutations here is if the current cluster we are looking at has more than 1 member			
         number_of_clusters_in_branch <- length(unique_clusters_in_branch)
         branches_permutations <- as.matrix(combn(unique_clusters_in_branch, 2))# a matrix were each column is a permutation of 2 out of the clusters in this branch (most often just 2, but sometimes more...)
         #		as.matrix(combn(1:3, 2))
         # permn(number_of_clusters_in_branch) # this will be 2 most of the time, but this structure allows one to deal with clusters which have more than 2 branches
         n_permutations <- ncol(branches_permutations)			
         
         for(j in seq_len(n_permutations)) { # would often run just once.
            
            # choosing the leaves belonging to each of the two clusters
            ss_leaves1 <- k_cluster_leaves == branches_permutations[1,j]
            ss_leaves2 <- k_cluster_leaves == branches_permutations[2,j]
            leaves1 <- leaves_order[ss_leaves1]
            leaves2 <- leaves_order[ss_leaves2]
            
            # 				plot(flip.leaves(dendro, leaves1, leaves2))
            # Flipping the branches of the two adjecent clusters:
            permutation_indx <- permutation_indx + 1
            permutated_dendro[[permutation_indx]] <- flip.leaves(dendro, leaves1, leaves2) # this will not work for hclust (will for dendro)
         }			
      }
   }	
   return(permutated_dendro)
}


# dend=best_dend
# k = 13
# all.couple.rotations.at.k(best_dend, k)
# system.time(all.couple.rotations.at.k(dend1, k = 4))




if(F) { # example
   # 	tmp <- all.couple.rotations.at.k(dend, 10)
   # 	flip.leaves(dend, leaves1, leaves2)
   # 	order.dendrogram(dend)
   
   
   tmp <- all.couple.rotations.at.k(dend, 2)
   length(tmp)
   plot(tmp[[1]])
   plot(tmp[[2]])
   
   tmp <- all.couple.rotations.at.k(dend, 3)
   plot(tmp[[1]])
   plot(tmp[[2]])
   
   tmp <- all.couple.rotations.at.k(dend, 4)
   length(tmp)
   plot(tmp[[1]])
   plot(tmp[[2]])
   
   tmp <- all.couple.rotations.at.k(dend, 5)
   length(tmp)
   plot(tmp[[1]])
   plot(tmp[[2]])	
}

# cut.hierarchical.cluster.matrix(dend1)

# all.couple.rotations.at.k(dend, 2)
# all.couple.rotations.at.k(Dan_arc_tree, 3)
# all.couple.rotations.at.k(dend, 10)
# untangle.forward.rotate.1side(dend1, dend2)

# (dend12s[[1]], dend12s[[2]])
# dend1 = dend12s[[1]]
# dend2_fixed = dend12s[[2]]


untangle.forward.rotate.1side <- function(dend1, dend2_fixed, L = 1) {
   # this function gets two dendgrams, and goes over each k splits of the first dend1, and checks if the flip at level k of splitting imporves the entanglement between dend1 and dend2 (Which is fixed)
   require(plyr)
   leaves_order <- order.dendrogram(dend1)
   best_dend <- dend1
   best_dend_heights_per_k <- dendrogram.heights.per.k(best_dend) # since this function takes a looong time, I'm running it here so it will need to run only once!	
   
   for(k in 2:length(leaves_order)) {
      dend1_k_rotated <- all.couple.rotations.at.k(best_dend, k, dendrogram_heights_per_k = best_dend_heights_per_k)
      dend1_cut_k_entanglements <- laply(dend1_k_rotated, entanglement, dend2 = dend2_fixed, L = L)
      ss_best_dend <- which.min(dend1_cut_k_entanglements)
      current_best_dend <- dend1_k_rotated[[ss_best_dend]]
      
      # if this loop's best dendro is not identical to our last best dendro - then we should pick it as the new best dendro
      #		And that means we'll have to update the dendrogram.heights.per.k (which takes time, and we would like to avoid if it is not necessary)
      if(!identical(current_best_dend, best_dend)) {
         best_dend <- current_best_dend
         best_dend_heights_per_k <- dendrogram.heights.per.k(best_dend) # since this function takes a looong time, I'm running it here so it will need to run only once!	
      }# however, if the current dend is just like our best dend - then there is NO NEED to update dendrogram.heights.per.k (and we just saved some time!!)
      # this combination is only useful if we have a tree for which there are only a few rotations which are useful
   }
   
   return(best_dend)	
}

# system.time(identical(dend1, dend2))
# system.time(identical(dend1, dend1))
# identical(dend1, dend1)


untangle.backward.rotate.1side <- function(dend1, dend2_fixed , L = 1) {
   # this function gets two dendgrams, and goes over each k splits of the first dend1, and checks if the flip at level k of splitting imporves the entanglement between dend1 and dend2 (Which is fixed)
   require(plyr)
   leaves_order <- order.dendrogram(dend1)
   best_dend <- dend1
   
   for(k in length(leaves_order):2) {
      dend1_k_rotated <- all.couple.rotations.at.k(best_dend, k)
      dend1_cut_k_entanglements <- laply(dend1_k_rotated, entanglement, dend2 = dend2_fixed, L = L)
      ss_best_dend <- which.min(dend1_cut_k_entanglements)
      best_dend <- dend1_k_rotated[[ss_best_dend]]
   }
   
   return(best_dend)	
}


# 
# untangle.forward.step.rotate.1side <- function(dend1, dend2_fixed) {
# 	# this function gets two dendgrams, and goes over each k splits of the first dend1, and checks if the flip at level k of splitting imporves the entanglement between dend1 and dend2 (Which is fixed)
# 	require(plyr)
# 	leaves_order <- order.dendrogram(dend1)
# 	best_dend <- dend1
# 	
# 	k_visited <- rep(F, length(leaves_order))
# 	k_visited[1] <- T # I don't need the first one
# 	k <- 1
# 	
# 	while(!all(k_visited)) {
# 		# create all of the rotations with k+-1:
# 		dend1_k_p1_rotated <- all.couple.rotations.at.k(best_dend, k+1)
# 		dend1_k_m1_rotated <- all.couple.rotations.at.k(best_dend, k-1)
# 		# find the enteglement for all of them:
# 		dend1_cut_k_p1_entanglements <- laply(dend1_k_p1_rotated, entanglement, dend2 = dend2_fixed)
# 		dend1_cut_k_m1_entanglements <- laply(dend1_k_m1_rotated, entanglement, dend2 = dend2_fixed)
# 		# what is best, forward or backward?
# 		if(min(dend1_cut_k_p1_entanglements) > min(dend1_cut_k_m1_entanglements)) {
# 		
# 		}
# 		k <- k + 1
# 		ss_best_dend <- which.min(dend1_cut_k_entanglements)
# 		best_dend <- dend1_k_rotated[[ss_best_dend]]		
# 		
# 		all.couple.rotations.at.k(best_dend, -1)
# 	}
# 	
# 	return(best_dend)	
# }


# dend12s_1_better <- untangle.forward.rotate.1side(dend1, dend2)
# cutree(dend1, 10)






untangle.forward.rotate.2side <- function(dend1, dend2, max_n_iterations = 10, output_times = T, L = 1) {
   # this function gets two dendgrams, and orders dend1 and 2 until a best entengelment is reached.
   
   
   # Next, let's try to improve upon this tree using a forwared rotation of our tree:
   dend1_better <- untangle.forward.rotate.1side(dend1, dend2, L = L) 
   dend2_better <- untangle.forward.rotate.1side(dend2, dend1_better, L = L) 
   
   entanglement_new <- entanglement(dend1_better, dend2_better, L = L) 
   entanglement_old <- entanglement_new+1
   
   times <- 1
   
   while(times < max_n_iterations & !identical(entanglement_new, entanglement_old)) { # if we got an improvement from last entaglement, we'll keep going!
      entanglement_old <- entanglement_new
      
      dend1_better_loop <- untangle.forward.rotate.1side(dend1_better, dend2_better, L = L) 
      # if the new dend1 is just like we just had - then we can stop the function since we found the best solution - else - continue
      if(identical(dend1_better_loop, dend1_better)) {
         break;
      } else {
         dend1_better <- dend1_better_loop
      }			 
      
      # if the new dend2 is just like we just had - then we can stop the function since we found the best solution - else - continue
      dend2_better_loop <- untangle.forward.rotate.1side(dend2_better, dend1_better, L = L) 
      if(identical(dend2_better_loop, dend2_better)) {
         break;
      } else {
         dend2_better <- dend2_better_loop
      }			 
      
      entanglement_new <- entanglement(dend1_better, dend2_better, L = L) 
      times <- times + 1
   }
   
   # identical(1,1+.00000000000000000000000001) # T
   if(output_times) cat("We ran untangle ", times, " times\n")
   
   return(list(dend1 = dend1_better, dend2 = dend2_better))	
}


# evolution algorithm
untangle.intercourse <- function(brother_1_dend1, brother_1_dend2, 
                                 sister_2_dend1, 	sister_2_dend2, L = 1) 
{
   # Gets two pairs of dend, and returns two childrens (inside a list)
   children_1 <- untangle.forward.rotate.2side(brother_1_dend1,sister_2_dend2, L = L) 
   children_2 <- untangle.forward.rotate.2side(sister_2_dend1,brother_1_dend2, L = L) 
   
   list(children_1, children_2)
}

entanglement.return.best.brother <- function(brother_1_dend1, brother_1_dend2, 
                                             brother_2_dend1, brother_2_dend2, L = 1)
{
   # Gets two pairs of dend, and returns the pair with the best (minimal) entanglement
   
   if( entanglement(brother_1_dend1, brother_1_dend2, L = L) <
          entanglement(brother_2_dend1, brother_2_dend2, L = L)  ) {
      return(list(brother_1_dend1, brother_1_dend2))
   } else {
      return(list(brother_2_dend1, brother_2_dend2))
   }
}

untangle.intercourse.evolution <- function(intercourse, L = 1) {
   # intercourse is a list with two elements.  Each element has two dends
   entanglement.return.best.brother(intercourse[[1]][[1]], intercourse[[1]][[2]],
                                    intercourse[[2]][[1]], intercourse[[2]][[2]], L = L) 
   
   
}


untangle.evolution<- function(brother_1_dend1, brother_1_dend2, 
                              sister_2_dend1, 	sister_2_dend2, L = 1) 
{
   intercourse <- untangle.intercourse(brother_1_dend1, brother_1_dend2, 
                                       sister_2_dend1, 	sister_2_dend2, L = L)  # creates a list with two pairs of dends
   untangle.intercourse.evolution(intercourse, L = L)  # returns the best child
}










####
# A new approuch - I will go through every possible flip on one side, and find the one that gives the best improvement.
# I will do the same on each tree, back and forth, until no better flip is found.

untangle.best.k.to.rotate.by.1side <- function(dend1, dend2_fixed, L = 1) {
   # this function gets two dendgrams, and goes over each k splits of the first dend1, and checks if the flip at level k of splitting imporves the entanglement between dend1 and dend2 (Which is fixed)
   require(plyr)
   leaves_order <- order.dendrogram(dend1)
   best_dend <- dend1
   dend1_k_rotated <- NULL
   
   best_dend_heights_per_k <- dendrogram.heights.per.k(best_dend) # since this function takes a looong time, I'm running it here so it will need to run only once!	
   # this makes the function about twice as fast... 
   
   for(k in 2:length(leaves_order)) {
      dend1_k_rotated <- c(dend1_k_rotated, 
                           all.couple.rotations.at.k(best_dend, k, 
                                                     dendrogram_heights_per_k = best_dend_heights_per_k))
   }
   
   dend1_cut_k_entanglements <- laply(dend1_k_rotated, entanglement, dend2 = dend2_fixed, L = L)
   ss_best_dend <- which.min(dend1_cut_k_entanglements)
   best_dend <- dend1_k_rotated[[ss_best_dend]]
   return(best_dend)	
}



flip.1.and.2 <- function(x) ifelse(x == 1, 2, 1)

untangle.best.k.to.rotate.by.2side.backNforth <- function(dend1, dend2, times_to_stop = 2, output_times = T, L = 1) {
   # this function gets two dendgrams, and orders dend1 and then 2 and then 1 again - back and forth -until a best entengelment is reached.
   
   was_improved <- T # e.g: we can improve it further
   counter <- 1
   
   while(was_improved) {
      entanglement_old <- entanglement(dend1, dend2, L = L) 
      dend1 <- untangle.best.k.to.rotate.by.1side(dend1, dend2, L = L) 
      dend2 <- untangle.best.k.to.rotate.by.1side(dend2, dend1, L = L) 
      entanglement_new <- entanglement(dend1, dend2, L = L) 
      was_improved <- identical(entanglement_old, entanglement_new)
      counter <- counter + 1
   }
   # identical(1,1+.00000000000000000000000001) # T
   if(output_times) cat("We ran untangle.best.k.to.rotate.by.2side.backNforth ", counter, " times")
   
   return(list(dend1 = dend1, dend2 = dend2))	
}




if(F) {
   # example
   dist_DATA <- dist(USArrests[1:20,])
   # First two dummy clusters (since you didn't provide with some...)
   hc1 <- hclust(dist_DATA , "single")
   hc2 <- hclust(dist_DATA , "complete")
   dend1 <- as.dendrogram(hc1)
   dend2 <- as.dendrogram(hc2)	
   entanglement(dend1, dend2) 		
   
   system.time(dend12_best_01 <- untangle.forward.rotate.2side(dend1, dend2, L = 2)) # 0.47 sec
   system.time(dend12_best_02 <- untangle.best.k.to.rotate.by.2side.backNforth(dend1, dend2, L = 2)) # 0.44 sec
   tanglegram(dend1, dend2) 
   tanglegram(dend12_best_01[[1]], dend12_best_01[[2]]) 
   tanglegram(dend12_best_02[[1]], dend12_best_02[[2]]) 
}








richrach <- function(x) { 
   # move back and forth between the beginning and the end of a vector
   c(t(cbind(x, rev(x))))[1:length(x)]
   # example:
   # richrach(1:6)
   # from this:  1 2 3 4 5 6
   # to this: 1 6 2 5 3 4
}

richrach.xy <- function(x,y) { 
   # move back and forth between the beginning and the end of a vector
   c(t(cbind(x, y)))[1:length(x)]
   # example:
   # richrach(1:6)
   # from this:  1 2 3 4 5 6
   # to this: 1 6 2 5 3 4
}


odd.locations <- function(x) {
   x[seq(1, length(x), by = 2)]
}
# odd.locations(1:6)



if(F) {
   
   dist_DATA <- dist(USArrests[1:30,])
   dist_DATA <- dist(USArrests[1:10,])
   # First two dummy clusters (since you didn't provide with some...)
   hc1 <- hclust(dist_DATA , "single")
   hc2 <- hclust(dist_DATA , "complete")
   dend1 <- as.dendrogram(hc1)
   dend2 <- as.dendrogram(hc2)
   
   tanglegram(dend1, dend2) 
   entanglement(dend1, dend2) # 0.8
   
   # after sorting we get a better measure of entanglement and also a better looking plot
   tanglegram(sort(dend1), sort(dend2))
   entanglement(sort(dend1), sort(dend2)) # 0.1818
   
   # let's cause some shuffle... (e.g: mix the dendrogram, and see how that effects the outcome)
   set.seed(134)
   s_dend1 <- shuffle(dend1)
   s_dend2 <- shuffle(dend2)
   tanglegram(s_dend1, s_dend2)
   entanglement(s_dend1, s_dend2) # 0.7515
   
   
   set.seed(1234)
   dend12s <- untangle.random.search(dend1, dend2, R = 10)
   entanglement(dend12s[[1]], dend12s[[2]]) # 0.042
   tanglegram(dend12s[[1]], dend12s[[2]]) # 
   # this is a case where it is CLEAR that the simplest heuristic would improve this to 0 entanglement...	
   
   # let's see if we can reach a good solution using a greedy forward selection algorithm
   dend12s_1_better <- untangle.forward.rotate.1side(dend12s[[1]], dend12s[[2]])
   entanglement(dend12s_1_better, dend12s[[2]]) # from 0.042 to 0.006 !!
   tanglegram(dend12s_1_better, dend12s[[2]]) # 
   
   # let's see from the beginning
   entanglement(dend1, dend2) # 0.6
   tanglegram(dend1, dend2) # 0.6
   dend12s_1_better <- untangle.forward.rotate.1side(dend1, dend2)
   entanglement(dend12s_1_better, dend2) # from 0.6 to 0.036
   tanglegram(dend12s_1_better, dend2) # 
   # let's try the other side:
   dend12s_2_better <- untangle.forward.rotate.1side(dend2, dend12s_1_better)
   entanglement(dend12s_1_better, dend12s_2_better) # no improvment
   
   
   
   dend2_01 <- untangle.forward.rotate.1side(dend2, dend1)
   dend2_02 <- untangle.backward.rotate.1side(dend2, dend1)
   dend2_03 <- untangle.backward.rotate.1side(dend2_01, dend1)
   dend2_04 <- untangle.forward.rotate.1side(dend2_02, dend1)
   dend2_05 <- untangle.evolution(dend1, dend2 , dend1, dend2_01 )
   entanglement(dend1, dend2) 
   entanglement(dend1, dend2_01) 
   
   entanglement(dend1, dend2_02) 
   entanglement(dend1, dend2_03) 
   entanglement(dend1, dend2_04) 
   entanglement(dend2_05[[1]], dend2_05[[2]]) 
   tanglegram(dend1, dend2) 
   tanglegram(dend1, dend2_01) 
   tanglegram(dend1, dend2_02) 
   tanglegram(dend1, dend2_03) 
   tanglegram(dend1, dend2_04) 
   tanglegram(dend2_05[[1]], dend2_05[[2]]) 
   
   
   
   entanglement(dend1, dend2) 
   tanglegram(dend1, dend2) 
   dend2_01 <- untangle.forward.rotate.1side(dend2, dend1)
   dend2_01 <- untangle.backward.rotate.1side(dend2, dend1)
   tanglegram(dend1, dend2_01) 
   
   
   
   # 
   dist_DATA <- dist(USArrests[1:10,])
   # First two dummy clusters (since you didn't provide with some...)
   hc1 <- hclust(dist_DATA , "single")
   hc2 <- hclust(dist_DATA , "complete")
   dend1 <- as.dendrogram(hc1)
   dend2 <- as.dendrogram(hc2)
   dend1_01 <- untangle.forward.rotate.1side(dend1, dend2)
   entanglement(dend1, dend2) 
   entanglement(dend1_01, dend2) 
   tanglegram(dend1, dend2) 
   tanglegram(dend1_01, dend2) 
   
   system.time(dend1_01 <- untangle.forward.rotate.1side(dend1, dend2)) # 0.47 sec
   system.time(dend1_01 <- untangle.best.k.to.rotate.by(dend1, dend2)) # 0.44 sec
   tanglegram(dend1, dend2) 
   tanglegram(dend1_01, dend2) 
   
   
   
   
   #### profiling
   require(profr)
   slow_dude <- profr(untangle.forward.rotate.1side(dend2, dend1))
   head(slow_dude)
   summary(slow_dude)
   plot(slow_dude)
   
   require(reshape)
   a <- cast(slow_dude, f~., value="time", fun.aggregate=c(length, sum))
   a[order(a[,3]),]
   ## End(Not run)
   slow_dude[slow_dude$time > .079991, ]
   
   
   # this also helped:
   # 	install.packages("microbenchmark")
   require(microbenchmark)
   
   system.time(entanglement(dend1, dend2) 	) # 0.01
   microbenchmark( entanglement(dend1, dend2) , times = 10 )# so this is 10 times faster (the medians)
   #		betweem 0.011 to 0.038
   
}






if(F){
   
   # Finding the BEST tree by going through many random seeds and looking for a good solution :)
   
   entanglement_history <- c()
   
   
   get.seed <- function(max_lengh = 10e7) round(runif(1)*max_lengh)
   
   best_seed <- 28754448 # 55639690 # 5462457 # 75173309 # 20295644
   set.seed(best_seed) 
   times_a_better_seed_was_found <- 0
   random_dendros <- untangle.random.search(yoavs_tree, Dan_arc_tree, R = 1, L = 1.5)
   rotated_dendros <- untangle.forward.rotate.2side(random_dendros[[1]], random_dendros[[2]], L = 1.5)
   best_entanglement <- entanglement(rotated_dendros[[1]], rotated_dendros[[2]], L = 1.5)
   # tanglegram(rotated_dendros[[1]], rotated_dendros[[2]])
   
   
   for(i in 1:100000) {
      print(i)
      current_seed <- get.seed()
      set.seed(current_seed)
      random_dendros <- untangle.random.search(yoavs_tree, Dan_arc_tree, R = 10, L = 1.5)
      rotated_dendros <- untangle.forward.rotate.2side(random_dendros[[1]], random_dendros[[2]], L = 1.5)
      new_entanglement <- entanglement(rotated_dendros[[1]], rotated_dendros[[2]], L = 1.5)
      
      entanglement_history <- c(entanglement_history, new_entanglement)		
      
      if(new_entanglement < best_entanglement ){
         times_a_better_seed_was_found <- times_a_better_seed_was_found + 1
         best_seed <- current_seed
         print(best_seed)
         print(new_entanglement)
         best_entanglement <- new_entanglement
         tanglegram(rotated_dendros[[1]], rotated_dendros[[2]])
      }	
      flush.console()
   }
   
   
   hist(entanglement_history)
   
}











# this function is from the combinat package
# permn <- function (x, fun = NULL, ...) 
# {
#    if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == 
# 				x) 
# 		x <- seq(x)
# 	n <- length(x)
# 	nofun <- is.null(fun)
# 	out <- vector("list", gamma(n + 1))
# 	p <- ip <- seqn <- 1:n
# 	d <- rep(-1, n)
# 	d[1] <- 0
# 	m <- n + 1
# 	p <- c(m, p, m)
# 	i <- 1
# 	use <- -c(1, n + 2)
# 	while (m != 1) {
# 		out[[i]] <- if (nofun) 
# 			x[p[use]]
# 		else fun(x[p[use]], ...)
# 		i <- i + 1
# 		m <- n
# 		chk <- (p[ip + d + 1] > seqn)
# 		m <- max(seqn[!chk])
# 		if (m < n) 
# 			d[(m + 1):n] <- -d[(m + 1):n]
# 		index1 <- ip[m] + 1
# 		index2 <- p[index1] <- p[index1 + d[m]]
# 		p[index1 + d[m]] <- m
# 		tmp <- ip[index2]
# 		ip[index2] <- ip[m]
# 		ip[m] <- tmp
# 	}
# 	out
# }
# 





order.weights.by.cluster.order <- function(weights, cluster_id, new_cluster_order) {
   # this function gets a vector of weights. The clusters each weight belongs to
   # and a new order for the clusters
   # and outputs the new weight vector after ordering the vector by the new order of the cluster (internal order of elements within each cluster is preserved)
   output <- NULL
   for(i in seq_along(new_cluster_order)) {
      output <- c(output, weights[cluster_id == new_cluster_order[i]])
   }
   return(output)
}
if(F){
   # example:
   x = c(1,2,3,4,5,6)
   ord1 = c(1,1,2,2,2,3)
   ord_of_clusters = c(2,1,3)
   c(x[ord1 == ord_of_clusters[1]],x[ord1 == ord_of_clusters[2]],x[ord1 == ord_of_clusters[3]])
   order.weights.by.cluster.order(x, ord1, ord_of_clusters)	
}
