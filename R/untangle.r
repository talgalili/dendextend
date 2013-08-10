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









match.leaves.by.labels <- function(dendo_change, dendo_template , check_that_labels_match = F) {   
   
   #let's put the leaves' numbers and labels in two data.frames (one for Dan and one for Yoav)	
   # 	tree_to_change_labels_order <- data.frame(labels = labels(dendo_change), values = leaves.values(dendo_change))
   # 	tree_template_labels_order <- data.frame(labels = labels(dendo_template), values = leaves.values(dendo_template))
   tree_to_change_labels_order_labels = labels(dendo_change)
   # 	tree_to_change_labels_order_values = leaves.values(dendo_change)	# this is not used.
   tree_template_labels_order_labels = labels(dendo_template)
   
   if(check_that_labels_match) { 
      if(any(sort(tree_to_change_labels_order_labels) != sort(tree_template_labels_order_labels))) stop("labels do not match in both trees.  Please make sure to fix the labels names!")
   }	
   
   tree_template_labels_order_values = leaves.values(dendo_template)
   
   # this gives us how to order y so it would be in the order of x.
   # y_to_order_like_x <- c(2,3,1,4)
   # y_to_order_like_x[match(c(1:4), y_to_order_like_x)]
   # I want to order the numbers in yoav_tree so that they would match the needed order in dans_tree
   
   ss_order_change_leaf_numbers_to_match_template <- match(x= tree_to_change_labels_order_labels, table= tree_template_labels_order_labels)
   # let's check if it works: (it does!)
   # data.frame(
   # 		yoav_num = Yoav_tree_labels_order$order[ss_order_yoav_leaf_numbers_to_match_dans],
   # 		yoav_lab = Yoav_tree_labels_order$labels[ss_order_yoav_leaf_numbers_to_match_dans],
   # 		dan_num = Dan_arc_tree_labels_order$order,
   # 		dan_lab = Dan_arc_tree_labels_order$labels)
   
   
   tree_new_leaf_numbers <- tree_template_labels_order_values[ss_order_change_leaf_numbers_to_match_template]
   leaves.values(dendo_change) <- tree_new_leaf_numbers
   
   return(dendo_change)
   # data.frame(
   # 		yoav_num = Yoav_tree_labels_order$order[ss_order_yoav_leaf_numbers_to_match_dans],
   # 		yoav_lab = Yoav_tree_labels_order$labels[ss_order_yoav_leaf_numbers_to_match_dans],
   # 		dan_num = leaves.values(Dan_arc_tree),
   # 		dan_lab = labels(Dan_arc_tree))
}


# untangle.dendrogram # A function to take two dendrograms and rotate them so to minimize some penalty on entanglement 

# entanglement

entanglement <- function (...) UseMethod("entanglement")
entanglement.default <- function (object, ...) stop("no default function for entanglement")


# ### OLD entanglement concept.
# entanglement.dendrogram <- function(dendo1,dendo2 , method = c("absolute.rank.sum", "cor.spearman") )
# {
# 	# One day, one might think of other measures of entanglement.  But for now, we have only one method ("cor.spearman").  Which is the 1-absolute value of the tanks of the values in the two dendrograms.
# 	# A level close to 1 is bad (very entangled).  A number close to 0 is good (low entanglement)
# 	# leaves(dendo1),leaves(dendo2)
# 	
# 	n_leaves <- length(leaves.values(dendo1)) # how many leaves do we have? (number of leaves)
# 	leaves.values(dendo1) <- seq_len(n_leaves) # change the leaves of dendo1 to be 1:n
# 	dendo2 <- match.leaves.by.labels(dendo2	, dendo1) # make sure that the numbers if the 
# 
# 	if(method[1] == "cor.spearman") {
# 		order_cor <- cor(leaves.values(dendo1),leaves.values(dendo2), method = "spearman")
# 		entanglement_result <- (1-order_cor)/2 # cor=1 is best (0 entanglement), cor = 0 is bad (0.5 entanglement), cor = -1 is worst (1 entanglament)		
# 	}
# 	if(method[1] == "absolute.rank.sum") {					
# 		entanglement_result <- sum(abs(leaves.values(dendo1)-leaves.values(dendo2)))
# 	}
# 	
# 	entanglement_result
# }



# tanglegram(dendo1,dendo2)
# tanglegram(sort(dendo1),sort(dendo2))
# entanglement(dendo1,dendo2, L = 0)
# entanglement(dendo1,dendo2, L = 0.25)
# entanglement(dendo1,dendo2, L = 1)
# entanglement(dendo1,dendo2, L = 2)
# entanglement(dendo1,dendo2, L = 10)
# entanglement(sort(dendo1),sort(dendo2), L=0)
# entanglement(sort(dendo1),sort(dendo2), L=0.25)
# entanglement(sort(dendo1),sort(dendo2), L=1)
# entanglement(sort(dendo1),sort(dendo2), L=2)
# entanglement(sort(dendo1),sort(dendo2), L=10)

# OLD and SLOW
entanglement.dendrogram <- function(dendo1,dendo2, L = 1.5)
{
   # One day, one might think of other measures of entanglement.  But for now, we have only one method ("cor.spearman").  Which is the 1-absolute value of the tanks of the values in the two dendrograms.
   # A level close to 1 is bad (very entangled).  A number close to 0 is good (low entanglement)
   # leaves(dendo1),leaves(dendo2)
   # L tells us which panelty level we are at (L0, L1, L2, partial L's etc).  L>1 means that we give a big panelty for sharp angles.  While L->0 means that any time something is not a streight horizontal line, it gets a large penalty
   # If L=0.1 it means that we much prefer streight lines over non streight lines
   
   if(L==0) L <- L + 1e-50 # this is in order to make sure L is not ==0.  Because that would just create nonsical meaning.
   
   
   n_leaves <- length(leaves.values(dendo1)) # how many leaves do we have? (number of leaves)
   one_to_n_leaves <- seq_len(n_leaves)
   leaves.values(dendo1) <- one_to_n_leaves # change the leaves of dendo1 to be 1:n
   dendo2 <- match.leaves.by.labels(dendo2	, dendo1) # make sure that the numbers if the 
   
   sum.abs.diff.L <- function(x,y,L) sum(abs(x-y)**L)
   
   entanglement_result <- sum.abs.diff.L(leaves.values(dendo1), leaves.values(dendo2), L)	
   worse_entanglement_result <- sum.abs.diff.L(one_to_n_leaves, rev(one_to_n_leaves), L)		
   normalized_entanglement_result <- entanglement_result/worse_entanglement_result # should range between 0 (no etnaglement) and 1 (max entangelment
   
   normalized_entanglement_result
}







# FASTER version of entanglement (assumes that the leaves values are matched correctly with the labels in both trees!)
match.leaves.values.by.old.values <- function(dendo_change, dendo_template , dendo_template_old_values,
                                              check_that_labels_match = F, check_that_leaves_values_match =F ,
                                              print_NOTE = F) {	
   
   # this function was made to help make entanglement.dendrogram faster.
   # But it relies on some important assumptions (otherwise, its results will be nonsense!)
   
   if(check_that_labels_match) { # I am turning this check to FALSE since it takes 0.03 sec from the function (which is a long time when running this function a lot)
      if(any(sort(labels(dendo_change)) != sort(labels(dendo_template)))) stop("labels do not match in both trees.  Please make sure to fix the labels names!")
   }
   if(check_that_leaves_values_match) { # I am turning this check to FALSE since it takes 0.03 sec from the function (which is a long time when running this function a lot)
      if(any(sort(leaves.values(dendo_change)) != sort(leaves.values(dendo_template)))) stop("leaves.values do not match in both trees.  Please make sure to fix the labels names!")
   }
   
   if(print_NOTE) cat("NOTE:
                      Make sure that the values in dendo_template_old_values match the labels of dendo1 in the same way
                      as the values and labels of the dendo_change!
                      ")
   
   
   #let's put the leaves' numbers and labels in two data.frames (one for Dan and one for Yoav)	
   # 	tree_to_change_labels_order <- data.frame(labels = labels(dendo_change), values = leaves.values(dendo_change))
   # 	tree_template_labels_order <- data.frame(labels = labels(dendo_template), values = leaves.values(dendo_template))
   tree_to_change_labels_order_values = leaves.values(dendo_change)
   # 	tree_to_change_labels_order_values = leaves.values(dendo_change)	# this is not used.
   tree_template_labels_order_values = leaves.values(dendo_template) # these should be values after some change was done outside the function (and dendo_template_old_values are the values before the change)
   
   # this gives us how to order y so it would be in the order of x.
   # y_to_order_like_x <- c(2,3,1,4)
   # y_to_order_like_x[match(c(1:4), y_to_order_like_x)]
   # I want to order the numbers in yoav_tree so that they would match the needed order in dans_tree
   
   ss_order_change_leaf_numbers_to_match_template <- match(x= tree_to_change_labels_order_values, table= dendo_template_old_values)
   # let's check if it works: (it does!)
   # data.frame(
   # 		yoav_num = Yoav_tree_labels_order$order[ss_order_yoav_leaf_numbers_to_match_dans],
   # 		yoav_lab = Yoav_tree_labels_order$labels[ss_order_yoav_leaf_numbers_to_match_dans],
   # 		dan_num = Dan_arc_tree_labels_order$order,
   # 		dan_lab = Dan_arc_tree_labels_order$labels)
   
   
   new_leaves_values <- tree_template_labels_order_values[ss_order_change_leaf_numbers_to_match_template]
   leaves.values(dendo_change) <- new_leaves_values
   
   return(dendo_change)
   # data.frame(
   # 		yoav_num = Yoav_tree_labels_order$order[ss_order_yoav_leaf_numbers_to_match_dans],
   # 		yoav_lab = Yoav_tree_labels_order$labels[ss_order_yoav_leaf_numbers_to_match_dans],
   # 		dan_num = leaves.values(Dan_arc_tree),
   # 		dan_lab = labels(Dan_arc_tree))
}


entanglement.dendrogram <- function(dendo1,dendo2, L = 1.5, leaves_matching_method = c("leaves", "labels")) {
   # One day, one might think of other measures of entanglement.  But for now, we have only one method ("cor.spearman").  Which is the 1-absolute value of the tanks of the values in the two dendrograms.
   # A level close to 1 is bad (very entangled).  A number close to 0 is good (low entanglement)
   # leaves(dendo1),leaves(dendo2)
   # L tells us which panelty level we are at (L0, L1, L2, partial L's etc).  L>1 means that we give a big panelty for sharp angles.  While L->0 means that any time something is not a streight horizontal line, it gets a large penalty
   # If L=0.1 it means that we much prefer streight lines over non streight lines
   
   if(L==0) L <- L + 1e-50 # this is in order to make sure L is not ==0.  Because that would just create nonsical meaning.
   
   
   n_leaves <- length(leaves.values(dendo1)) # how many leaves do we have? (number of leaves)
   one_to_n_leaves <- seq_len(n_leaves)
   
   if(leaves_matching_method[1] == "leaves") {	
      dendo1_old_values <- leaves.values(dendo1)
      leaves.values(dendo1) <- one_to_n_leaves # change the leaves of dendo1 to be 1:n	
      dendo2 <- match.leaves.values.by.old.values(dendo2	, dendo1, dendo1_old_values) # make sure that the numbers if the 
   } else { # "labels" - this method is "safer" (since we can easily see if the labels on the two trees match or not
      # however, this is twice as slow (which adds up quite a bit with the functions that rely on this)
      # Hence, it is best to make sure that the trees used here have the same labels and the SAME values matched to these values
      leaves.values(dendo1) <- one_to_n_leaves # change the leaves of dendo1 to be 1:n
      dendo2 <- match.leaves.by.labels(dendo2	, dendo1) # This one is "slow"
   }
   
   sum.abs.diff.L <- function(x,y,L) sum(abs(x-y)**L)
   
   entanglement_result <- sum.abs.diff.L(leaves.values(dendo1), leaves.values(dendo2), L)	
   worse_entanglement_result <- sum.abs.diff.L(one_to_n_leaves, rev(one_to_n_leaves), L)		
   normalized_entanglement_result <- entanglement_result/worse_entanglement_result # should range between 0 (no etnaglement) and 1 (max entangelment
   
   normalized_entanglement_result
}


# get("sort")
# stir is a function that randomilly "stirs" a dendrogram leaves order (by means of rotation)
stir <- function (...) UseMethod("stir")
stir.default <- function (object, ...) stop("no default function for entanglement")
stir.dendrogram <- function(object) {
   # takes a dendrogram object and stirrs its branches in a random fashion
   # 	num_of_leaves <- length(labels(object))	# leaves.value is faster then labels!
   num_of_leaves <- length(leaves.values(object))
   random_weights <- sample(seq_len(num_of_leaves)) # a random ordaring of 1:num_of_leaves weights
   rotate(object, random_weights)
}


untangle.random.search <- function(dendo1, dendo2, R = 100, L = 1) {
   # this is a simple random search algorithm for the optimal tanglegram layout problem.
   # it stirrs the trees, and see if we got a better entanglement or not
   
   best_ordaring_entanglement <- entanglement(dendo1, dendo2, L)
   
   for(i in 1:R) {
      s_dendo1 <- stir(dendo1)
      s_dendo2 <- stir(dendo2)
      current_entanglement <- entanglement(s_dendo1, s_dendo2, L)
      
      # if we came across a better ordaring, then update the "Best" dendrograms 
      if(current_entanglement < best_ordaring_entanglement) {
         best_ordaring_entanglement<- current_entanglement
         optimal_dendo1 <- s_dendo1
         optimal_dendo2 <- s_dendo2			
      }
   }
   
   return(list(dendo1 = optimal_dendo1, dendo2 = optimal_dendo2))
}


# this function is from the combinat package
# permn <- function (x, fun = NULL, ...) 
# {
# 	if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == 
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



flip.leaves <- function(dendo, leaves1, leaves2) {
   # flip a node in a tree based on the leaves in each branch in the node:
   # this function gets a dendogram with two vector of leaves that needs to be flipped with one another on the tree
   # we assume here unique values of leaves.
   # also notice that this is based on the values of the leaves and NOT their labels.
   leaves_values <- leaves.values(dendo)
   weights <- seq_along(leaves_values)
   
   #turn the values of leaves and leaves1/2 to strings with || delim:
   leaves_values_string <- collapse.pipes.zzz(leaves_values)
   leaves1_string <- collapse.pipes.zzz(leaves1)
   leaves2_string <- collapse.pipes.zzz(leaves2)
   # then flips the locations of leaves1 and 2 in the string
   flipped_leaves_values_string <- flip.strings(leaves_values_string, leaves1_string, leaves2_string)
   # and turn the string back to a vector of flipped leaves values:
   flipped_leaves_values <- as.numeric(remove.pipes.and.zzz(flipped_leaves_values_string))
   
   new_order_weights <- match(flipped_leaves_values, leaves_values) # order the leaves_values to be like flipped_leaves_values
   # leaves_values[new_order_weights]
   # now use this order to order the weights!
   new_weights <- weights[new_order_weights]
   
   flipped_dendo <- rotate(dendo, new_weights) # and lastly - rotate the dendo by the leaves to flip.
   
   return(flipped_dendo)
}

if(F) { # example
   leaves.values(dendo)
   plot(dendo)
   flip.leaves(dendo, c(10), c(1))
   plot(flip.leaves(dendo, c(2), c(3,5)))
   plot(flip.leaves(dendo, c(9), c(2,3,5)))
   plot(flip.leaves(dendo, c(7), c( 1L, 8L, 4L, 6L, 10L, 9L, 2L, 3L, 5L)))
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
   
   leaves_values <- leaves.values(dendro)
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
            leaves1 <- leaves_values[ss_leaves1]
            leaves2 <- leaves_values[ss_leaves2]
            
            # 				plot(flip.leaves(dendro, leaves1, leaves2))
            # Flipping the branches of the two adjecent clusters:
            permutation_indx <- permutation_indx + 1
            permutated_dendro[[permutation_indx]] <- flip.leaves(dendro, leaves1, leaves2) # this will not work for hclust (will for dendro)
         }			
      }
   }	
   return(permutated_dendro)
}


# dendo=best_dendo
# k = 13
# all.couple.rotations.at.k(best_dendo, k)
# system.time(all.couple.rotations.at.k(dendo1, k = 4))




if(F) { # example
   # 	tmp <- all.couple.rotations.at.k(dendo, 10)
   # 	flip.leaves(dendo, leaves1, leaves2)
   # 	leaves.values(dendo)
   
   
   tmp <- all.couple.rotations.at.k(dendo, 2)
   length(tmp)
   plot(tmp[[1]])
   plot(tmp[[2]])
   
   tmp <- all.couple.rotations.at.k(dendo, 3)
   plot(tmp[[1]])
   plot(tmp[[2]])
   
   tmp <- all.couple.rotations.at.k(dendo, 4)
   length(tmp)
   plot(tmp[[1]])
   plot(tmp[[2]])
   
   tmp <- all.couple.rotations.at.k(dendo, 5)
   length(tmp)
   plot(tmp[[1]])
   plot(tmp[[2]])	
}

# cut.hierarchical.cluster.matrix(dendo1)

# all.couple.rotations.at.k(dendo, 2)
# all.couple.rotations.at.k(Dan_arc_tree, 3)
# all.couple.rotations.at.k(dendo, 10)
# untangle.forward.rotate.1side(dendo1, dendo2)

# (dendo12s[[1]], dendo12s[[2]])
# dendo1 = dendo12s[[1]]
# dendo2_fixed = dendo12s[[2]]


untangle.forward.rotate.1side <- function(dendo1, dendo2_fixed, L = 1) {
   # this function gets two dendograms, and goes over each k splits of the first dendo1, and checks if the flip at level k of splitting imporves the entanglement between dendo1 and dendo2 (Which is fixed)
   require(plyr)
   leaves_values <- leaves.values(dendo1)
   best_dendo <- dendo1
   best_dendo_heights_per_k <- dendrogram.heights.per.k(best_dendo) # since this function takes a looong time, I'm running it here so it will need to run only once!	
   
   for(k in 2:length(leaves_values)) {
      dendo1_k_rotated <- all.couple.rotations.at.k(best_dendo, k, dendrogram_heights_per_k = best_dendo_heights_per_k)
      dendo1_cut_k_entanglements <- laply(dendo1_k_rotated, entanglement, dendo2 = dendo2_fixed, L = L)
      ss_best_dendo <- which.min(dendo1_cut_k_entanglements)
      current_best_dendo <- dendo1_k_rotated[[ss_best_dendo]]
      
      # if this loop's best dendro is not identical to our last best dendro - then we should pick it as the new best dendro
      #		And that means we'll have to update the dendrogram.heights.per.k (which takes time, and we would like to avoid if it is not necessary)
      if(!identical(current_best_dendo, best_dendo)) {
         best_dendo <- current_best_dendo
         best_dendo_heights_per_k <- dendrogram.heights.per.k(best_dendo) # since this function takes a looong time, I'm running it here so it will need to run only once!	
      }# however, if the current dendo is just like our best dendo - then there is NO NEED to update dendrogram.heights.per.k (and we just saved some time!!)
      # this combination is only useful if we have a tree for which there are only a few rotations which are useful
   }
   
   return(best_dendo)	
}

# system.time(identical(dendo1, dendo2))
# system.time(identical(dendo1, dendo1))
# identical(dendo1, dendo1)


untangle.backward.rotate.1side <- function(dendo1, dendo2_fixed , L = 1) {
   # this function gets two dendograms, and goes over each k splits of the first dendo1, and checks if the flip at level k of splitting imporves the entanglement between dendo1 and dendo2 (Which is fixed)
   require(plyr)
   leaves_values <- leaves.values(dendo1)
   best_dendo <- dendo1
   
   for(k in length(leaves_values):2) {
      dendo1_k_rotated <- all.couple.rotations.at.k(best_dendo, k)
      dendo1_cut_k_entanglements <- laply(dendo1_k_rotated, entanglement, dendo2 = dendo2_fixed, L = L)
      ss_best_dendo <- which.min(dendo1_cut_k_entanglements)
      best_dendo <- dendo1_k_rotated[[ss_best_dendo]]
   }
   
   return(best_dendo)	
}


# 
# untangle.forward.step.rotate.1side <- function(dendo1, dendo2_fixed) {
# 	# this function gets two dendograms, and goes over each k splits of the first dendo1, and checks if the flip at level k of splitting imporves the entanglement between dendo1 and dendo2 (Which is fixed)
# 	require(plyr)
# 	leaves_values <- leaves.values(dendo1)
# 	best_dendo <- dendo1
# 	
# 	k_visited <- rep(F, length(leaves_values))
# 	k_visited[1] <- T # I don't need the first one
# 	k <- 1
# 	
# 	while(!all(k_visited)) {
# 		# create all of the rotations with k+-1:
# 		dendo1_k_p1_rotated <- all.couple.rotations.at.k(best_dendo, k+1)
# 		dendo1_k_m1_rotated <- all.couple.rotations.at.k(best_dendo, k-1)
# 		# find the enteglement for all of them:
# 		dendo1_cut_k_p1_entanglements <- laply(dendo1_k_p1_rotated, entanglement, dendo2 = dendo2_fixed)
# 		dendo1_cut_k_m1_entanglements <- laply(dendo1_k_m1_rotated, entanglement, dendo2 = dendo2_fixed)
# 		# what is best, forward or backward?
# 		if(min(dendo1_cut_k_p1_entanglements) > min(dendo1_cut_k_m1_entanglements)) {
# 		
# 		}
# 		k <- k + 1
# 		ss_best_dendo <- which.min(dendo1_cut_k_entanglements)
# 		best_dendo <- dendo1_k_rotated[[ss_best_dendo]]		
# 		
# 		all.couple.rotations.at.k(best_dendo, -1)
# 	}
# 	
# 	return(best_dendo)	
# }


# dendo12s_1_better <- untangle.forward.rotate.1side(dendo1, dendo2)
# cutree(dendo1, 10)






untangle.forward.rotate.2side <- function(dendo1, dendo2, max_n_iterations = 10, output_times = T, L = 1) {
   # this function gets two dendograms, and orders dendo1 and 2 until a best entengelment is reached.
   
   
   # Next, let's try to improve upon this tree using a forwared rotation of our tree:
   dendo1_better <- untangle.forward.rotate.1side(dendo1, dendo2, L = L) 
   dendo2_better <- untangle.forward.rotate.1side(dendo2, dendo1_better, L = L) 
   
   entanglement_new <- entanglement(dendo1_better, dendo2_better, L = L) 
   entanglement_old <- entanglement_new+1
   
   times <- 1
   
   while(times < max_n_iterations & !identical(entanglement_new, entanglement_old)) { # if we got an improvement from last entaglement, we'll keep going!
      entanglement_old <- entanglement_new
      
      dendo1_better_loop <- untangle.forward.rotate.1side(dendo1_better, dendo2_better, L = L) 
      # if the new dendo1 is just like we just had - then we can stop the function since we found the best solution - else - continue
      if(identical(dendo1_better_loop, dendo1_better)) {
         break;
      } else {
         dendo1_better <- dendo1_better_loop
      }			 
      
      # if the new dendo2 is just like we just had - then we can stop the function since we found the best solution - else - continue
      dendo2_better_loop <- untangle.forward.rotate.1side(dendo2_better, dendo1_better, L = L) 
      if(identical(dendo2_better_loop, dendo2_better)) {
         break;
      } else {
         dendo2_better <- dendo2_better_loop
      }			 
      
      entanglement_new <- entanglement(dendo1_better, dendo2_better, L = L) 
      times <- times + 1
   }
   
   # identical(1,1+.00000000000000000000000001) # T
   if(output_times) cat("We ran untangle ", times, " times\n")
   
   return(list(dendo1 = dendo1_better, dendo2 = dendo2_better))	
}


# evolution algorithm
untangle.intercourse <- function(brother_1_dendo1, brother_1_dendo2, 
                                 sister_2_dendo1, 	sister_2_dendo2, L = 1) 
{
   # Gets two pairs of dendo, and returns two childrens (inside a list)
   children_1 <- untangle.forward.rotate.2side(brother_1_dendo1,sister_2_dendo2, L = L) 
   children_2 <- untangle.forward.rotate.2side(sister_2_dendo1,brother_1_dendo2, L = L) 
   
   list(children_1, children_2)
}

entanglement.return.best.brother <- function(brother_1_dendo1, brother_1_dendo2, 
                                             brother_2_dendo1, brother_2_dendo2, L = 1)
{
   # Gets two pairs of dendo, and returns the pair with the best (minimal) entanglement
   
   if( entanglement(brother_1_dendo1, brother_1_dendo2, L = L) <
          entanglement(brother_2_dendo1, brother_2_dendo2, L = L)  ) {
      return(list(brother_1_dendo1, brother_1_dendo2))
   } else {
      return(list(brother_2_dendo1, brother_2_dendo2))
   }
}

untangle.intercourse.evolution <- function(intercourse, L = 1) {
   # intercourse is a list with two elements.  Each element has two dendos
   entanglement.return.best.brother(intercourse[[1]][[1]], intercourse[[1]][[2]],
                                    intercourse[[2]][[1]], intercourse[[2]][[2]], L = L) 
   
   
}


untangle.evolution<- function(brother_1_dendo1, brother_1_dendo2, 
                              sister_2_dendo1, 	sister_2_dendo2, L = 1) 
{
   intercourse <- untangle.intercourse(brother_1_dendo1, brother_1_dendo2, 
                                       sister_2_dendo1, 	sister_2_dendo2, L = L)  # creates a list with two pairs of dendos
   untangle.intercourse.evolution(intercourse, L = L)  # returns the best child
}










####
# A new approuch - I will go through every possible flip on one side, and find the one that gives the best improvement.
# I will do the same on each tree, back and forth, until no better flip is found.

untangle.best.k.to.rotate.by.1side <- function(dendo1, dendo2_fixed, L = 1) {
   # this function gets two dendograms, and goes over each k splits of the first dendo1, and checks if the flip at level k of splitting imporves the entanglement between dendo1 and dendo2 (Which is fixed)
   require(plyr)
   leaves_values <- leaves.values(dendo1)
   best_dendo <- dendo1
   dendo1_k_rotated <- NULL
   
   best_dendo_heights_per_k <- dendrogram.heights.per.k(best_dendo) # since this function takes a looong time, I'm running it here so it will need to run only once!	
   # this makes the function about twice as fast... 
   
   for(k in 2:length(leaves_values)) {
      dendo1_k_rotated <- c(dendo1_k_rotated, 
                            all.couple.rotations.at.k(best_dendo, k, 
                                                      dendrogram_heights_per_k = best_dendo_heights_per_k))
   }
   
   dendo1_cut_k_entanglements <- laply(dendo1_k_rotated, entanglement, dendo2 = dendo2_fixed, L = L)
   ss_best_dendo <- which.min(dendo1_cut_k_entanglements)
   best_dendo <- dendo1_k_rotated[[ss_best_dendo]]
   return(best_dendo)	
}



flip.1.and.2 <- function(x) ifelse(x == 1, 2, 1)

untangle.best.k.to.rotate.by.2side.backNforth <- function(dendo1, dendo2, times_to_stop = 2, output_times = T, L = 1) {
   # this function gets two dendograms, and orders dendo1 and then 2 and then 1 again - back and forth -until a best entengelment is reached.
   
   was_improved <- T # e.g: we can improve it further
   counter <- 1
   
   while(was_improved) {
      entanglement_old <- entanglement(dendo1, dendo2, L = L) 
      dendo1 <- untangle.best.k.to.rotate.by.1side(dendo1, dendo2, L = L) 
      dendo2 <- untangle.best.k.to.rotate.by.1side(dendo2, dendo1, L = L) 
      entanglement_new <- entanglement(dendo1, dendo2, L = L) 
      was_improved <- identical(entanglement_old, entanglement_new)
      counter <- counter + 1
   }
   # identical(1,1+.00000000000000000000000001) # T
   if(output_times) cat("We ran untangle.best.k.to.rotate.by.2side.backNforth ", counter, " times")
   
   return(list(dendo1 = dendo1, dendo2 = dendo2))	
}




if(F) {
   # example
   dist_DATA <- dist(USArrests[1:20,])
   # First two dummy clusters (since you didn't provide with some...)
   hc1 <- hclust(dist_DATA , "single")
   hc2 <- hclust(dist_DATA , "complete")
   dendo1 <- as.dendrogram(hc1)
   dendo2 <- as.dendrogram(hc2)	
   entanglement(dendo1, dendo2) 		
   
   system.time(dendo12_best_01 <- untangle.forward.rotate.2side(dendo1, dendo2, L = 2)) # 0.47 sec
   system.time(dendo12_best_02 <- untangle.best.k.to.rotate.by.2side.backNforth(dendo1, dendo2, L = 2)) # 0.44 sec
   tanglegram(dendo1, dendo2) 
   tanglegram(dendo12_best_01[[1]], dendo12_best_01[[2]]) 
   tanglegram(dendo12_best_02[[1]], dendo12_best_02[[2]]) 
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
   dendo1 <- as.dendrogram(hc1)
   dendo2 <- as.dendrogram(hc2)
   
   tanglegram(dendo1, dendo2) 
   entanglement(dendo1, dendo2) # 0.8
   
   # after sorting we get a better measure of entanglement and also a better looking plot
   tanglegram(sort(dendo1), sort(dendo2))
   entanglement(sort(dendo1), sort(dendo2)) # 0.1818
   
   # let's cause some stir... (e.g: mix the dendrogram, and see how that effects the outcome)
   set.seed(134)
   s_dendo1 <- stir(dendo1)
   s_dendo2 <- stir(dendo2)
   tanglegram(s_dendo1, s_dendo2)
   entanglement(s_dendo1, s_dendo2) # 0.7515
   
   
   set.seed(1234)
   dendo12s <- untangle.random.search(dendo1, dendo2, R = 10)
   entanglement(dendo12s[[1]], dendo12s[[2]]) # 0.042
   tanglegram(dendo12s[[1]], dendo12s[[2]]) # 
   # this is a case where it is CLEAR that the simplest heuristic would improve this to 0 entanglement...	
   
   # let's see if we can reach a good solution using a greedy forward selection algorithm
   dendo12s_1_better <- untangle.forward.rotate.1side(dendo12s[[1]], dendo12s[[2]])
   entanglement(dendo12s_1_better, dendo12s[[2]]) # from 0.042 to 0.006 !!
   tanglegram(dendo12s_1_better, dendo12s[[2]]) # 
   
   # let's see from the beginning
   entanglement(dendo1, dendo2) # 0.6
   tanglegram(dendo1, dendo2) # 0.6
   dendo12s_1_better <- untangle.forward.rotate.1side(dendo1, dendo2)
   entanglement(dendo12s_1_better, dendo2) # from 0.6 to 0.036
   tanglegram(dendo12s_1_better, dendo2) # 
   # let's try the other side:
   dendo12s_2_better <- untangle.forward.rotate.1side(dendo2, dendo12s_1_better)
   entanglement(dendo12s_1_better, dendo12s_2_better) # no improvment
   
   
   
   dendo2_01 <- untangle.forward.rotate.1side(dendo2, dendo1)
   dendo2_02 <- untangle.backward.rotate.1side(dendo2, dendo1)
   dendo2_03 <- untangle.backward.rotate.1side(dendo2_01, dendo1)
   dendo2_04 <- untangle.forward.rotate.1side(dendo2_02, dendo1)
   dendo2_05 <- untangle.evolution(dendo1, dendo2 , dendo1, dendo2_01 )
   entanglement(dendo1, dendo2) 
   entanglement(dendo1, dendo2_01) 
   
   entanglement(dendo1, dendo2_02) 
   entanglement(dendo1, dendo2_03) 
   entanglement(dendo1, dendo2_04) 
   entanglement(dendo2_05[[1]], dendo2_05[[2]]) 
   tanglegram(dendo1, dendo2) 
   tanglegram(dendo1, dendo2_01) 
   tanglegram(dendo1, dendo2_02) 
   tanglegram(dendo1, dendo2_03) 
   tanglegram(dendo1, dendo2_04) 
   tanglegram(dendo2_05[[1]], dendo2_05[[2]]) 
   
   
   
   entanglement(dendo1, dendo2) 
   tanglegram(dendo1, dendo2) 
   dendo2_01 <- untangle.forward.rotate.1side(dendo2, dendo1)
   dendo2_01 <- untangle.backward.rotate.1side(dendo2, dendo1)
   tanglegram(dendo1, dendo2_01) 
   
   
   
   # 
   dist_DATA <- dist(USArrests[1:10,])
   # First two dummy clusters (since you didn't provide with some...)
   hc1 <- hclust(dist_DATA , "single")
   hc2 <- hclust(dist_DATA , "complete")
   dendo1 <- as.dendrogram(hc1)
   dendo2 <- as.dendrogram(hc2)
   dendo1_01 <- untangle.forward.rotate.1side(dendo1, dendo2)
   entanglement(dendo1, dendo2) 
   entanglement(dendo1_01, dendo2) 
   tanglegram(dendo1, dendo2) 
   tanglegram(dendo1_01, dendo2) 
   
   system.time(dendo1_01 <- untangle.forward.rotate.1side(dendo1, dendo2)) # 0.47 sec
   system.time(dendo1_01 <- untangle.best.k.to.rotate.by(dendo1, dendo2)) # 0.44 sec
   tanglegram(dendo1, dendo2) 
   tanglegram(dendo1_01, dendo2) 
   
   
   
   
   #### profiling
   require(profr)
   slow_dude <- profr(untangle.forward.rotate.1side(dendo2, dendo1))
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
   
   system.time(entanglement(dendo1, dendo2) 	) # 0.01
   microbenchmark( entanglement(dendo1, dendo2) , times = 10 )# so this is 10 times faster (the medians)
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




