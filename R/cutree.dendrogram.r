# cutree.dendrogram.h(tree, h = h_to_use)

# needed functions
# followed by some short examples

is.natural.number <- function(x, tol = .Machine$double.eps^0.5)  x > tol & abs(x - round(x)) < tol
# Thanks to marcog
# http://stackoverflow.com/questions/4562257/what-is-the-fastest-way-to-check-if-a-number-is-a-positive-natural-number-in-r
## example:
# is.naturalnumber(1) # is TRUE
# (x <- seq(-1,5, by=0.5) )
# is.natural.number( x )
all.natural.numbers <- function(x) all(is.natural.number(x))   # check if all the numbers in a vector are natural
# why is this important?
# because it can enable one to check if what we have is a vector of "order"


# cut(tree, h = 330)
# cutree.dendrogram.h(tree, h = 100)
# cut(tree, h = 100)
# plot(tree)

cutree.dendrogram.h <- function(tree, h, order_clusters_using_tree = F, to_print = F, use_labels_not_values = T)
{
   # tree	a dendrogram object
   # h	 numeric scalar or vector with heights where the tree should be cut.
   # use use_labels_not_values = F, if the actual labels of the clusters do not matter - and we want to gain speed (say, 10 times faster)
   # 				use use_labels_not_values = F gives the leaves values instead of their labels.
   
   if(use_labels_not_values) {
      names_in_clusters <- sapply(cut(tree, h = h)$lower, labels)	# a list with names per cluster
   } else {
      names_in_clusters <- sapply(cut(tree, h = h)$lower, leaves.values)	# If the proper labels are not important, this function is around 10 times faster than using labels (so it is much better for some other algorithms)
   }
   
   number_of_clusters <- length(names_in_clusters)
   number_of_members_in_clusters <-sapply(names_in_clusters, length) # a list with item per cluster. each item is a character vector with the names of the items in that cluster
   cluster_vec <- rep(rev(seq_len(number_of_clusters)), times = number_of_members_in_clusters ) # like and in the original cutree
   # I am using "rev" on "seq_len" - so that the resulting cluster numbers will be consistant with those of cutree.hclust
   if(h > attr(tree, "height")) cluster_vec <- rep(1, length(cluster_vec))	# 10.01.11: this is to fix the "bug" (I don't think it's a feature) of having the cut.dendrogram return splitted tree when h is heigher then the tree...
   names(cluster_vec) <- unlist(names_in_clusters)
   
   
   # note: The order of the items in cluster_vec, is according to their order in the dendrogram.
   # If the dendrogram was created through as.dendrogram(hclust_object)
   # The original order of the names of the items, from which the hclust (and the dendrogram) object was created from, will not be preserved!
   
   
   if(order_clusters_using_tree) 
   {
      # clusters_order <- rapply(tree, function(x) {x})	# this was the old way, but it misses one thing:
      # 1) sometimes the tree has gotten some leaves chopped, in which case, we need to rank the leaves order (since their range may not be relevant anymore...
      clusters_order <- unlist(tree) # rapply(tree, function(x) {x})  # previously we used the rapply method
      ranked_clusters_order <- rank(clusters_order, ties.method = "first")	# we use the "first" ties method - to handle the cases of ties in the ranks (after splits/merges with other trees)
      
      if(!all.natural.numbers(clusters_order)) {
         warning("The numbers in the tips of the tree are not 'natural numbers'!  they were used for ordering the labels, but this ordering may be false!")
         # cat("Here is the cluster order vector (from the tree tips) \n", clusters_order, "\n")	# If the numbers aren't natural - this will be printed in the next warning section...
      }
      if(!all(ranked_clusters_order == clusters_order)) {
         warning("rank() was used for the leaves order number.  Explenation: leaves tip number (the order), and the ranks of these numbers - are not equal.  The tree was probably trimmed and/or merged with other trees- and now the order labels don't make so much sense (hence, the rank on them was used.")
         if(to_print) cat("Here is the cluster order vector (from the tree tips) \n", clusters_order, "\n")
      }		
      
      cluster_vec <- cluster_vec[order(clusters_order)]	# this reorders the cluster_vec according to the original order of the items from which the tree (maybe hclust) was created
   } else { 
      # warning("Can not order tree, since tree leaves don't seem to include information on trees items order (e.g: They are not a vector of type 1:number_of_items)")
   }
   
   
   return(cluster_vec)
}

rllply <- function(X, FUN,...)
{	# recursivly apply a function on a list - and returns the output as a list # following the naming convention in the {plyr} package
   # the big difference between this and rapply is that this will also apply the function on each element of the list, even if it's not a "terminal node" inside the list tree
   # an attribute is added to indicate if the value returned is from a branch or a leaf
   if(is.list(X)) {
      output <- list()
      for(i in seq_len(length(X)))
      {		
         output[[i]] <- list(rllply(X[[i]], FUN,...))
         attr(output[[i]][[1]], "position_type") <- "Branch"
      }
      output <- list(FUN(X,...), output)
   } else {
      output <- FUN(X,...)
      attr(output, "position_type") <- "Leaf"	
   }
   
   # for(i in seq_len(length(X)))
   # {
   # if(is.list(X[[i]])) {
   # output[[i]] <- list(FUN(X[[i]],...),rllply(X[[i]], FUN,...))
   # attr(output[[i]][[1]], "position_type") <- "Branch"
   # } else {
   # output[[i]] <- FUN(X[[i]],...)
   # attr(output[[i]], "position_type") <- "Leaf"
   # }
   # }
   return(output)
}

get.dendrogram.heights <- function(tree)
{
   height <- unlist(rllply(tree, function(x){attr(x, "height")}))
   height <- height[height != 0] # include only the non zero values
   height <- sort(height) 	# sort the height
   return(height)
}

dendrogram.heights.per.k <- function(tree)
{
   # gets a dendro tree
   # returns a vector of heights, and the k clusters we'll get for each of them.
   
   our_dendrogram_heights <- sort(unique(get.dendrogram.heights(tree)), T)
   
   heights_to_remove_for_A_cut <- min(-diff(our_dendrogram_heights))/2 # the height to add so to be sure we get a "clear" cut
   heights_to_cut_by <- c((max(our_dendrogram_heights) + heights_to_remove_for_A_cut),	# adding the height for 1 clusters only (this is not mandetory and could be different or removed)
                          (our_dendrogram_heights - heights_to_remove_for_A_cut))
   # 	names(heights_to_cut_by) <- sapply(heights_to_cut_by, function(h) {length(cut(tree, h = h)$lower)}) # this is the SLOW line - I need to do it differently...
   names(heights_to_cut_by) <- sapply(heights_to_cut_by, function(h) {length(cut(tree, h = h)$lower)}) # this is the SLOW line - I need to do it differently...
   names(heights_to_cut_by)[1] <- "1" # should always be 1. (the fact that it's currently not is a bug - remove this line once it is fixed)
   return(heights_to_cut_by)
   # notice we might have certion k's that won't exist in this list!
}



# Play with:


# cutree.dendrogram.h(tree, h = h,use_labels_not_values=F)
# cutree.dendrogram.k(tree, 4)
# cutree.dendrogram.k(tree, 4, use_labels_not_values=F)
# 										h = h,use_labels_not_values=F)

cutree.dendrogram.k <- function(tree, k, to_print = F, dendrogram_heights_per_k, use_labels_not_values = T,  ...)
{
   # tree	a dendrogram object
   # k	 an integer scalar or vector with the desired number of groups
   
   # step 1: find all possible h cuts for tree	
   if(missing(dendrogram_heights_per_k)) {
      # since this is a step which takes a long time, If possible, I'd rather supply this to the function, so to make sure it runs faster...
      dendrogram_heights_per_k <- dendrogram.heights.per.k(tree)
   }
   
   
   # step 2: Check location in the vector of the height for the k we are interested in	
   height_for_our_k <- which(names(dendrogram_heights_per_k) == k)
   if(length(height_for_our_k) != 0)  # if such a height exists
   {
      h_to_use <- dendrogram_heights_per_k[height_for_our_k]
      cluster_vec <- cutree.dendrogram.h(tree, h = h_to_use, use_labels_not_values = use_labels_not_values, ...)
      if(to_print) print(paste("The dendrogram was cut at height", round(h_to_use, 4), "in order to create",k, "clusters."))
   } else {
      cluster_vec <- NULL
      
      # telling the user way he can't use this k
      if(k > max(as.numeric(names(dendrogram_heights_per_k))) || k < min(as.numeric(names(dendrogram_heights_per_k))))
      {
         range_for_clusters <- paste("[",  paste(range(names(dendrogram_heights_per_k)), collapse = "-"),"]", sep = "") # it's always supposed to be between 1 to max number of items (so this could be computed in more efficient ways)
         warning(paste("No cut exists for creating", k, "clusters.  The possible range for clusters is:", range_for_clusters))
      }
      if( !identical(round(k), k) || k < min(as.numeric(names(dendrogram_heights_per_k))))
      {				
         warning(paste("k must be a natural number.  The k you used ("  ,k, ") is not a natural number"))
      } else {
         warning(paste("You (probably) have some branches with equal heights so that there exist no height(h) that can create",k," clusters"))
      }
   }
   return(cluster_vec)
}

cutree.dendrogram <- function(tree, k = NULL, h = NULL,...)
{
   # tree	a dendrogram object
   # k	 an integer scalar or vector with the desired number of groups
   # h	 numeric scalar or vector with heights where the tree should be cut.
   # use_labels_not_values - if F, the resulting clusters will not have their lables (but instead, they will have tree values), however, the function will be about 10 times faster.  So if the labels are not useful, this is a good parameter to use.
   
   # warnings and stopping rules:
   if(class(tree) !="dendrogram") warning("tree object is not of class dendrogram - this function might not work properly")
   if(is.null(k) && is.null(h)) stop("Neither k nor h were specified")
   if(!is.null(k) && !is.null(h)) {
      warning("Both k and h were specified - using h as default (consider using only h or k in order to avoid confusions)")
      k <- NULL
   }
   
   if(!is.null(k)) cluster_vec <- cutree.dendrogram.k(tree, k,...)
   
   # What to do in case h is supplied
   if(!is.null(h)) cluster_vec <- cutree.dendrogram.h(tree, h,...)
   
   return(cluster_vec)
}


# this allows the making of cutree.dendrogram into a method :)
cutree <- function(tree, k = NULL, h = NULL,...)  UseMethod("cutree")

cutree.default <- function (tree, k = NULL, h = NULL,...) 
{
   if (is.null(n1 <- nrow(tree$merge)) || n1 < 1) 
      stop("invalid 'tree' (merge component)")
   n <- n1 + 1
   if (is.null(k) && is.null(h)) 
      stop("either 'k' or 'h' must be specified")
   if (is.null(k)) {
      if (is.unsorted(tree$height)) 
         stop("the 'height' component of 'tree' is not sorted\n(increasingly); consider applying as.hclust() first")
      k <- integer(length(h))
      k <- n + 1 - apply(outer(c(tree$height, Inf), h, ">"), 
                         2, which.max)
      if (getOption("verbose")) 
         message("cutree(): k(h) = ", k, domain = NA)
   }
   else {
      k <- as.integer(k)
      if (min(k) < 1 || max(k) > n) 
         stop(gettextf("elements of 'k' must be between 1 and %d", 
                       n), domain = NA)
   }
   ans <- .Call("R_cutree", tree$merge, k, PACKAGE = "stats")
   if (length(k) == 1L) {
      ans <- as.vector(ans)
      names(ans) <- tree$labels
   }
   else {
      colnames(ans) <- if (!is.null(h)) 
         h
      else k
      rownames(ans) <- tree$labels
   }
   return(ans)
}




## ----------------------
## examples:
# hc <- hclust(dist(USArrests[c(1:3,7,5),]), "ave")
# dhc <- as.dendrogram(hc)
# str(dhc)
# plot(hc)

# cutree(dhc, h = 50)
# cutree.dendrogram(dhc, h = 50)
# cutree.dendrogram(dhc, k = 3) # same output
# cutree.dendrogram(dhc, k = 3,h = 50) # conflicting options - using h as default
# cutree.dendrogram(dhc, k = 10) # handaling the case were k is not a viable number of clusters

## showing another case were k is not an option
# attr(dhc[[2]][[1]], "height") <- 23.2
# attr(dhc[[2]][[2]], "height") <- 23.2
# plot(dhc)
# cutree.dendrogram(dhc, k = 4) # handaling the case were k is not a viable number of clusters
# cutree.dendrogram(dhc, k = 3.2) # handaling the case were k is not a viable number of clusters


# dendrogram.heights.per.k(dhc)

