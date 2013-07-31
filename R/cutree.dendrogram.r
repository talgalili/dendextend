

#' @title Check if numbers are natural
#' @export
#' @description Vectorized function for checking if numbers are natural or not.
#' Helps in checking if a vector is of type "order".
#' @param x a vector of numbers
#' @param tol tolerence to floating point issues.
#' @param ... (not currently in use)
#' @return logical - is the entered number natural or not.
#' @author Marco Gallotta (a.k.a: marcog), Tal Galili
#' @source 
#' This function was written by marcog, as an answer to my question here:
#' \url{http://stackoverflow.com/questions/4562257/what-is-the-fastest-way-to-check-if-a-number-is-a-positive-natural-number-in-r}
#' @seealso \code{\link{is.numeric}}, \code{\link{is.double}}, \code{\link{is.integer}}
#' @examples
#' is.natural.number(1) # is TRUE
#' (x <- seq(-1,5, by=0.5) )
#' is.natural.number( x )
#' # is.natural.number( "a" )
#' all(is.natural.number( x ))
#' 
is.natural.number <- function(x, tol = .Machine$double.eps^0.5, ...) {
   x > tol & abs(x - round(x)) < tol 
} 

## Not important enough to include
# all.natural.numbers <- function(x) all(is.natural.number(x))   # check if all the numbers in a vector are natural
# why is this important?
# because it can enable one to check if what we have is a vector of "order"






#' @title cutree for dendrogram (by 1 height only!)
#' @export
#' @description Cuts a dendrogram tree into several groups 
#' by specifying the desired cut height (only a single height!).
#' @param tree   a dendrogram object
#' @param h    numeric scalar (NOT a vector) with a height where the tree should be cut.
#' @param use_labels_not_values logical, defaults to TRUE. If the actual labels of the 
#' clusters do not matter - and we want to gain speed (say, 10 times faster) - 
#' then use FALSE (gives the "leaves order" instead of their labels.).
#' @param order_clusters_as_data logical, defaults to TRUE. There are two ways by which 
#' to order the clusters: 1) By the order of the original data. 2) by the order of the 
#' labels in the dendrogram. In order to be consistent with \link[stats]{cutree}, this is set
#' to TRUE.
#' @param ... (not currently in use)
#' @return \code{cutree_1h.dendrogram} returns an integer vector with group memberships 
#' @author Tal Galili
#' @seealso \code{\link{hclust}}, \code{\link{cutree}}
#' @examples
#' hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
#' dend <- as.dendrogram(hc)
#' cutree(hc, h=50) # on hclust
#' cutree_1h.dendrogram(dend, h=50) # on a dendrogram
#' 
#' labels(dend)
#' 
#' # the default (ordered by original data's order)
#' cutree_1h.dendrogram(dend, h=50, order_clusters_as_data = TRUE) 
#' 
#' # A different order of labels - order by their order in the tree
#' cutree_1h.dendrogram(dend, h=50, order_clusters_as_data = FALSE) 
#' 
#' 
#' # make it faster
#' \dontrun{
#' require(microbenchmark)
#' microbenchmark(
#'          cutree_1h.dendrogram(dend, h=50),
#'          cutree_1h.dendrogram(dend, h=50,use_labels_not_values = FALSE)
#'          )
#'          # 0.8 vs 0.6 sec - for 100 runs
#' }
#' 
#' 
cutree_1h.dendrogram <- function(tree, h, order_clusters_as_data = TRUE, use_labels_not_values = TRUE,...)
{
   
   if(missing(h)) stop("h is missing")   
   
   if(length(h) > 1) {
      warning("h has length > 1 and only the first element will be used")
      h <- h[1]
   }
   
   if(use_labels_not_values) {
      names_in_clusters <- sapply(cut(tree, h = h)$lower, labels)   # a list with names per cluster
   } else {
      names_in_clusters <- sapply(cut(tree, h = h)$lower, order.dendrogram)	# If the proper labels are not important, this function is around 10 times faster than using labels (so it is much better for some other algorithms)
   }
   
   number_of_clusters <- length(names_in_clusters)
   number_of_members_in_clusters <-sapply(names_in_clusters, length) # a list with item per cluster. each item is a character vector with the names of the items in that cluster
   cluster_vec <- rep(rev(seq_len(number_of_clusters)), times = number_of_members_in_clusters ) # like in the original cutree
   # I am using "rev" on "seq_len" - so that the resulting cluster numbers will be consistant with those of cutree.hclust
   
   # 2011-01-10: this is to fix the "bug" (I don't think it's a feature) of having the cut.dendrogram return splitted tree when h is heigher then the tree...
   # now it gives consistent results with cutree
   if(h > attr(tree, "height")) cluster_vec <- rep(1L, length(cluster_vec))	
   
   names(cluster_vec) <- unlist(names_in_clusters)
   
   
   # note: The order of the items in cluster_vec, is according to their order in the dendrogram.
   # If the dendrogram was created through as.dendrogram(hclust_object)
   # The original order of the names of the items, from which the hclust (and the dendrogram) object was created from, will not be preserved!
   
   clusters_order <- order.dendrogram(tree)
   
   if(order_clusters_as_data) 
   {
      if(!all(clusters_order %in% seq_along(clusters_order))){
         warning("rank() was used for the leaves order number! \nExplenation: leaves tip number (the order), and the ranks of these numbers - are not equal.  The tree was probably trimmed and/or merged with other trees- and now the order labels don't make so much sense (hence, the rank on them was used.")
         warning("Here is the cluster order vector (from the tree tips) \n", clusters_order, "\n")
         clusters_order <- rank(clusters_order, ties.method = "first")   # we use the "first" ties method - to handle the cases of ties in the ranks (after splits/merges with other trees)
      }
      
      cluster_vec <- cluster_vec[order(clusters_order)]	# this reorders the cluster_vec according to the original order of the items from which the tree (maybe hclust) was created
   }   
   
   # 2013-07-28: stay consistant with hclust:
   # if we have as many clusters as items - they should be numbered
   # from left to right...
   tree_size <- nleaves(tree)
   if(number_of_clusters == tree_size) cluster_vec[seq_len(tree_size)] <- seq_len(tree_size)
   
   return(cluster_vec)
}











#' @title Which height will result in which k for a dendrogram
#' @export
#' @param tree a dendrogram.
#' @param ... not used.
#' @return a vector of heights, with its names being the k clusters that will
#' result for cutting the dendrogram at each height.
#' 
#' @examples
#' \donotrun{
#' hc <- hclust(dist(USArrests[1:4,]), "ave")
#' dend <- as.dendrogram(hc)
#' heights_per_k.dendrogram(dend)
#' ##       1        2        3        4 
#' ##86.47086 68.84745 45.98871 28.36531 
#' 
#' cutree(hc, h = 68.8) # and indeed we get 2 clusters
#' 
#' unroot_dend <- unroot(dend,2)
#' plot(unroot_dend)
#' heights_per_k.dendrogram(unroot_dend)
#'        #1        3        4 
#'        #97.90023 57.41808 16.93594 
#'        # we do NOT have a height for k=2 because of the tree's structure.
#' }
heights_per_k.dendrogram <- function(tree,...)
{
   # gets a dendro tree
   # returns a vector of heights, and the k clusters we'll get for each of them.
   
   our_dend_heights <- sort(unique(get_branches_heights(tree)), TRUE)
   
   heights_to_remove_for_A_cut <- min(-diff(our_dend_heights))/2 # the height to add so to be sure we get a "clear" cut
   heights_to_cut_by <- c((max(our_dend_heights) + heights_to_remove_for_A_cut),	# adding the height for 1 clusters only (this is not mandetory and could be different or removed)
                          (our_dend_heights - heights_to_remove_for_A_cut))
   # 	names(heights_to_cut_by) <- sapply(heights_to_cut_by, function(h) {length(cut(tree, h = h)$lower)}) # this is the SLOW line - I need to do it differently...
   names(heights_to_cut_by) <- sapply(heights_to_cut_by, function(h) {length(cut(tree, h = h)$lower)}) # this is the SLOW line - I need to do it differently...
   names(heights_to_cut_by)[1] <- "1" # should always be 1. (the fact that it's currently not is a bug - remove this line once it is fixed)
   return(heights_to_cut_by)
   # notice we might have certion k's that won't exist in this list!
}






#' @title cutree for dendrogram (by 1 k value only!)
#' @export
#' @description Cuts a dendrogram tree into several groups 
#' by specifying the desired number of clusters k (only a single k value!).
#' 
#' In case there exists no such k for which exists a relevant split of the 
#' dendrogram, a warning is issued to the user, and NULL is returned.
#' @param tree   a dendrogram object
#' @param k    numeric scalar (not a vector!) with the number of clusters
#' the tree should be cut into.
#' @param dend_heights_per_k a named vector that resulted from running.
#' \code{heights_per_k.dendrogram}. When running the function many times,
#' supplying this object will help improve the running time.
#' @param use_labels_not_values logical, defaults to TRUE. If the actual labels of the 
#' clusters do not matter - and we want to gain speed (say, 10 times faster) - 
#' then use FALSE (gives the "leaves order" instead of their labels.).
#' This is passed to \code{cutree_1h.dendrogram}.
#' @param order_clusters_as_data logical, defaults to TRUE. There are two ways by which 
#' to order the clusters: 1) By the order of the original data. 2) by the order of the 
#' labels in the dendrogram. In order to be consistent with \link[stats]{cutree}, this is set
#' to TRUE.
#' This is passed to \code{cutree_1h.dendrogram}.
#' @param warn logical. Should the function send a warning in case the desried 
#' k is not available? (deafult is TRUE)
#' @param ... (not currently in use)
#' @return \code{cutree_1k.dendrogram} returns an integer vector with group 
#' memberships.
#' 
#' In case there exists no such k for which exists a relevant split of the 
#' dendrogram, a warning is issued to the user, and NULL is returned.
#' @author Tal Galili
#' @seealso \code{\link{hclust}}, \code{\link{cutree}}, 
#' \code{\link{cutree_1h.dendrogram}}
#' @examples
#' hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
#' dend <- as.dendrogram(hc)
#' cutree(hc, k=3) # on hclust
#' cutree_1k.dendrogram(dend, k=3) # on a dendrogram
#' 
#' labels(dend)
#' 
#' # the default (ordered by original data's order)
#' cutree_1k.dendrogram(dend, k=3, order_clusters_as_data = TRUE) 
#' 
#' # A different order of labels - order by their order in the tree
#' cutree_1k.dendrogram(dend, k=3, order_clusters_as_data = FALSE) 
#' 
#' 
#' # make it faster
#' \dontrun{
#' require(microbenchmark)
#' dend_ks <- heights_per_k.dendrogram
#' microbenchmark(
#'          cutree_1k.dendrogram = cutree_1k.dendrogram(dend, k=4),
#'          cutree_1k.dendrogram_no_labels = cutree_1k.dendrogram(dend, k=4,use_labels_not_values = FALSE),
#'          cutree_1k.dendrogram_no_labels_per_k = cutree_1k.dendrogram(dend, k=4,
#'                                              use_labels_not_values = FALSE,
#'                                              dend_heights_per_k = dend_ks)
#'          )
#'          # the last one is the fastest...
#' }
#' 
#' 
cutree_1k.dendrogram <- function(tree, k, dend_heights_per_k, 
                                 use_labels_not_values = TRUE, 
                                 order_clusters_as_data =TRUE, 
                                 warn = TRUE, ...)
{
   
   # step 1: find all possible h cuts for tree	
   if(missing(dend_heights_per_k)) {
      # since this is a step which takes a long time, If possible, I'd rather supply this to the function, so to make sure it runs faster...
      dend_heights_per_k <- heights_per_k.dendrogram(tree)
   }
   
   
   # step 2: Check location in the vector of the height for the k we are interested in	
   height_for_our_k <- which(names(dend_heights_per_k) == k)
   if(length(height_for_our_k) != 0)  # if such a height exists
   {
      h_to_use <- dend_heights_per_k[height_for_our_k]
      cluster_vec <- cutree_1h.dendrogram(tree, 
                                          h = h_to_use, 
                                          use_labels_not_values = use_labels_not_values, 
                                          order_clusters_as_data = order_clusters_as_data,
                                          ...)
#       if(to_print) print(paste("The dendrogram was cut at height", 
#                                round(h_to_use, 4), "in order to create",k, "clusters."))
   } else {
      cluster_vec <- NULL
      
      # telling the user way he can't use this k
      if(warn) {
         if(k > max(as.numeric(names(dend_heights_per_k))) || k < min(as.numeric(names(dend_heights_per_k)))) {
            range_for_clusters <- paste("[",  paste(range(names(dend_heights_per_k)), collapse = "-"),"]", sep = "") # it's always supposed to be between 1 to max number of items (so this could be computed in more efficient ways)
            warning(paste("No cut exists for creating", k, "clusters.  The possible range for clusters is:", range_for_clusters))
         }
         if( !identical(round(k), k) || k < min(as.numeric(names(dend_heights_per_k))))   {				
            warning(paste("k must be a natural number.  The k you used ("  ,k, ") is not a natural number"))
         } else {
            warning(paste("You (probably) have some branches with equal heights so that there exist no height(h) that can create",k," clusters"))
         }
      }
   }
   return(cluster_vec)
}




# this allows the making of cutree.dendrogram into a method :)
cutree <- function(tree, k = NULL, h = NULL,...)  UseMethod("cutree")


#' @export
cutree.default <- function(tree, k = NULL, h = NULL,...)  stop("Function cutree is only available for hclust and dendrogram objects.")

#' @export
#' @S3method cutree hclust
cutree.hclust <- stats:::cutree


#' @export
#' @S3method cutree dendrogram
cutree.dendrogram <- function(tree, k = NULL, h = NULL,...)
{
   # TODO:
   # cutree.dendrogram might benefit from using 
   # tryCatch(stop(), error = function(e) TRUE)
   # tryCatch(stop(), error = function(e) TRUE, finally = FALSE)
   # tryCatch(is.hclust(hc_dend <- as.hclust(dend)), error = function(e) FALSE)
   # and if it works to use cutree.hclust
   # this would be faster, especially when using k.
   # and if it doesn't, one could use my functions
   #     also, I should add a "force" parameter - to force using my function (for weird cases...)
   
   # tree   a dendrogram object
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
   
   if(!is.null(k)) cluster_vec <- cutree_1k.dendrogram(tree, k,...)
   
   # What to do in case h is supplied
   if(!is.null(h)) cluster_vec <- cutree_1h.dendrogram(tree, h,...)
   
   return(cluster_vec)
}















### TODO:
### possible functions to add:
# ultrametric
# is.ultrametric(as.phylo(as.hclust(dend)))
# is.ultrametric(as.phylo(as.hclust(hang.dendrogram(dend))))
# plot(as.phylo(as.hclust(hang.dendrogram(dend))))
# is.ultrametric(as.phylo(as.hclust(dend, hang = 2)))
# is.binary.tree






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
# is.ultrametric(as.phylo(dhc))
# cutree.dendrogram(dhc, k = 4) # handaling the case were k is not a viable number of clusters
# cutree.dendrogram(dhc, k = 3.2) # handaling the case were k is not a viable number of clusters


# heights_per_k.dendrogram(dhc)
