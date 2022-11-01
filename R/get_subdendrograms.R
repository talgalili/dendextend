#' @title Extract a list of \emph{k} subdendrograms from a given dendrogram
#' object
#' @export
#' @description
#' Extracts a list (\link{dendlist}) of subdendrogram structures based on the cutree \code{\link{cutree.dendrogram}} function
#' from a given dendrogram object. It can be useful in case we're interested in a visual investigation of 
#' specific clustering results.
#' @param dend a dendrogram object
#' @param k the number of subdendrograms that should be extracted
#' @param order_clusters_as_data passed to \link[dendextend]{cutree}, default is FALSE 
#' (while the cutree default is TRUE). The reason is since it's easier to look at the dendrogram plot
#' and then get subtrees that are in the same order is in the plot/dendrogram object.
#' This is in contrast to more traditional use of cutree, where it is used with the original order or rows from the data.
#' @param ... parameters that should be passed to the cutree
#' \code{\link{cutree.dendrogram}}
#' @return
#' A list of \emph{k} subdendrograms, based on the cutree
#' \code{\link{cutree.dendrogram}} clustering
#' clusters.
#' @examples
#'
#' # needed packages:
#' # install.packages(gplots)
#' # install.packages(viridis)
#' # install.packages(devtools)
#' # devtools::install_github('talgalili/dendextend') #' dendextend from github
#' 
#' # define dendrogram object to play with:
#' dend <- iris[1:20, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   # set("labels_to_character") %>%
#'   color_branches(k = 5)
#' labels(dend) <- letters[1:20]
#' plot(dend)
#' dend_list <- get_subdendrograms(dend, 5)
#' lapply(dend_list, labels)
#' # [[1]]
#' # [1] "a" "b"
#' # 
#' # [[2]]
#' # [1] "c" "d" "e" "f" "g"
#' # 
#' # [[3]]
#' # [1] "h" "i"
#' # 
#' # [[4]]
#' # [1] "j" "k" "l" "m"
#' # 
#' # [[5]]
#' # [1] "n" "o" "p" "q" "r" "s" "t"
#' 
#' # define dendrogram object to play with:
#' dend <- iris[, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("labels_to_character") %>%
#'   color_branches(k = 5)
#' dend_list <- get_subdendrograms(dend, 5)
#'
#' # Plotting the result
#' par(mfrow = c(2, 3))
#' plot(dend, main = "Original dendrogram")
#' sapply(dend_list, plot)
#'
#' # plot a heatmap of only one of the sub dendrograms
#' par(mfrow = c(1, 1))
#' library(gplots)
#' sub_dend <- dend_list[[1]] #' get the sub dendrogram
#' # make sure of the size of the dend
#' nleaves(sub_dend)
#' length(order.dendrogram(sub_dend))
#' # get the subset of the data
#' subset_iris <- as.matrix(iris[order.dendrogram(sub_dend), -5])
#' # update the dendrogram's internal order so to not cause an error in heatmap.2
#' order.dendrogram(sub_dend) <- as.integer(rank(order.dendrogram(sub_dend)))
#' heatmap.2(subset_iris, Rowv = sub_dend, trace = "none", col = viridis::viridis(100))
get_subdendrograms <- function(dend, k, order_clusters_as_data = FALSE, ...) {
  clusters <- cutree(dend, k, order_clusters_as_data = order_clusters_as_data, ...)
  dend_list <- lapply(unique(clusters), function(cluster.id) {
    find_dendrogram(dend, names(which(clusters == cluster.id)))
  })
  class(dend_list) <- "dendlist"
  dend_list
}

#' @title Search for the sub-dendrogram structure composed of selected labels
#' @export
#' @description
#' Given a dendrogram object, the function performs a recursive DFS algorithm to determine
#' the sub-dendrogram which is composed of (exactly) all 'selected_labels'.
#' @param dend a dendrogram object
#' @param selected_labels A character vector with the labels we expect to have 
#' in the sub-dendrogram. This doesn't have to be in the same order as in the dendrogram.
#' @return
#' Either a sub-dendrogram composed of only members of selected_labels.
#' If such a sub-dendrogram doesn't exist, the function returns NULL.
#' @examples
#'
#' \dontrun{
#' # define dendrogram object to play with:
#' dend <- iris[, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("labels_to_character") %>%
#'   color_branches(k = 5)
#' first.subdend.only <- names(cutree(dend, 4)[cutree(dend, 4) == 1])
#' sub.dend <- find_dendrogram(dend, first.subdend.only)
#' # Plotting the result
#' par(mfrow = c(1, 2))
#' plot(dend, main = "Original dendrogram")
#' plot(sub.dend, main = "First subdendrogram")
#' 
#'   dend <- 1:10 %>%
#' dist() %>%
#'   hclust() %>%
#'   as.dendrogram() %>%
#'   set("labels_to_character") %>%
#'   color_branches(k = 5)
#' 
#' selected_labels <- as.character(1:4)
#' sub_dend <- find_dendrogram(dend, selected_labels)
#' plot(dend, main = "Original dendrogram")
#' plot(sub_dend, main = "First subdendrogram")
#' 
#' 
#' }
#'
find_dendrogram <- function(dend, selected_labels) {
  # if the dendrogram is exactly the labels in selected_labels - then we found our dend 
  if (all(labels(dend) %in% selected_labels) && 
      (length(labels(dend)) == length(selected_labels))) {
    return(dend)
  }

  # if not, either we can find such a sub dendrogram, or it doesn't exist (return NULL)
  for(i in 1:length(dend)) {
    if(all(selected_labels %in% labels(dend[[i]]))) {
      return(Recall(dend[[i]], selected_labels))
    }
  }
  # if we couldn't find any sub-dend that includes all the labels we're looking for
  # then we return NULL
  return(NULL)
}





squash_dendrogram <- function(dend, squashed_original_height = FALSE, ...) {
   squashed_labels <- paste(labels(dend), collapse = "_")
   squashed_height <- ifelse(squashed_original_height, attr(dend, "height"), 0)
   dend <- min(unlist(dend))
   attr(dend, "midpoint") <- NULL
   attr(dend, "members") <- 1L
   attr(dend, "leaf") <- TRUE
   attr(dend, "height") <- squashed_height
   attr(dend, "label") <- squashed_labels
   class(dend) <- "dendrogram"
   dend
}


collapse_labels_0 <- function(dend, selected_labels,...) {
   # if the dendrogram is exactly the labels in selected_labels - then we found our dend 
   # let's squash it
   if (all(labels(dend) %in% selected_labels) && 
       (length(labels(dend)) == length(selected_labels))) {
      return(squash_dendrogram(dend,...))
   }
   
   # if not, either we can find such a sub dendrogram, or it doesn't exist (return original dend)
   for(i in 1:length(dend)) {
      if(all(selected_labels %in% labels(dend[[i]]))) {
         dend[[i]] <- Recall(dend[[i]], selected_labels,...)
      }
   }
   
   # return the dend (with/without squashing)
   return(dend)
}


#' @title Collapse a sub dendrogram of adjacent labels within a dend
#' @export
#' @description
#' Given a dendrogram object, and a set of labels that are in the same sub-dendrogram,
#' the function performs a recursive DFS algorithm to determine
#' the sub-dendrogram which is composed of (exactly) all 'selected_labels'.
#' It then squashes this sub-dendrogram, and returns the original dendrogram with the squashed 
#' dendrogram with it.
#' @param dend a dendrogram object
#' @param selected_labels A character vector with the labels we expect to have 
#' in the sub-dendrogram. This doesn't have to be in the same order as in the dendrogram.
#' @param ... elipsis (passed to squash_dendrogram)
#' @return
#' Either the original dend.
#' Or, if the labels properly are in the dend by each other, a dend with
#' a squashed sub-dendrogram inside it.
#' 
#' @examples
#' library("dendextend")
#' 
#' set.seed(23235)
#' ss <- sample(1:150, 5)
#' 
#' # Getting the dend object
#' dend25 <- iris[ss, -5] %>%
#'    dist() %>%
#'    hclust() %>%
#'    as.dendrogram() %>% 
#'    set("labels", letters[1:5])
#' 
#' par(mfrow = c(1,4))
#' plot(dend25)
#' plot(collapse_labels(dend25, c("d", "e")))
#' plot(collapse_labels(dend25, c("c", "d", "e")))
#' plot(collapse_labels(dend25, c("c", "d", "e"), squashed_original_height=TRUE))
collapse_labels <- function(dend, selected_labels,...) {
   dend <- collapse_labels_0(dend, selected_labels,...)
   dend <- midcache.dendrogram(dend)
   return(dend)
}

