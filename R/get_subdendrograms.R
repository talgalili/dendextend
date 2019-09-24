#' @title Extract a list of \emph{k} subdendrograms from a given dendrogram
#' object
#' @export
#' @description
#' Extracts a list of subdendrogram structures based on the cutree \code{\link{cutree.dendrogram}} function
#' from a given dendrogram object. It can be useful in case of more exact visual
#' investigation of clustering results.
#' @param dend a dendrogram object
#' @param k the number of subdendrograms that should be extracted
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
#' order.dendrogram(sub_dend) <- rank(order.dendrogram(sub_dend))
#' heatmap.2(subset_iris, Rowv = sub_dend, trace = "none", col = viridis::viridis(100))
get_subdendrograms <- function(dend, k, ...) {
  clusters <- cutree(dend, k, ...)
  dend_list <- lapply(unique(clusters), function(cluster.id) {
    find_dendrogram(dend, which(clusters == cluster.id))
  })
  class(dend_list) <- "dendlist"
  dend_list
}

#' @title Search for the subdendrogram structure composed of indicated labels
#' @export
#' @description
#' Given a dendrogram object, the function performs a recursive DFS algorithm to determine
#' the subdendrogram which is composed of all indicated labels. The labels
#' which should compose the subdendrogram are marked as TRUE in the logical
#' vector of length \code{nleaves(dend)}
#' @param dend a dendrogram object
#' @param selected_labels logical vector with TRUE values at positions of
#' members which should be included in the resulting subdendrogram
#' @return
#' A subdendrogram composed of only members indicated in the given logical
#' vector
#' clusters.
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
#' first.subdend.only <- cutree(dend, 4) == 1
#' sub.dend <- find_dendrogram(dend, first.subdend.only)
#' # Plotting the result
#' par(mfrow = c(1, 2))
#' plot(dend, main = "Original dendrogram")
#' plot(sub.dend, main = "First subdendrogram")
#' }
#'
find_dendrogram <- function(dend, selected_labels) {
  if (all(unlist(dend) %in% selected_labels)) {
    return(dend)
  }

  if (any(unlist(dend[[1]]) %in% selected_labels)) {
    return(find_dendrogram(dend[[1]], selected_labels))
  } else {
    return(find_dendrogram(dend[[2]], selected_labels))
  }
}
