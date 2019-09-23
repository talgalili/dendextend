

# devtools::install_github('metrumresearchgroup/sinew')
# sinew::makeOxygen(min_depth)


#' @title Find minimum/maximum depth of a dendrogram
#' @rdname depth
#' @export
#' @description
#' As the name implies. This can also work for non-dendrogram nested lists.
#' @param dend Any nested list object (including \link{dendrogram}).
#' @param ... unused at the moment.
#' @return Integer, the (min/max) number of nodes from the root to the leafs
#' @examples
#'
#' hc <- hclust(dist(USArrests), "ave")
#' (dend1 <- as.dendrogram(hc)) # "print()" method
#' is.list(dend1)
#' is.list(dend1[[1]][[1]][[1]])
#' dend1[[1]][[1]][[1]]
#' plot(dend1)
#' min_depth(dend1)
#' max_depth(dend1)
min_depth <- function(dend, ...) {
  if (!is.list(dend)) {
    return(1L)
  } # we reached a leaf, and should just return 1 node
  # if it is a list, then we may have children, let's check how many we have:
  n_children <- length(dend)
  if (n_children < 2) stop("something is odd. Each non-leaf node should have at least two children")

  children_depth <- numeric(n_children)
  for (i in 1:n_children) {
    # children_depth[i] <- min_depth(dend[[i]])
    children_depth[i] <- Recall(dend[[i]])
  }

  # return 1 for current node
  1L + min(children_depth)
}


#' @rdname depth
#' @export
max_depth <- function(dend, ...) {
  if (!is.list(dend)) {
    return(1L)
  } # we reached a leaf, and should just return 1 node
  # if it is a list, then we may have children, let's check how many we have:
  n_children <- length(dend)
  if (n_children < 2) stop("something is odd. Each non-leaf node should have at least two children")

  children_depth <- numeric(n_children)
  for (i in 1:n_children) {
    children_depth[i] <- Recall(dend[[i]])
  }

  # return 1 for current node
  1L + max(children_depth)
}
