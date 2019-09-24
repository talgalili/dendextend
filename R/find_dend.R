

#' @title Finds a "good" dendrogram for a dist
#' @export
#' @rdname find_dend
#'
#' @description
#' There are many options for choosing distance and linkage functions for hclust.
#' This function goes through various combinations of the two and helps find the one
#' that is most "similar" to the original distance matrix.
#'
#' @param x A matrix or a data.frame. Can also be a \link{dist} object.
#'
#' @param dist_methods A vector of possible \link{dist} methods.
#' @param hclust_methods A vector of possible \link{hclust} methods.
#' @param hclust_fun By default \link{hclust}.
#' @param optim_fun A function that accepts a dend and a dist and returns how the two
#' are in agreement. Default is \link{cor_cophenetic}.
#' @param ... options passed from find_dend to dend_expend.
#'
#' @return
#' dend_expend:
#' A list with three items. The first item is called "dends" and includes
#' a dendlist with all the possible dendrogram combinations. The second is "dists" and
#' includes a list with all the possible distance matrix combination.
#' The third. "performance", is data.frame with three columns: dist_methods, hclust_methods, and optim.
#' optim is calculated (by default) as the cophenetic correlation (see: \link{cor_cophenetic}) between the distance matrix and
#' the \link{cophenetic} distance of the hclust object.
#'
#' @examples
#'
#' x <- datasets::mtcars
#' out <- dend_expend(x, dist_methods = c("euclidean", "manhattan"))
#' out$performance
#'
#' dend_expend(dist(x))$performance
#'
#' best_dend <- find_dend(x, dist_methods = c("euclidean", "manhattan"))
#' plot(best_dend)
dend_expend <- function(x,
                        dist_methods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                        hclust_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                        hclust_fun = hclust,
                        optim_fun = cor_cophenetic,
                        ...) {
  if (is.dist(x)) {
    dist_combo <- list(x)
    names(dist_combo) <- dist_methods <- "unknown"
  } else {
    dist_combo <- list()
    for (i in 1:length(dist_methods)) {
      dist_combo[[i]] <- x %>% dist(method = dist_methods[i])
    }
    names(dist_combo) <- dist_methods
  }


  out_dendlist <- dendlist()
  dist_hclust_combo <- expand.grid(dist_methods, hclust_methods) %>% data.frame()
  colnames(dist_hclust_combo) <- c("dist_methods", "hclust_methods")

  for (i in 1:nrow(dist_hclust_combo)) {
    tmp_dist_name <- dist_hclust_combo[i, 1]
    tmp_dist <- dist_combo[[tmp_dist_name]]
    tmp_hclust_method <- dist_hclust_combo[i, 2]
    tmp_dend <- tmp_dist %>%
      hclust(method = tmp_hclust_method) %>%
      as.dendrogram()

    dist_hclust_combo$optim[i] <- optim_fun(tmp_dend, tmp_dist)

    out_dendlist <- dendlist(out_dendlist, tmp_dend)
  }

  names(out_dendlist) <- paste(dist_hclust_combo[, 1], dist_hclust_combo[, 2], sep = "_")
  # attr(out_dendlist, "dist_method") <- dist_hclust_combo[,1]
  # attr(out_dendlist, "hclust_method") <- dist_hclust_combo[,2]

  # which.max(dist_hclust_combo$optim)
  # dist_hclust_combo[29,]

  list(
    dends = out_dendlist,
    dists = dist_combo,
    performance = dist_hclust_combo
  )
}


# a <- dend_expend(mtcars)
# cophenetic(a[[1]])
# dist(mtcars)
#
# identical(labels(cophenetic(a[[1]])),
#           labels(dist(mtcars)))
#
# identical(labels(sort_dist_mat(cophenetic(a[[1]]))),
#           labels(sort_dist_mat(dist(mtcars))))



#' @export
#' @rdname find_dend
#' @return
#' find_dend: A dendrogram which is "optimal" based on the output from dend_expend.
find_dend <- function(x, ...) {
  out <- dend_expend(x, ...)
  best_dend_loc <- which.max(out$performance$optim)
  out$dends[[best_dend_loc]]
}
