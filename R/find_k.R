

#' @importFrom fpc pamk
NULL



#' @title Find the (estimated) number of clusters for a dendrogram using average silhouette width
#' @export
#' @aliases 
#' plot.find_k
#' @description
#' This function estimates the number of clusters based on the maximal average \link[cluster]{silhouette} width 
#' derived from running \link[cluster]{pam} on the \link[stats]{cophenetic} distance matrix of 
#' the \link[stats]{dendrogram}. The output is based on the \link[fpc]{pamk} output.
#' @param dend A dendrogram (or hclust) tree object
#' @param krange integer vector. Numbers of clusters which are to be compared 
#' by the average silhouette width criterion. 
#' Note: average silhouette width and Calinski-Harabasz can't estimate number 
#' of clusters nc=1. If 1 is included, a Duda-Hart test is applied and 1 is 
#' estimated if this is not significant.
#' @param ... passed to \link[fpc]{pamk} (the current defaults criterion="asw" and usepam=TRUE can not be changes).
#' @seealso
#' \link[fpc]{pamk}, \link[cluster]{pam}, \link[cluster]{silhouette}.
#' @return 
#' A \link[fpc]{pamk} output. This is a list with the following components: 
#' 1) pamobject - The output of the optimal run of the pam-function.
#' 2) nc	- the optimal number of clusters.
#' 3) crit - vector of criterion values for numbers of clusters. crit[1] is the p-value of the Duda-Hart test if 1 is in krange and diss=FALSE.
#' @examples
#' 
#' dend <- iris[,-5] %>% dist %>% hclust %>% as.dendrogram
#' dend_k <- find_k(dend)
#' plot(dend_k)
#' plot(color_branches(dend, k = dend_k$nc))
#' 
#' library(cluster)
#' sil <- silhouette(dend_k$pamobject)
#' plot(sil)
#' 
#' dend <- USArrests %>% dist %>% hclust(method = "ave") %>% as.dendrogram
#' dend_k <- find_k(dend)
#' plot(dend_k)
#' plot(color_branches(dend, k = dend_k$nc))
#' 
find_k <- function(dend, krange= 2:min(10, (nleaves(dend)-1)), ...)
{
   # library(fpc)
   # krange = 2:10
   # criterion = "asw"
   d <- cophenetic(dend) # this will work for both a dendrogram and an hclust object.
   out <- pamk(d, krange = krange, criterion="asw", usepam=TRUE, ...)
   class(out) <- "find_k"
   out
}


#' @export
plot.find_k <- function(x , 
                        xlab = "Number of clusters (k)", 
                        ylab = "Average silhouette width", 
                        main = "Estimating the number of clusters using\n average silhouette width", 
                        ...) {
   asw <- x$crit
   k <- seq_along(x$crit)
   
   col <- rep("black", length(k))
   col[which.max(asw)] <- "red"
   
   plot(asw ~ k, 
        xlab = xlab,
        ylab = ylab,
        main = main,
        type = "b", 
        las = 1,
        col = col,
        ...)
}



