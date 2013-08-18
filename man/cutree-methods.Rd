\name{cutree}
\alias{cutree}
\alias{cutree.default}
\alias{cutree.dendrogram}
\alias{cutree.hclust}
\alias{cutree.phylo}
\title{Cut a Tree (Dendrogram/hclust/phylo) into Groups of Data}
\usage{
  cutree(tree, k = NULL, h = NULL,...)

  \method{cutree}{hclust}(tree, k = NULL, h = NULL,
    order_clusters_as_data =TRUE, sort_cluster_numbers =
    TRUE, warn = TRUE, ...)

  \method{cutree}{phylo}(tree, k = NULL, h = NULL,...)

  \method{cutree}{dendrogram}(tree, k = NULL, h = NULL,
    dend_heights_per_k = NULL, use_labels_not_values =
    TRUE, order_clusters_as_data =TRUE,
    sort_cluster_numbers = TRUE, warn = TRUE,
    try_cutree_hclust = TRUE, ...)
}
\arguments{
  \item{tree}{a dendrogram object}

  \item{k}{numeric scalar (OR a vector) with the number of
  clusters the tree should be cut into.}

  \item{h}{numeric scalar (OR a vector) with a height where
  the tree should be cut.}

  \item{dend_heights_per_k}{a named vector that resulted
  from running. \code{heights_per_k.dendrogram}. When
  running the function many times, supplying this object
  will help improve the running time if using k!=NULL .}

  \item{use_labels_not_values}{logical, defaults to TRUE.
  If the actual labels of the clusters do not matter - and
  we want to gain speed (say, 10 times faster) - then use
  FALSE (gives the "leaves order" instead of their
  labels.). This is passed to \code{cutree_1h.dendrogram}.}

  \item{order_clusters_as_data}{logical, defaults to TRUE.
  There are two ways by which to order the clusters: 1) By
  the order of the original data. 2) by the order of the
  labels in the dendrogram. In order to be consistent with
  \link[stats]{cutree}, this is set to TRUE. This is passed
  to \code{cutree_1h.dendrogram}.}

  \item{sort_cluster_numbers}{logical (TRUE). Should the
  resulting cluster id numbers be sorted? (default is FALSE
  in order to make the function compatible with \code{
  \link[stats]{cutree} } ) from {stats}, but it allows for
  sensible color order when using \link{color_branches}.}

  \item{warn}{logical. Should the function send a warning
  in case the desried k is not available? (deafult is
  TRUE)}

  \item{try_cutree_hclust}{logical. default is TRUE. Since
  cutree for hclust is MUCH faster than for dendrogram -
  cutree.dendrogram will first try to change the dendrogram
  into an hclust object. If it will fail (for example, with
  unbranched trees), it will continue using the
  cutree.dendrogram function. If try_cutree_hclust=FALSE,
  it will force to use cutree.dendrogram and not
  cutree.hclust.}

  \item{...}{(not currently in use)}
}
\value{
  If k or h are scalar - \code{cutree.dendrogram} returns
  an integer vector with group memberships. Otherwise a
  matrix with group memberships is returned where each
  column corresponds to the elements of k or h,
  respectively (which are also used as column names).

  In case there exists no such k for which exists a
  relevant split of the dendrogram, a warning is issued to
  the user, and NA is returned.
}
\description{
  Cuts a dendrogram tree into several groups by specifying
  the desired number of clusters k(s), or cut height(s).

  For \code{hclust.dendrogram} - In case there exists no
  such k for which exists a relevant split of the
  dendrogram, a warning is issued to the user, and NA is
  returned.
}
\details{
  At least one of k or h must be specified, k overrides h
  if both are given.

  as opposed to \link[stats]{cutree} for hclust,
  \code{cutree.dendrogram} allows the cutting of trees at a
  given height also for non-ultrametric trees (ultrametric
  tree == a tree with monotone clustering heights). This
  \code{sort_cluster_numbers} parameter is based on the
  \link{sort_levels_values} function. Using that parameter
  with TRUE makes the clusters id's from cutree to be
  ordered from left to right. e.g: the left most cluster in
  the tree will be numbered "1", the one after it will be
  "2" etc...).
}
\examples{
\dontrun{
hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
dend <- as.dendrogram(hc)
unbranch_dend <- unbranch(dend,2)

cutree(hc, k=2:4) # on hclust
cutree(dend, k=2:4) # on dendrogram

cutree(hc, k=2) # on hclust
cutree(dend, k=2) # on dendrogram

cutree(dend, h = c(20, 25.5, 50,170))
cutree(hc, h = c(20, 25.5, 50,170))

# the default (ordered by original data's order)
cutree(dend, k=2:3, order_clusters_as_data = FALSE)
labels(dend)

# as.hclust(unbranch_dend) # ERROR - can not do this...
cutree(unbranch_dend, k = 2) # all NA's
cutree(unbranch_dend, k = 1:4)
cutree(unbranch_dend, h = c(20, 25.5, 50,170))
cutree(dend, h = c(20, 25.5, 50,170))


require(microbenchmark)
## this shows how as.hclust is expensive - but still worth it if possible
microbenchmark(
      cutree(hc, k=2:4),
      cutree(as.hclust(dend), k=2:4),
      cutree(dend, k=2:4),
      cutree(dend, k=2:4, try_cutree_hclust=FALSE)
   )
         # the dendrogram is MUCH slower...

# Unit: microseconds
##                       expr      min       lq    median        uq       max neval
##        cutree(hc, k = 2:4)   91.270   96.589   99.3885  107.5075   338.758   100
##    tree(as.hclust(dend),
##			  k = 2:4)           1701.629 1767.700 1854.4895 2029.1875  8736.591   100
##      cutree(dend, k = 2:4) 1807.456 1869.887 1963.3960 2125.2155  5579.705   100
##  cutree(dend, k = 2:4,
##	try_cutree_hclust = FALSE) 8393.914 8570.852 8755.3490 9686.7930 14194.790   100

# and trying to "hclust" is not expensive (which is nice...)
microbenchmark(
  cutree_unbranch_dend = cutree(unbranch_dend, k=2:4),
  cutree_unbranch_dend_not_trying_to_hclust =
  cutree(unbranch_dend, k=2:4, try_cutree_hclust=FALSE)
)


## Unit: milliseconds
##                   expr      min       lq   median       uq      max neval
##cutree_unbranch_dend       7.309329 7.428314 7.494107 7.752234 17.59581   100
##cutree_unbranch_dend_not
##_trying_to_hclust        6.945375 7.079198 7.148629 7.577536 16.99780   100
##There were 50 or more warnings (use warnings() to see the first 50)



}
}
\author{
  \code{cutree.dendrogram} was written by Tal Galili.
  \code{cutree.hclust} is redirecting the function to
  \link[stats]{cutree} from base R.
}
\seealso{
  \code{\link{hclust}}, \code{\link[stats]{cutree}},
  \code{\link{cutree_1h.dendrogram}},
  \code{\link{cutree_1k.dendrogram}},
  \code{\link{cutree_per_k}}
}

