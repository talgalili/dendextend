\name{cutree.dendrogram}
\alias{cutree.dendrogram}
\title{Cut a Dendrogram Tree into Groups of Data}
\usage{
  cutree.dendrogram(tree, k = NULL, h = NULL,
    dend_heights_per_k = NULL,
    use_labels_not_values = TRUE,
    order_clusters_as_data = TRUE, warn = TRUE, ...)
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

  \item{warn}{logical. Should the function send a warning
  in case the desried k is not available? (deafult is
  TRUE)}

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

  In case there exists no such k for which exists a
  relevant split of the dendrogram, a warning is issued to
  the user, and NA is returned.
}
\details{
  At least one of k or h must be specified, k overrides h
  if both are given.

  as opposed to \link{cutree} for hclust,
  \code{cutree.dendrogram} allows the cutting of trees at a
  given height also for non-ultrametric trees (ultrametric
  tree == a tree with monotone clustering heights).
}
\examples{
\dontrun{
hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
dend <- as.dendrogram(hc)
unroot_dend <- unroot(dend,2)

cutree(hc, k=2:4) # on hclust
cutree(dend, k=2:4) # on dendrogram

cutree(hc, k=2) # on hclust
cutree(dend, k=2) # on dendrogram

cutree(dend, h = c(20, 25.5, 50,170))
cutree(hc, h = c(20, 25.5, 50,170))

# the default (ordered by original data's order)
cutree(dend, k=2:3, order_clusters_as_data = FALSE)
labels(dend)

# as.hclust(unroot_dend) # ERROR - can not do this...
cutree(unroot_dend, k = 2) # all NA's
cutree(unroot_dend, k = 1:4)
cutree(unroot_dend, h = c(20, 25.5, 50,170))
cutree(dend, h = c(20, 25.5, 50,170))


require(microbenchmark)
microbenchmark(
            cutree(hc, k=2:4),
            cutree(dend, k=2:4)
         )
         # the dendrogram is MUCH slower...

  Unit: microseconds
                   expr      min       lq    median       uq       max neval
    cutree(hc, k = 2:4)   70.552   73.351   89.0295  115.906   238.531   100
  cutree(dend, k = 2:4) 8379.915 8487.141 8615.9260 8785.585 12305.026   100

}
}
\author{
  Tal Galili
}
\seealso{
  \code{\link{hclust}}, \code{\link{cutree}},
  \code{\link{cutree_1h.dendrogram}}
}

