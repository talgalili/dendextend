\name{cutree_1k.dendrogram}
\alias{cutree_1k.dendrogram}
\title{cutree for dendrogram (by 1 k value only!)}
\usage{
  cutree_1k.dendrogram(tree, k, dendrogram_heights_per_k,
    use_labels_not_values = TRUE,
    order_clusters_as_data = TRUE, warn = TRUE, ...)
}
\arguments{
  \item{tree}{a dendrogram object}

  \item{k}{numeric scalar (not a vector!) with the number
  of clusters the tree should be cut into.}

  \item{dendrogram_heights_per_k}{a named vector that
  resulted from running. \code{heights_per_k.dendrogram}.
  When running the function many times, supplying this
  object will help improve the running time.}

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
  \code{cutree_1k.dendrogram} returns an integer vector
  with group memberships.

  In case there exists no such k for which exists a
  relevant split of the dendrogram, a warning is issued to
  the user, and NULL is returned.
}
\description{
  Cuts a dendrogram tree into several groups by specifying
  the desired number of clusters k (only a single k
  value!).

  In case there exists no such k for which exists a
  relevant split of the dendrogram, a warning is issued to
  the user, and NULL is returned.
}
\examples{
hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
dend <- as.dendrogram(hc)
cutree(hc, h=50) # on hclust
cutree_1h.dendrogram(dend, h=50) # on a dendrogram

labels(dend)

# the default (ordered by original data's order)
cutree_1h.dendrogram(dend, h=50, order_clusters_as_data = TRUE)

# A different order of labels - order by their order in the tree
cutree_1h.dendrogram(dend, h=50, order_clusters_as_data = FALSE)


# make it faster
\dontrun{
require(microbenchmark)
microbenchmark(
         cutree_1h.dendrogram(dend, h=50),
         cutree_1h.dendrogram(dend, h=50,use_labels_not_values = FALSE)
         )
         # 0.8 vs 0.6 sec - for 100 runs
}
}
\author{
  Tal Galili
}
\seealso{
  \code{\link{hclust}}, \code{\link{cutree}},
  \code{\link{cutree_1h.dendrogram}}
}

