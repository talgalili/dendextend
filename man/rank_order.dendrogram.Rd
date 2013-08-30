\name{rank_order.dendrogram}
\alias{rank_order.dendrogram}
\title{Fix rank of leaves order values in a dendrogram}
\usage{
  rank_order.dendrogram(dend, ...)
}
\arguments{
  \item{dend}{a dendrogram object}

  \item{...}{not used}
}
\value{
  A dendrogram, after fixing its leaves order values.
}
\description{
  Generally, leaves order value should be a sequence of
  integer values. From 1 to nleaves(tree). This function
  fixes trees by using \link{rank} on existing leaves order
  values.
}
\examples{
# define dendrogram object to play with:
dend <- as.dendrogram(hclust(dist(USArrests[1:4,]), "ave"))
# plot(dend)
order.dendrogram(dend)
dend2 <- prune(dend, "Alaska")
order.dendrogram(dend2)
order.dendrogram(rank_order.dendrogram(dend2))
}
\seealso{
  \link{prune}
}

