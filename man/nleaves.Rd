\name{nleaves}
\alias{nleaves}
\alias{nleaves.default}
\alias{nleaves.dendrogram}
\alias{nleaves.hclust}
\title{Counts the number of leaves in a tree}
\usage{
  nleaves(x, ...)

  \method{nleaves}{dendrogram}(x, ...)

  \method{nleaves}{hclust}(x, ...)
}
\arguments{
  \item{x}{tree object (dendrogram or hclust)}

  \item{...}{not used}
}
\value{
  The number of leaves in the tree
}
\description{
  Counts the number of leaves in a tree (dendrogram or
  hclust).
}
\details{
  The idea for the name is from functions like ncol, and
  nrow.

  Also, it is worth noting that the nleaves.dendrogram is
  based on order.dendrogram instead of labels.dendrogram
  since the first is MUCH faster than the later.
}
\examples{
hc <- hclust(dist(USArrests[1:5,]), "ave")
dend <- as.dendrogram(hc)

nleaves(dend) # 5
nleaves(hc) # 5
}
\seealso{
  \link{nrow}, \link{count_terminal_nodes}
}

