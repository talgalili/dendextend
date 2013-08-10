\name{nleaves}
\alias{nleaves}
\alias{nleaves.default}
\alias{nleaves.dendrogram}
\alias{nleaves.hclust}
\alias{nleaves.phylo}
\title{Counts the number of leaves in a tree}
\usage{
  nleaves(x, ...)

  \method{nleaves}{dendrogram}(x, method = c("order",
    "members"), ...)

  \method{nleaves}{hclust}(x, ...)

  \method{nleaves}{phylo}(x, ...)
}
\arguments{
  \item{x}{tree object (dendrogram or hclust)}

  \item{method}{a character scalar (default is "members").
  If "order" than nleaves is based on length of
  \link{order.dendrogram}. If "members", than length is
  trusting what is written in the dendrogram's root
  \link{attr}. "members" is about 4 times faster than
  "order".}

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

  The phylo method is based on turning the phylo to hclust
  and than to dendrogram. It may not work for complex phylo
  trees.
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

