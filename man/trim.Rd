\name{trim}
\alias{trim}
\alias{trim.default}
\alias{trim.dendrogram}
\alias{trim.hclust}
\alias{trim.phylo}
\title{Trim a tree}
\usage{
  trim(x, ...)

  \method{trim}{dendrogram}(x, leaves,...)

  \method{trim}{hclust}(x, leaves,...)

  \method{trim}{phylo}(x, ...)
}
\arguments{
  \item{x}{tree object (dendrogram/hclust/phylo)}

  \item{leaves}{a character vector of the label(S) of the
  tip(s) (leaves) we wish to trim off the tree.}

  \item{...}{passed on}
}
\value{
  A trimmed tree
}
\description{
  Trimms a tree (dendrogram, hclust) from a set of leaves
  based on their labels.
}
\details{
  I was not sure if to call this function drop.tip (from
  ape), snip/prune (from rpart) or just remove.leaves.  I
  ended up deciding on trim.
}
\examples{
hc <- hclust(dist(USArrests[1:5,]), "ave")
dend <- as.dendrogram(hc)

par(mfrow = c(1,2))
plot(dend, main = "original tree")
plot(trim(dend , c("Alaska", "California")), main = "tree without Alaska and California")
}
\seealso{
  \link{trim_leaf}, \link[ape]{drop.tip} {ape}
}

