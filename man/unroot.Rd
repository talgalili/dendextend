\name{unroot}
\alias{unroot}
\alias{unroot.default}
\alias{unroot.dendrogram}
\alias{unroot.hclust}
\alias{unroot.phylo}
\title{unroot trees}
\usage{
  unroot(x, ...)

  \method{unroot}{dendrogram}(x, branch_becoming_root = 1,
    new_root_height, ...)

  \method{unroot}{hclust}(x, branch_becoming_root = 1,
    new_root_height, ...)

  \method{unroot}{phylo}(x, ...)
}
\arguments{
  \item{x}{tree (dendrogram/hclust) object}

  \item{branch_becoming_root}{a numeric choosing the branch
  of the root which will become the new root (from left to
  right)}

  \item{new_root_height}{the new height of the branch which
  will become the new root.}

  \item{...}{passed on}
}
\value{
  An unrooted dendrogram
}
\description{
  unroot trees
}
\examples{
hc <- hclust(dist(USArrests[2:9,]), "com")
dend <- as.dendrogram(hc)

par(mfrow = c(1,3))
plot(dend, main = "original tree")
plot(unroot(dend , 1), main = "unrooted tree (left branch)")
plot(unroot(dend , 2), main = "tree without  (right branch)")
}
\seealso{
  \link[ape]{unroot} {ape}
}

