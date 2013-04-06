\name{trim_leaf}
\alias{trim_leaf}
\title{Trimms one leaf from a dendrogram}
\usage{
  trim_leaf(x, leaf_name, ...)
}
\arguments{
  \item{x}{dendrogram object}

  \item{leaf_name}{a character string as the label of the
  tip we wish to trim}

  \item{...}{passed on}
}
\value{
  A dendrogram with with a leaf trimmed
}
\description{
  Trimms one leaf from a dendrogram
}
\details{
  Used through \link{trim}
}
\examples{
hc <- hclust(dist(USArrests[1:5,]), "ave")
dend <- as.dendrogram(hc)

par(mfrow = c(1,2))
plot(dend, main = "original tree")
plot(dendextend:::trim_leaf(dend , "Alaska"), main = "tree without Alaska")
}

