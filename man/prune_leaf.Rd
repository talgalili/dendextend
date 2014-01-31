\name{prune_leaf}
\alias{prune_leaf}
\title{Trimms one leaf from a dendrogram}
\usage{
prune_leaf(x, leaf_name, ...)
}
\arguments{
  \item{x}{dendrogram object}

  \item{leaf_name}{a character string as the label of the
  tip we wish to prune}

  \item{...}{passed on}
}
\value{
A dendrogram with a leaf pruned
}
\description{
Trimms one leaf from a dendrogram
}
\details{
Used through \link{prune}
}
\examples{
hc <- hclust(dist(USArrests[1:5,]), "ave")
dend <- as.dendrogram(hc)

par(mfrow = c(1,2))
plot(dend, main = "original tree")
plot(dendextend:::prune_leaf(dend , "Alaska"), main = "tree without Alaska")
}

