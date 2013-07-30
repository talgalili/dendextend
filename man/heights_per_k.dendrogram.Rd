\name{heights_per_k.dendrogram}
\alias{heights_per_k.dendrogram}
\title{Which height will result in which k for a dendrogram}
\usage{
  heights_per_k.dendrogram(tree, ...)
}
\arguments{
  \item{tree}{a dendrogram.}

  \item{...}{not used.}
}
\value{
  a vector of heights, with its names being the k clusters
  that will result for cutting the dendrogram at each
  height.
}
\description{
  Which height will result in which k for a dendrogram
}
\examples{
\\donotrun{
hc <- hclust(dist(USArrests[1:4,]), "ave")
dend <- as.dendrogram(hc)
heights_per_k.dendrogram(dend)
##       1        2        3        4
##86.47086 68.84745 45.98871 28.36531

cutree(hc, h = 68.8) # and indeed we get 2 clusters
}
}

