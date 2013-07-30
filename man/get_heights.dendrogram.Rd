\name{get_heights.dendrogram}
\alias{get_heights.dendrogram}
\title{Get height attributes from a dendrogram}
\usage{
  get_heights.dendrogram(tree, decreasing = FALSE, ...)
}
\arguments{
  \item{tree}{a dendrogram.}

  \item{decreasing}{logical. Should the sort be increasing
  or decreasing? Not available for partial sorting.}

  \item{...}{not used.}
}
\value{
  a vector of the dendrogram's branches heights.
}
\description{
  Get height attributes from a dendrogram
}
\examples{
hc <- hclust(dist(USArrests[1:4,]), "ave")
dend <- as.dendrogram(hc)
get_heights.dendrogram(dend)
}

