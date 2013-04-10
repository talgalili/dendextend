\name{intersect_trees}
\alias{intersect_trees}
\title{Intersect trees}
\usage{
  intersect_trees(x1, x2, ...)
}
\arguments{
  \item{x1}{tree object (dendrogram/hclust/phylo)}

  \item{x2}{tree object (dendrogram/hclust/phylo)}

  \item{...}{passed on}
}
\value{
  A list with two trimmed trees
}
\description{
  Return two trees after trimming them so that the only
  leaves left are the intersection of their labels.
}
\examples{
hc <- hclust(dist(USArrests[1:5,]), "ave")
dend <- as.dendrogram(hc)
labels(dend) <- 1:5
dend1 <- trim(dend, 1)
dend2 <- trim(dend, 5)
intersect_dend <- intersect_trees(dend1, dend2)

layout(matrix(c(1,1,2,3,4,5), 3,2, byrow=TRUE))
plot(dend)
plot(dend1); plot(dend2)
plot(intersect_dend[[1]]);plot(intersect_dend[[2]])
}
\seealso{
  \link{trim}, \link{intersect}, \link{labels}
}

