\name{intersect_trees}
\alias{intersect_trees}
\title{Intersect trees}
\usage{
  intersect_trees(x1, x2, warn = FALSE, ...)
}
\arguments{
  \item{x1}{tree object (dendrogram/hclust/phylo)}

  \item{x2}{tree object (dendrogram/hclust/phylo)}

  \item{warn}{logical (FALSE). Should a warning be issued
  if there was a need to perform intersaction.}

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
plot(dend, main = "Original tree")
plot(dend1, main = "Tree 1:\\n original with label 1 trimmed"); plot(dend2, main = "Tree 2:\\n original with label 2 trimmed")
plot(intersect_dend[[1]], main = "Tree 1 trimmed\\n with the labels that intersected with those of Tree 2");plot(intersect_dend[[2]], main = "Tree 2 trimmed\\n with the labels that intersected with those of Tree 1")
}
\seealso{
  \link{trim}, \link{intersect}, \link{labels}
}

