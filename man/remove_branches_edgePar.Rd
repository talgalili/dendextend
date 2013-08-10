\name{remove_branches_edgePar}
\alias{remove_branches_edgePar}
\title{Remove all edgePar values from a dendrogram's branches}
\usage{
  remove_branches_edgePar(object, ...)
}
\arguments{
  \item{object}{a dendrogram object}

  \item{...}{not used}
}
\value{
  A dendrogram, after removing the edgePar attribute in all
  of its branches,
}
\description{
  Go through the dendrogram branches and remove its
  edgePar.
}
\examples{
\dontrun{

dend <- as.dendrogram(hclust(dist(USArrests[1:5,])))
dend <- color_branches(dend, 3)
par(mfrow = c(1,2))
plot(dend)
plot(remove_branches_edgePar(dend))

}
}
\seealso{
  \link{get_branches_attr},
  \link{assign_values_to_branches_edgePar}
}

