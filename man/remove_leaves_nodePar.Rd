\name{remove_leaves_nodePar}
\alias{remove_leaves_nodePar}
\title{Remove all nodePar values from a dendrogram's leaves}
\usage{
remove_leaves_nodePar(object, ...)
}
\arguments{
  \item{object}{a dendrogram object}

  \item{...}{not used}
}
\value{
A dendrogram, after removing the nodePar attribute in all
of its leaves,
}
\description{
Go through the dendrogram leaves and remove its nodePar.
}
\examples{
\dontrun{

dend <- as.dendrogram(hclust(dist(USArrests[1:5,])))

dend <- color_labels(dend, 3)
par(mfrow = c(1,2))
plot(dend)
plot(remove_leaves_nodePar(dend))


get_leaves_attr(dend, "nodePar")
get_leaves_attr(remove_leaves_nodePar(dend), "nodePar")


}
}
\seealso{
\link{get_leaves_attr},
\link{assign_values_to_leaves_nodePar}
}

