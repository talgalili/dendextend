\name{assign_values_to_leaves_nodePar}
\alias{assign_values_to_leaves_nodePar}
\title{Assign values to nodePar of dendrogram's leaves}
\usage{
  assign_values_to_leaves_nodePar(object, value, nodePar,
    ...)
}
\arguments{
  \item{object}{a dendrogram object}

  \item{value}{a new value vector for the nodePar
  attribute. It should be the same length as the number of
  leaves in the tree. If not, it will recycle the value and
  issue a warning.}

  \item{nodePar}{the value inside nodePar to adjust.}

  \item{...}{not used}
}
\value{
  A dendrogram, after adjusting the nodePar attribute in
  all of its leaves,
}
\description{
  Go through the dendrogram leaves and updates the values
  inside its nodePar
}
\examples{
\dontrun{

hc <- hclust(dist(USArrests[1:5,]), "ave")
dend <- as.dendrogram(hc)


# reproduces "labels_colors<-"
# although it does force us to run through the tree twice,
# hence "labels_colors<-" is better...
plot(dend)
dend <- assign_values_to_leaves_nodePar(object=dend, value = c(3,2), nodePar = "lab.col")
plot(dend)

dend <- assign_values_to_leaves_nodePar(dend, 1, "pch")
plot(dend)
# fix the annoying pch=1:
dend <- assign_values_to_leaves_nodePar(dend, NA, "pch")
plot(dend)
# adjust the cex:
dend <- assign_values_to_leaves_nodePar(dend, 19, "pch")
dend <- assign_values_to_leaves_nodePar(dend, 2, "lab.cex")
plot(dend)

get_leaves_attr(dend, "nodePar", simplify=FALSE)

}
}
\seealso{
  \link{get_leaves_attr}
}

