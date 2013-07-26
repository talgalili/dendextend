\name{labels_colors}
\alias{labels_colors}
\alias{labels_colors<-}
\title{Retrieve/assign colors to the labels of a dendrogram}
\source{
  Inspired by the code in the example of \link{dendrapply},
  so credit should also go to Martin Maechler.
}
\usage{
  labels_colors(object, ...)

  labels_colors(object, ...) <- value
}
\arguments{
  \item{object}{a dendrogram object}

  \item{...}{not used}

  \item{value}{a vector of colors to be used as new label's
  colors for the dendrogram}
}
\value{
  A vector with the dendrogram's labels colors (or a
  colored dendrogram, in case assignment is used)
}
\description{
  Retrieve/assign colors to the labels of a dendrogram
}
\examples{
# define dendrogram object to play with:
hc <- hclust(dist(USArrests[1:3,]), "ave")
dend <- as.dendrogram(hc)

# Defaults:
labels_colors(dend)
plot(dend)

# let's add some color:
labels_colors(dend) <- 2:4
labels_colors(dend)
plot(dend)

# changing color to black
labels_colors(dend) <- 1
labels_colors(dend)
plot(dend)

# removing color (and the nodePar completely - if it has no other attributed but lab.col)
labels_colors(dend) <- NULL
labels_colors(dend)
plot(dend)
}

