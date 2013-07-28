\name{get_leaves_attr}
\alias{get_leaves_attr}
\title{Get/set attributes of dendrogram's leaves}
\source{
  Heavily inspired by the code in the function
  \code{labels.dendrogram}, so credit should go to Martin
  Maechler.
}
\usage{
  get_leaves_attr(object, attribute, ...)
}
\arguments{
  \item{object}{a dendrogram object}

  \item{attribute}{character scaler of the attribute
  (\code{attr}) we wish to get/set from the leaves}

  \item{...}{not used}
}
\value{
  A vector with the dendrogram's leaves attribute
}
\description{
  Get/set attributes of dendrogram's leaves
}
\examples{
# define dendrogram object to play with:
hc <- hclust(dist(USArrests[1:3,]), "ave")
dend <- as.dendrogram(hc)

# get_leaves_attr(dend) # error :)
get_leaves_attr(dend, "label")
labels(dend, "label")
get_leaves_attr(dend, "height") # should be 0's
get_leaves_attr(dend, "leaf") # should be TRUE's
get_leaves_attr(dend, "members") # should be TRUE's
}

