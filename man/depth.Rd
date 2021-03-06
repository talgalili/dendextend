% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/depth.R
\name{min_depth}
\alias{min_depth}
\alias{max_depth}
\title{Find minimum/maximum depth of a dendrogram}
\usage{
min_depth(dend, ...)

max_depth(dend, ...)
}
\arguments{
\item{dend}{Any nested list object (including \link{dendrogram}).}

\item{...}{unused at the moment.}
}
\value{
Integer, the (min/max) number of nodes from the root to the leafs
}
\description{
As the name implies. This can also work for non-dendrogram nested lists.
}
\examples{

hc <- hclust(dist(USArrests), "ave")
(dend1 <- as.dendrogram(hc)) # "print()" method
is.list(dend1)
is.list(dend1[[1]][[1]][[1]])
dend1[[1]][[1]][[1]]
plot(dend1)
min_depth(dend1)
max_depth(dend1)
}
