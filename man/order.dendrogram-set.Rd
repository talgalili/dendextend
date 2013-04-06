\name{order.dendrogram<-}
\alias{order.dendrogram<-}
\title{order.dendrogram<- assignment operator}
\usage{
  order.dendrogram(object, ...) <- value
}
\arguments{
  \item{object}{a variable name (possibly quoted) who's
  label are to be updated}

  \item{...}{parameters passed (not currently in use)}

  \item{value}{a value to be assigned to object's leaves
  value (their "order")}
}
\value{
  dendrogram with updated order leaves values
}
\description{
  order.dendrogram<- assignment operator.  This is useful
  in cases where some object is turned into a dendrogram
  but its leaves values (the order) are all mixed up.
}
\examples{
################
# Example for using the assignment with dendrogram and hclust objects:
hc <- hclust(dist(USArrests[1:4,]), "ave")
dend <- as.dendrogram(hc)

str(dend)
order.dendrogram(dend) # 4 3 1 2
order.dendrogram(dend) <- 1:4
order.dendrogram(dend) # 1 2 3 4
str(dend) # the structure is still fine.
}
\seealso{
  \code{\link{order.dendrogram}}, \code{\link{labels<-}}
}

