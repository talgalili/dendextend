\name{labels<-}
\alias{"labels<-.default"}
\alias{"labels<-.dendrogram"}
\alias{labels<-}
\title{"label" assignment operator}
\source{
  The functions here are based on code by Gavin and kohske
  from:
  \url{http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question}
}
\usage{
  labels(object, value, ...) <- value
}
\arguments{
  \item{object}{a variable name (possibly quoted) who's
  label are to be updated}

  \item{value}{a value to be assigned to object's label}
}
\value{
  The updated object
}
\description{
  "label" assignment operator for vectors, dendrogram, and
  hclust classes.
}
\examples{
x <- 1:3
labels(x)
labels(x) <- letters[1:3]
labels(x) # [1] "a" "b" "c"
x
# a b c
# 1 2 3


# get("labels<-")

################
# Example for using the assignment with dendrogram and hclust objects:
hc <- hclust(dist(USArrests[1:3,]), "ave")
dend <- as.dendrogram(hc)

labels(hc) # "Arizona" "Alabama" "Alaska"
labels(hc)  <- letters[1:3]
labels(hc)# "a" "b" "c"
labels(dend) # "Arizona" "Alabama" "Alaska"
labels(dend) <- letters[1:3]
labels(dend) # "a" "b" "c"
}
\author{
  Gavin Simpson, kohske, Tal Galili
}
\seealso{
  \code{\link{labels}}
}

