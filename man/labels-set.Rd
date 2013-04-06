\name{labels<-}
\alias{labels.hclust}
\alias{labels.matrix}
\alias{labels<-}
\alias{labels<-.default}
\alias{labels<-.dendrogram}
\alias{labels<-.hclust}
\alias{labels<-.matrix}
\title{"label" assignment operator}
\source{
  The functions here are based on code by Gavin and kohske
  from (adopted to dendrogram by Tal Galili):
  \url{http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question}
}
\usage{
  labels(object, ...) <- value

  \method{labels}{matrix}(object, which = c("colnames",
    "rownames"), ...)

  \method{labels}{matrix}(object, which = c("colnames",
    "rownames"), ...) <- value

  \method{labels}{dendrogram}(object, ...) <- value

  \method{labels}{hclust}(object, ...)

  \method{labels}{hclust}(object, ...) <- value
}
\arguments{
  \item{object}{a variable name (possibly quoted) who's
  label are to be updated}

  \item{which}{"colnames" or "rownames", to which of the
  two should labels refer to.}

  \item{...}{parameters passed (not currently in use)}

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
# Example for matrix objects

x <- matrix(1:9, 3,3)
labels(x)
x
labels(x) <- letters[1:3]
x
labels(x, which = "rownames") <- LETTERS[24:26]
x
#  a b c
#X 1 4 7
#Y 2 5 8
#Z 3 6 9

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
  Gavin Simpson, Tal Galili
}
\seealso{
  \code{\link{labels}}, \code{\link{labels.matrix}}
}

