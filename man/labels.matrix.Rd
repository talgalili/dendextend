\name{labels.matrix}
\alias{'labels<-.matrix'}
\alias{labels.matrix}
\title{"label" assignment operator for matrix class}
\source{
  Based on code by Gavin Simpson from:
  \url{http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question}
}
\usage{
  labels.matrix(obj, which = c("colnames", "rownames"),
    ...)
}
\arguments{
  \item{object}{a variable name (possibly quoted) who's
  label are to be updated}

  \item{which}{"colnames" or "rownames", to which of the
  two should labels refer to.}

  \item{value}{a value to be assigned to object's label}
}
\value{
  The matrix object with updated col/row names
}
\description{
  "label" assignment operator for matrix class
}
\examples{
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
}
\author{
  Gavin Simpson, Tal Galili
}
\seealso{
  \code{\link{labels}}
}

