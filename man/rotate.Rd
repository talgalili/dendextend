\name{rotate}
\alias{flip}
\alias{rotate}
\alias{rotate.default}
\alias{rotate.dendrogram}
\alias{rotate.hclust}
\alias{rotate.phylo}
\alias{sort.dendrogram}
\alias{sort.hclust}
\title{Rotate tree objects based on label's order}
\usage{
  rotate(x, order, ...)

  \method{rotate}{dendrogram}(x, order, ...)

  \method{rotate}{hclust}(x, order, ...)

  \method{rotate}{phylo}(x, ...)

  flip(x,...)

  \method{sort}{dendrogram}(x, decreasing=FALSE, ...)

  \method{sort}{hclust}(x, decreasing=FALSE, ...)
}
\arguments{
  \item{x}{a tree object (either a \code{dendrogram} or
  \code{hclust})}

  \item{order}{a numeric vector with the order of the value
  to be assigned to object's label. The numbers say which
  of the items on the tree-plot should be "first" (e.g:
  most left), second etc. (releacnt only to \code{rotate})}

  \item{decreasing}{logical. Should the sort be increasing
  or decreasing? Not available for partial sorting.
  (relevant only to \code{sort})}

  \item{...}{parameters passed (for example, in case of
  sort)}
}
\value{
  A rotated tree object
}
\description{
  Rotates, flip and sort the branches of a tree object
  (dendrogram, hclust) based on a vector of labels order
}
\details{
  The motivation for this function came from the function
  \code{\link{order.dendrogram}} not being intuitive enough
  to use as is.  What \code{rotate} aims to do is give a
  simple tree rotation function which is based on the order
  the user would like to see the tree rotated by (just as
  \code{\link{order}} works for numeric vectors).

  \code{flip} returns the tree object after rotating it so
  to reverse the order of the labels.  Note that flip is
  just like \code{\link{rev.dendrogram}} (yet it also
  includes an hclust method)

  The \code{sort} methods sort the labels of the tree
  (using \code{order}) and then attempts to rotate the tree
  to fit that order.

  The hclust method of "\code{rotate}" works by first
  changing the object into dendrogram, performing the
  rotation, and then changing it back to hclust.  Special
  care is taken in preserving some of the properties of the
  hclust object.

  The {ape} package has its own
  \code{\link[ape]{rotate}}({ape}) function (Which is sadly
  not S3, so cannot be easily connected with the current
  implementation).  Still, there is an S3 plug that makes
  sure people loading first ape and then dendextend will
  still be able to use \code{rotate} without a problem.
  Notice that if you will first load {ape} and only then
  {dendextend}, using "rotate" will fail with the error:
  "Error in rotate(dend, ____) : object "phy" is not of
  class "phylo"" - this is because rotate in ape is not S3
  and will fail to find the rotate.dendrogram function.  In
  such a case simply run \code{detach("package:ape")}
}
\examples{
hc <- hclust(dist(USArrests[c(1,6,13,20, 23),]), "ave")
dend <- as.dendrogram(hc)

# For dendrogram objects:
labels_colors(dend) <- rainbow(nleaves(dend)) # let's color the labels to make the followup of the rotation easier
par(mfrow = c(1,2))
plot(dend, main = "Original tree")
plot(rotate(dend, c(2:5,1)), main = "Rotates the left most leaf \\n into the right side of the tree")
par(mfrow = c(1,2))
plot(dend, main = "Original tree")
plot(sort(dend), main = "Sorts the labels by alphabetical order \\n and rotates the tree to give the best fit possible")
par(mfrow = c(1,2))
plot(dend, main = "Original tree")
plot(flip(dend), main = "Flips the order of the tree labels")

# For hclust objects:
plot(hc)
plot(rotate(hc, c(2:5,1)), main = "Rotates the left most leaf \\n into the right side of the tree")
}
\seealso{
  \code{\link{order.dendrogram}}, \code{\link{order}},
  \code{\link{rev.dendrogram}}, \code{\link[ape]{rotate}}
  ({ape})
}

