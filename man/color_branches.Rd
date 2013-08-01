\name{color_branches}
\alias{color_branches}
\alias{colour_clusters}
\title{Color tree's branches according to sub-clusters}
\source{
  This function is a derived work from the
  \code{\link[dendroextra]{color_clusters}} function, with
  some ideas from the \code{\link[dendroextra]{slice}}
  function - both are from the {\pkg{dendroextra}} package
  by jefferis.
}
\usage{
  color_branches(tree, k = NULL, h = NULL, col,
    groupLabels = NULL, ...)
}
\arguments{
  \item{tree}{A \code{dendrogram} or \code{hclust} tree
  object}

  \item{k}{number of groups (passed to
  \code{\link[dendextend]{cutree}})}

  \item{h}{height at which to cut tree (passed to
  \code{\link[dendextend]{cutree}})}

  \item{col}{Function or vector of Colors. By default it
  tries to use \link[colorspace]{rainbow_hcl} from the
  \code{colorspace} package. (with parameters c=90 and
  l=50). If \code{colorspace} is not available, It will
  fall back on the \link{rainbow} function.}

  \item{groupLabels}{If TRUE add numeric group label - see
  Details for options}

  \item{...}{ignored.}
}
\value{
  a tree object of class dendrogram.
}
\description{
  This function is for dendrogram and hclust objects. This
  function colors both the terminal leaves of a tree's
  cluster and the edges leading to those leaves. The
  edgePar attribute of nodes will be augmented by a new
  list item col. The groups will be defined by a call to
  \code{\link[dendextend]{cutree}} using the k or h
  parameters.

  If col is a color vector with a different length than the
  number of clusters (k) - then a recycled color vector
  will be used.
}
\details{
  If \code{groupLabels=TRUE} then numeric group labels will
  be added to each cluster. If a vector is supplied then
  these entries will be used as the group labels. If a
  function is supplied then it will be passed a numeric
  vector of groups (e.g. 1:5) and must return the formatted
  group labels.
}
\examples{
dend <- hclust(dist(USArrests), "ave")
d1=color_branches(dend,5, col = c(3,1,1,4,1))
plot(d1) # selective coloring of branches :)

d5=color_branches(dend,5)
plot(d5)
d5g=color_branches(dend,5,groupLabels=TRUE)
plot(d5g)
d5gr=color_branches(dend,5,groupLabels=as.roman)
plot(d5gr)
}
\author{
  Tal Galili, extensively based on code by jefferis
}
\seealso{
  \code{\link[dendextend]{cutree}},\code{\link{dendrogram}},
  \code{\link{hclust}}, \code{\link{labels_colors}}
}

