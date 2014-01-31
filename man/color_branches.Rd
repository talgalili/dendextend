\name{color_branches}
\alias{color_branches}
\alias{colour_branches}
\title{Color tree's branches according to sub-clusters}
\source{
This function is a derived work from the
\code{\link[dendroextra]{color_clusters}} function, with
some ideas from the \code{\link[dendroextra]{slice}}
function - both are from the {\pkg{dendroextra}} package by
jefferis.

It extends it by using \link[dendextend]{cutree.dendrogram}
- allowing the function to work for trees that hclust can
not handle (unbranched and non-ultrametric trees). Also, it
allows REPEATED cluster color assignments to branches on to
the same tree. Something which the original function was
not able to handle.
}
\usage{
color_branches(tree, k = NULL, h = NULL, col, groupLabels = NULL, ...)
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
cluster and the edges leading to those leaves. The edgePar
attribute of nodes will be augmented by a new list item
col. The groups will be defined by a call to
\code{\link[dendextend]{cutree}} using the k or h
parameters.

If col is a color vector with a different length than the
number of clusters (k) - then a recycled color vector will
be used.
}
\details{
If \code{groupLabels=TRUE} then numeric group labels will
be added to each cluster. If a vector is supplied then
these entries will be used as the group labels. If a
function is supplied then it will be passed a numeric
vector of groups (e.g. 1:5) and must return the formatted
group labels.

If the \link{labels} of the dendrogram are NOT character
(but, for example integers) - they are coerced into
character. This step is essential for the proper operation
of the function. A dendrogram labels might happen to be
integers if they are based on an \link{hclust} performed on
a \link{dist} of an object without \link{rownames}.
}
\examples{
\dontrun{
dend <- as.dendrogram(hclust(dist(USArrests), "ave"))
d1=color_branches(dend,5, col = c(3,1,1,4,1))
plot(d1) # selective coloring of branches :)
d2=color_branches(d1,5)
plot(d2)

d1=color_branches(dend,5, col = c(3,1,1,4,1),groupLabels=TRUE)
plot(d1) # selective coloring of branches :)
d2=color_branches(d1,5,groupLabels=TRUE)
plot(d2)

d5=color_branches(dend,5)
plot(d5)
d5g=color_branches(dend,5,groupLabels=TRUE)
plot(d5g)
d5gr=color_branches(dend,5,groupLabels=as.roman)
plot(d5gr)

# messy - but interesting:
dend_override=color_branches(dend,2,groupLabels=as.roman)
dend_override=color_branches(dend_override,4,groupLabels=as.roman)
dend_override=color_branches(dend_override,7,groupLabels=as.roman)
plot(dend_override)

d5=color_branches(tree=dend[[1]],k=5)


require(dendextend)
data(iris)
d_iris <- dist(iris[,-5])
hc_iris <- hclust(d_iris)
dend_iris <- as.dendrogram(hc_iris)
dend_iris=color_branches(dend_iris,k=3)

labels_colors(dend_iris) <-
 rainbow_hcl(3)[sort_levels_values(
 as.numeric(iris[,5])[order.dendrogram(dend_iris)]
 )]

plot(dend_iris,
main = "Clustered Iris dataset",
 sub = "labels are colored based on the true cluster")



# cutree(dend_iris,k=3, order_clusters_as_data=FALSE, sort_cluster_numbers = TRUE, try_cutree_hclust=FALSE)
# cutree(dend_iris,k=3, order_clusters_as_data=FALSE, sort_cluster_numbers = TRUE, try_cutree_hclust=TRUE)

require(colorspace)

data(iris)
d_iris <- dist(iris[,-5])
hc_iris <- hclust(d_iris)
labels(hc_iris) # no labels, because "iris" has no row names
dend_iris <- as.dendrogram(hc_iris)
is.integer(labels(dend_iris)) # this could cause problems...

iris_species <- rev(levels(iris[,5]))
dend_iris <- color_branches(dend_iris,k=3, groupLabels=iris_species)
is.character(labels(dend_iris)) # labels are no longer "integer"

# have the labels match the real classification of the flowers:
labels_colors(dend_iris) <-
   rainbow_hcl(3)[sort_levels_values(
      as.numeric(iris[,5])[order.dendrogram(dend_iris)]
   )]

# We'll add the flower type
labels(dend_iris) <- paste(as.character(iris[,5])[order.dendrogram(dend_iris)],
                           "(",labels(dend_iris),")",
                           sep = "")

dend_iris <- hang.dendrogram(dend_iris,hang_height=0.1)

# reduce the size of the labels:
dend_iris <- assign_values_to_leaves_nodePar(dend_iris, 0.5, "lab.cex")

par(mar = c(3,3,3,7))
plot(dend_iris,
     main = "Clustered Iris dataset
     (the labels give the true flower species)",
     horiz =  TRUE,  nodePar = list(cex = .007))
legend("topleft", legend = iris_species, fill = rainbow_hcl(3))
a= dend_iris[[1]]
dend_iris1 <- color_branches(a,k = 3)
plot(dend_iris1)

# str(dendrapply(d2, unclass))
# unclass(d1)

}
}
\author{
Tal Galili, extensively based on code by jefferis
}
\seealso{
\code{\link[dendextend]{cutree}},\code{\link{dendrogram}},
\code{\link{hclust}}, \code{\link{labels_colors}}
}

