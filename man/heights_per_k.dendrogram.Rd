\name{heights_per_k.dendrogram}
\alias{dendextend_heights_per_k.dendrogram}
\alias{heights_per_k.dendrogram}
\title{Which height will result in which k for a dendrogram}
\usage{
heights_per_k.dendrogram(tree, ...)
}
\arguments{
  \item{tree}{a dendrogram.}

  \item{...}{not used.}
}
\value{
a vector of heights, with its names being the k clusters
that will result for cutting the dendrogram at each height.
}
\description{
Which height will result in which k for a dendrogram
}
\examples{
\dontrun{
hc <- hclust(dist(USArrests[1:4,]), "ave")
dend <- as.dendrogram(hc)
heights_per_k.dendrogram(dend)
##       1        2        3        4
##86.47086 68.84745 45.98871 28.36531

cutree(hc, h = 68.8) # and indeed we get 2 clusters

unbranch_dend <- unbranch(dend,2)
plot(unbranch_dend)
heights_per_k.dendrogram(unbranch_dend)
       #1        3        4
       #97.90023 57.41808 16.93594
       # we do NOT have a height for k=2 because of the tree's structure.



# speed gains:
dat1 <- iris[1:150,-5]
# dat1 <- rbind(dat1,dat1,dat1,dat1,dat1,dat1,dat1)
dend_big = as.dendrogram(hclust(dist(dat1)))
require(microbenchmark)
require(dendextendRcpp)
microbenchmark(dendextend_heights_per_k.dendrogram(dend_big),
               heights_per_k.dendrogram(dend_big),
               dendextendRcpp::heights_per_k.dendrogram(dend_big),
               times = 10)
# ~126 times faster!
}
}

