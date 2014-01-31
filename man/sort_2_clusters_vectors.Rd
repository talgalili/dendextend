\name{sort_2_clusters_vectors}
\alias{sort_2_clusters_vectors}
\title{Sorts two clusters vector by their names}
\usage{
sort_2_clusters_vectors(A1_clusters, A2_clusters,
  assume_sorted_vectors = FALSE, warn = TRUE, ...)
}
\arguments{
  \item{A1_clusters}{a numeric vector of cluster grouping
  (numeric) of items, with a name attribute of item name
  for each element from group A1. These are often obtained
  by using some k cut on a dendrogram.}

  \item{A2_clusters}{a numeric vector of cluster grouping
  (numeric) of items, with a name attribute of item name
  for each element from group A2. These are often obtained
  by using some k cut on a dendrogram.}

  \item{assume_sorted_vectors}{logical (FALSE). Can we
  assume to two group vectors are sorter so that they have
  the same order of items? IF FALSE (default), then the
  vectors will be sorted based on their name attribute.}

  \item{warn}{logical (TRUE). Should a warning be issued in
  case of problems?}

  \item{...}{Ignored.}
}
\value{
A list with two elements, corrosponding two the two
clustering vectors.
}
\description{
Sorts two clusters vector by their names
}
\examples{
\dontrun{

set.seed(23235)
ss <- sample(1:150, 4 )
hc1 <- hclust(dist(iris[ss,-5]), "com")
hc2 <- hclust(dist(iris[ss,-5]), "single")
# dend1 <- as.dendrogram(hc1)
# dend2 <- as.dendrogram(hc2)
#    cutree(dend1)

A1_clusters <- cutree(hc1, k=3)
A2_clusters <- sample(cutree(hc1, k=3))

sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors =TRUE) # no sorting
sort_2_clusters_vectors(A1_clusters, A2_clusters, assume_sorted_vectors =FALSE) # Sorted


}
}
\seealso{
\link{FM_index_profdpm}
}

