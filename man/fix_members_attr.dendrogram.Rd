\name{fix_members_attr.dendrogram}
\alias{fix_members_attr.dendrogram}
\title{Fix members attr in a dendrogram}
\usage{
  fix_members_attr.dendrogram(dend, ...)
}
\arguments{
  \item{dend}{a dendrogram object}

  \item{...}{not used}
}
\value{
  A dendrogram, after adjusting the members attr in all of
  its nodes.
}
\description{
  Fix members attr in a dendrogram after (for example), the
  tree was trimmed or manipulated.
}
\examples{
# define dendrogram object to play with:
hc <- hclust(dist(USArrests[1:3,]), "ave")
dend <- as.dendrogram(hc)
# plot(dend)
# trim one leaf
dend[[2]] <- dend[[2]][[1]]
# plot(dend)
dend # but it is NO LONGER true that it has 3 members total!
fix_members_attr.dendrogram(dend) # it now knows it has only 2 members :)

hc <- hclust(dist(USArrests[1:3,]), "ave")
dend <- as.dendrogram(hc)

identical(trim_leaf(dend , "Alaska"), fix_members_attr.dendrogram(trim_leaf(dend , "Alaska")))
str(unclass(trim_leaf(dend , "Alaska")))
str(unclass(fix_members_attr.dendrogram(trim_leaf(dend , "Alaska"))))
}

