\name{get_branches_attr}
\alias{get_branches_attr}
\title{get attributes from the dendrogram's root branches}
\usage{
  get_branches_attr(dend, the_attr, ...)
}
\arguments{
  \item{dend}{dendrogram object}

  \item{the_attr}{the attribute to get from the branches
  (for example "height")}

  \item{...}{passed on to attr}
}
\value{
  The attributes of the branches of the dendrogram's root
}
\description{
  get attributes from the dendrogram's root branches
}
\examples{
hc <- hclust(dist(USArrests[2:9,]), "com")
dend <- as.dendrogram(hc)

get_branches_attr(dend, "height") # 0.00000 71.96247
str(dend, 2)
}
\seealso{
  \link{attr}
}

