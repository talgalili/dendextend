\name{entanglement}
\alias{entanglement}
\alias{entanglement.default}
\alias{entanglement.dendrogram}
\alias{entanglement.hclust}
\alias{entanglement.phylo}
\title{Measures entanglement between two trees}
\usage{
  entanglement(tree1, tree2, ...)

  \method{entanglement}{dendrogram}(tree1, tree2, L = 1.5,
    leaves_matching_method = c("labels", "order"),...)

  \method{entanglement}{hclust}(tree1, tree2, ...)

  \method{entanglement}{phylo}(tree1, tree2, ...)
}
\arguments{
  \item{tree1}{a tree object (of class
  dendrogram/hclust/phylo).}

  \item{tree2}{a tree object (of class
  dendrogram/hclust/phylo).}

  \item{L}{the distance norm to use for measuring the
  distance between the two trees. It can be any positive
  number, often one will want to use 0, 1, 1.5, 2 (see
  'details' for more).}

  \item{leaves_matching_method}{a character scalar, either
  "order" (default) or "labels". If using "labels", then we
  use the labels for matching the leaves order value. And
  if "order" then we use the old leaves order value for
  matching the leaves order value.

  Using "order" is faster, but "labels" is safer. "order"
  will assume that the original two trees had their labels
  and order values MATCHED.

  Hence, it is best to make sure that the trees used here
  have the same labels and the SAME values matched to these
  values - and then use "order" (for fastest results).}

  \item{...}{not used}
}
\value{
  The number of leaves in the tree
}
\description{
  Measures the entanglement between two trees. Entanglement
  is a measure between 1 (full entanglement) and 0 (no
  entanglement). The exact behavior of the number depends
  on the L norm which is chosen.
}
\details{
  Entanglement is measured by giving the left tree's labels
  the values of 1 till tree size, and than match these
  numbers with the right tree. Now, entanglement is the L
  norm distance between these two vectors. That is, we take
  the sum of the absolute difference (each one in the power
  of L). e.g: \code{sum(abs(x-y)**L)}. And this is devided
  by the "worst case" entanglement level (e.g: when the
  right tree is the complete reverse of the left tree).

  L tells us which panelty level we are at (L0, L1, L2,
  partial L's etc). L>1 means that we give a big panelty
  for sharp angles. While L->0 means that any time
  something is not a streight horizontal line, it gets a
  large penalty If L=0.1 it means that we much prefer
  streight lines over non streight lines
}
\examples{
hc <- hclust(dist(USArrests[1:5,]), "ave")
dend <- as.dendrogram(hc)

nleaves(dend) # 5
nleaves(hc) # 5
}
\seealso{
  \link{untangle}, \link{tanglegram},
  \link{match_order_by_labels}.
}

