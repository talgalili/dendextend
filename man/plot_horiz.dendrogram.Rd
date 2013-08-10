\name{plot_horiz.dendrogram}
\alias{plot_horiz.dendrogram}
\title{Plotting a left-tip-adjusted horizontal dendrogram}
\source{
  This function is based on replicating
  \link{plot.dendrogram}. In fact, I'd be happy if in the
  future, some tweaks could be make to
  \link{plot.dendrogram}, so that it would replace the need
  for this function.
}
\usage{
  plot_horiz.dendrogram(x, center = FALSE,
    edge.root = is.leaf(x) || !is.null(attr(x, "edgetext")),
    dLeaf = NULL, horiz = TRUE, xaxt = "n", yaxt = "s",
    xlim, ylim, side = FALSE, ...)
}
\arguments{
  \item{x}{tree object (dendrogram)}

  \item{center}{logical; if TRUE, nodes are plotted
  centered with respect to the leaves in the branch.
  Otherwise (default), plot them in the middle of all
  direct child nodes.}

  \item{edge.root}{logical; if true, draw an edge to the
  root node.}

  \item{dLeaf}{a number specifying the distance in user
  coordinates between the tip of a leaf and its label. If
  NULL as per default, 3/4 of a letter width or height is
  used.}

  \item{horiz}{logical indicating if the dendrogram should
  be drawn horizontally or not. In this function it MUST be
  TRUE!}

  \item{xaxt}{graphical parameters, or arguments for other
  methods.}

  \item{yaxt}{graphical parameters, or arguments for other
  methods.}

  \item{xlim}{optional x- and y-limits of the plot, passed
  to plot.default. The defaults for these show the full
  dendrogram.}

  \item{ylim}{optional x- and y-limits of the plot, passed
  to plot.default. The defaults for these show the full
  dendrogram.}

  \item{side}{logical (FALSE). Should the tips of the drawn
  tree be facing the left side. This is the important
  feature of this function.}

  \item{...}{passed to \link{plot.dendrogram}.}
}
\value{
  The invisiable dLeaf value.
}
\description{
  The default \code{plot(dend, horiz = TRUE)}, gives us a
  dendrogram tree plot with the tips turned right. The
  current function enables the creation of the same tree,
  but with the tips turned left. The main challange in
  doing this is finding the distance of the labels from the
  leaves tips - which is solved with this function.
}
\examples{
\dontrun{
dend <- as.dendrogram(hclust(dist(USArrests[1:10,])))

par(mfrow =c(1,2), mar = rep(6,4))
plot_horiz.dendrogram(dend)
plot_horiz.dendrogram(dend, side=TRUE)
# plot_horiz.dendrogram(dend, side=TRUE, dLeaf= 0)
# plot_horiz.dendrogram(dend, side=TRUE, nodePar = list(pos = 1))
# sadly, lab.pos is not implemented yet,
## so the labels can not be right aligned...
}
}
\seealso{
  \link{plot.dendrogram}, \link{tangelgram}
}

