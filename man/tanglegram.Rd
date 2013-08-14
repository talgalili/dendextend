\name{tanglegram}
\alias{dendbackback}
\alias{tanglegram}
\alias{tanglegram.default}
\alias{tanglegram.dendrogram}
\alias{tanglegram.hclust}
\alias{tanglegram.phylo}
\title{Tanglegram plot}
\source{
  The function is based on code from plannapus, after major
  revisions. See:
  \url{http://stackoverflow.com/questions/12456768/duelling-dendrograms-in-r-placing-dendrograms-back-to-back-in-r}
}
\usage{
  tanglegram(tree1, ...)

  \method{tanglegram}{dendrogram}(tree1, tree2 , sort =
    FALSE, color_lines = "darkgrey", lwd = 3.5,
    columns_width = c(5,3,5), margin_top = 3, margin_bottom
    = 2.5, margin_inner = 1.8, margin_outer = 0.5,
    left_dendo_mar =
    c(margin_bottom,margin_outer,margin_top,margin_inner),
    right_dendo_mar =
    c(margin_bottom,margin_inner,margin_top,margin_outer),
    intersecting = TRUE, dLeaf = NULL, axes = TRUE, type =
    "r", # can also be "t" lab.cex = 1, remove_nodePar =F,
    main_left = "", main_right = "", k_labels = NULL,
    k_branches = NULL, ...)

  \method{tanglegram}{hclust}(tree1, ...)

  \method{tanglegram}{phylo}(tree1, ...)
}
\arguments{
  \item{tree1}{tree object (dendrogram/hclust/phylo),
  plotted on the left}

  \item{tree2}{tree object (dendrogram/hclust/phylo),
  plotted on the right}

  \item{sort}{logical (FALSE). Should the dendrogram's
  labels be "sorted"? (might give a better tree in some
  cases).}

  \item{color_lines}{a vector of colors for the lines
  connected the labels. If the colors are shorter than the
  number of labels, they are recycled (and a warning is
  issued).}

  \item{lwd}{width of the lines connecting the labels.}

  \item{edge.lwd}{width of the dendrograms lines.}

  \item{columns_width}{a vector with three elements, giving
  the relative sizes of the the three plots (left
  dendrogram, connecting lines, right dendrogram). This is
  passed to \link{layout}.}

  \item{margin_top}{the number of lines of margin to be
  specified on the top of the plots.}

  \item{margin_bottom}{the number of lines of margin to be
  specified on the bottom of the plots.}

  \item{margin_inner}{margin_bottom the number of lines of
  margin to be specified on the inner distence between the
  dendrograms and the connecting lines.}

  \item{margin_outer}{margin_bottom the number of lines of
  margin to be specified on the outer distence between the
  dendrograms and the connecting lines.}

  \item{left_dendo_mar}{mar parameters of the left
  dendrgoram.}

  \item{right_dendo_mar}{mar parameters of the right
  dendrgoram.}

  \item{intersecting}{logical (TRUE). Should the leaves of
  the two dendrograms be trimmed so that the two trees will
  have the same labels?}

  \item{dLeaf}{a number specifying the distance in user
  coordinates between the tip of a leaf and its label. If
  NULL, as per default, 3/4 of a letter width or height is
  used.

  Notice that if we are comparing two dendrograms with
  different heights, manually changing dLeaf will affect
  both trees differently. In such a case, it is recommanded
  to manually change dLeaf_left and dLeaf_right. This can
  be especially important when changing the lab.cex of the
  dendrogram's labels. Alternatively, one could manually
  set the xlim parameter for both trees, which will force
  the proportion of distances of the labels from the trees
  to remain the same.}

  \item{dLeaf_left}{dLeaf of the left dendrogram, by
  default it is equal to dLeaf (often negative).}

  \item{dLeaf_right}{dLeaf of the right dendrogram, by
  default it is equal to minus dLeaf (often positive).}

  \item{axes}{logical (TRUE). Should plot axes be plotted?}

  \item{type}{type of plot ("t"/"r" = triangle or
  rectangle)}

  \item{lab.cex}{numeric scalar, influanicing the cex size
  of the labels.}

  \item{remove_nodePar}{logical (FALSE). Should the nodePar
  of the leaves be removed? (useful when the trees' leaves
  have too many parameters on them)}

  \item{main}{Character. Title above the connecting lines.}

  \item{main_left}{Character. Title of the left
  dendrogram.}

  \item{main_right}{Character. Title of the right
  dendrogram.}

  \item{k_labels}{integer. Number of groups by which to
  color the leaves.}

  \item{k_branches}{integer. Number of groups by which to
  color the branches.}

  \item{rank_branches}{logical (FALSE). Should the branches
  heights be adjusted? (setting this to TRUE - can make it
  easier for comparing topological differences)}

  \item{...}{not used.}
}
\value{
  invisible(NULL)
}
\description{
  Counts the number of leaves in a tree (dendrogram or
  hclust).
}
\details{
  Notice that tanglegram does not "resize" well. In case
  you are resizing your window you would need to re-run the
  function.
}
\examples{
\dontrun{
set.seed(23235)
ss <- sample(1:150, 10 )
hc1 <- hclust(dist(iris[ss,-5]), "com")
hc2 <- hclust(dist(iris[ss,-5]), "single")
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
tanglegram(dend1 , dend2)
tanglegram(dend1 , dend2, sort = TRUE)
tanglegram(dend1 , dend2, remove_nodePar = TRUE)
tanglegram(dend1 , dend2, k_labels = 6, k_branches = 4)

tanglegram(dend1 , dend2, lab.cex = 2, edge.lwd = 3,
margin_inner= 5, type = "t", center = TRUE)


## works nicely:
tanglegram(dend1 , dend2, lab.cex = 2, edge.lwd = 3,
margin_inner= 3.5, type = "t", center = TRUE,
dLeaf = -0.1, xlim = c(7,0),
k_branches=3)


# using rank_branches can make the comparison even easier
tanglegram(rank_branches(dend1) , rank_branches(dend2), lab.cex = 2, edge.lwd = 3,
 margin_inner= 3.5, type = "t", center = TRUE,
  dLeaf = -0.1, xlim = c(5.1,0), columns_width= c(5,1,5),
   k_branches=3)

}
}
\author{
  Tal Galili, plannapus
}
\seealso{
  \link{remove_leaves_nodePar},
  \link{plot_horiz.dendrogram}, \link{rank_branches}
}

