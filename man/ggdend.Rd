% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggdend.R
\name{ggdend}
\alias{ggdend}
\alias{as.ggdend}
\alias{as.ggdend.dendrogram}
\alias{prepare.ggdend}
\alias{ggplot.ggdend}
\alias{ggplot.dendrogram}
\alias{print.ggdend}
\title{Creates dendrogram plot using ggplot.}
\source{
These are extended versions of the functions \link[ggdendro]{ggdendrogram}, \link[ggdendro]{dendro_data} (and the hidden dendrogram_data)
from Andrie de Vries's ggdendro package. The motivation for this fork is the need to add more graphical parameters
to the plotted tree. This required a strong mixter of functions from ggdendro and dendextend (to the point that
it seemed better to just fork the code into its current form)
}
\usage{
ggdend(...)

as.ggdend(dend, ...)

\method{as.ggdend}{dendrogram}(dend, type = c("rectangle", "triangle"), edge.root = FALSE, ...)

prepare.ggdend(data, ...)

\method{ggplot}{ggdend}(
  data = NULL,
  mapping = aes(),
  ...,
  segments = TRUE,
  labels = TRUE,
  nodes = TRUE,
  horiz = FALSE,
  theme = theme_dendro(),
  offset_labels = 0,
  na.rm = TRUE,
  environment = parent.frame()
)

\method{ggplot}{dendrogram}(data, ...)

\method{print}{ggdend}(x, ...)
}
\arguments{
\item{...}{mostly ignored.}

\item{dend}{a \link{dendrogram} tree (to be turned into a ggdend object)}

\item{type}{The type of plot, indicating the shape of the dendrogram.  "rectangle" will draw
rectangular lines, while "triangle" will draw triangular lines.}

\item{edge.root}{currently ignored. One day it might do the following: logical; if true, draw an edge to the root node.}

\item{data, x}{a ggdend class object (passed to ggplot.dendrogram or print.ggdend).}

\item{mapping}{(passed in ggplot.ggdend) Default list of aesthetic mappings to use for plot. If not specified, must be supplied in each layer added to the plot.}

\item{segments}{a logical (TRUE) if to plot the segments (branches).}

\item{labels}{a logical (TRUE) if to plot the labels.}

\item{nodes}{a logical (TRUE) if to plot the nodes (points).}

\item{horiz}{a logical (TRUE) indicating if the dendrogram should be drawn horizontally or not.}

\item{theme}{the ggplot2 theme to use (default is \link{theme_dendro}, can also be NULL
for the default ggplot2 theme)}

\item{offset_labels}{a numeric value to offset the labels from the leaves}

\item{na.rm}{A logical (TRUE) to control removal of missing values. Passed to
\link[ggplot2]{geom_line} and \link[ggplot2]{geom_point}}

\item{environment}{(passed in ggplot.ggdend) deprecated / ignored.}
}
\value{
\itemize{
\item{\code{as.ggdend} - returns an object of class ggdend which is a list with 3 componants: segments, labels, nodes.
Each one contains the graphical parameters from the original dendrogram, but in a tabular form that
can be used by ggplot2+geom_segment+geom_text to create a dendrogram plot.}
\item{\code{prepare.ggdend} - a \code{ggdend} object (after filling it with various default values)}
\item{\code{ggplot.ggdend} - a \link[ggplot2]{ggplot} object}
}
}
\description{
Several functions for creating a dendrogram plot using ggplot2.
The core process is to transform a dendrogram into a ggdend object using as.ggdend,
and then plot it using ggplot. These two steps can be done in one command with either the function
ggplot or ggdend.

The reason we want to have as.ggdend (and not only ggplot.dendrogram), is (1) so that you could
create your own mapping of ggdend and, (2) since as.ggdend might be slow for large trees,
it is probably better to be able to run it only once for such cases.

A ggdend class object is a list with 3 componants: segments, labels, nodes.
Each one contains the graphical parameters from the original dendrogram, but in a tabular form that
can be used by ggplot2+geom_segment+geom_text to create a dendrogram plot.
}
\details{
\code{prepare.ggdend} is used by \code{plot.ggdend} to take the \code{ggdend} object
and prepare it for plotting. This is because the defaults of various parameters in \link{dendrogram}'s
are not always stored in the object itself, but are built-in into the \link{plot.dendrogram} function.
For example, the color of the labels is not (by default) specified in the dendrogram (only if we change it
from black to something else). Hence, when taking the object into a different plotting engine (say ggplot2), we
want to prepare the object by filling-in various defaults.
This function is autmatically invoked within the \code{plot.ggdend} function. You would probably use
it only if you'd wish to build your own ggplot2 mapping.
}
\examples{

\dontrun{

library(dendextend)
# library(ggdendro)
# Create a complex dend:
dend <- iris[1:30, -5] \%>\%
  dist() \%>\%
  hclust() \%>\%
  as.dendrogram() \%>\%
  set("branches_k_color", k = 3) \%>\%
  set("branches_lwd", c(1.5, 1, 1.5)) \%>\%
  set("branches_lty", c(1, 1, 3, 1, 1, 2)) \%>\%
  set("labels_colors") \%>\%
  set("labels_cex", c(.9, 1.2))
# plot the dend in usual "base" plotting engine:
plot(dend)
# Now let's do it in ggplot2 :)
ggd1 <- as.ggdend(dend)
library(ggplot2)
ggplot(ggd1) # reproducing the above plot in ggplot2 :)

# Triangle version:
plot(dend, type = "triangle")
ggd2 <- as.ggdend(dend, type = "triangle")
ggplot(ggd2) 


# More modifications:
labels(dend) <- paste0(labels(dend), "00000")
ggd1 <- as.ggdend(dend)
# Use ylim to deal with long labels in ggplot2
ggplot(ggd1) + ylim(-.4, max(get_branches_heights(dend)))


ggplot(ggd1, horiz = TRUE) # horiz plot in ggplot2
# Adding some extra spice to it...
# creating a radial plot:
ggplot(ggd1) + scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta = "x")
# The text doesn't look so great, so let's remove it:
ggplot(ggd1, labels = FALSE) + scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta = "x")

# This can now be sent to plot.ly - which adds zoom-in abilities, and more.
# Here is how it might look like: https://plot.ly/~talgalili/6/y-vs-x/

## Quick guide:
# install.packages("devtools")
# library("devtools")
# devtools::install_github("ropensci/plotly")
# library(plotly)
# set_credentials_file(...)
# you'll need to get it from here: https://plot.ly/ggplot2/getting-started/

# ggplot(ggd1)
# py <- plotly()
# py$ggplotly()

# And you'll get something like this: https://plot.ly/~talgalili/6/y-vs-x/

# Another example: https://plot.ly/ggplot2/
}
}
\seealso{
\link{dendrogram}, \link{get_nodes_attr}, \link{get_leaves_nodePar},
\link[ggplot2]{ggplot},
\link[ggdendro]{ggdendrogram}, \link[ggdendro]{dendro_data},
}
\author{
Tal Galili, using code modified from Andrie de Vries
}
