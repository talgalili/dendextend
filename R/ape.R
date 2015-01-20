# Copyright (C) Tal Galili
#
# This file is part of dendextend.
#
# dendextend is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# dendextend is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#




# as.dendrogram(as.hclust(as.phylo(hc)))
#' @export
# ' @S3method as.dendrogram phylo
as.dendrogram.phylo <- function(object,...) {
	library(ape)
	as.dendrogram(as.hclust(object))
}


# We can't use an S3 here without switching ape to be a depend/imports, and enhance is more fitting. So I must make this function visible... :\
# ' @S3method as.phylo dendrogram
# ' @method as.phylo dendrogram
#' @export
as.phylo.dendrogram <- function(x,...) {
	library(ape)
	ape::as.phylo.hclust(as.hclust(x))
}
## http://stackoverflow.com/questions/13085481/namespace-dependencies-not-required
## I also added ape to "Imports" in DESCRIPTION in order to avoid the error:
##    Namespace dependency not required: 'ape'
## Update: I removed ape from the imports and NAMESPACE.
