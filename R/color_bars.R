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



color_bars <- function(dendro, colors, rowLabels = NULL, cex.rowLabels = 0.9, ...) 
{

   if(!is.dendrogram(dendro)) dendro <- as.dendrogram(dendro)
   
   dend_order <- order.dendrogram(dendro)
   
   
   colors = as.matrix(colors);
   dimC = dim(colors)
   
   if (is.null(rowLabels) & (length(dimnames(colors)[[2]])==dimC[2])) rowLabels = names(as.data.frame(colors));
   
   options(stringsAsFactors=FALSE);
   if (length(dend_order) != dimC[1] ) 
      stop("ERROR: length of colors vector not compatible with number of objects in the hierarchical tree.");
   nSets = dimC[2];
   C = colors[dend_order, ]; 
   step = 1/(dimC[1]-1);
   ystep = 1/nSets;
   barplot(height=1, col = "white", border=FALSE, space=0, axes=FALSE, ...)
   charWidth = strwidth("W")/2;
   for (j in 1:nSets)
   {
      ind = (1:dimC[1]);
      xl = (ind-1.5) * step; xr = (ind-0.5) * step; 
      yb = rep(ystep*(j-1), dimC[1]); yt = rep(ystep * j, dimC[1]);
      if (is.null(dim(C))) {
         rect(xl, yb, xr, yt, col = as.character(C), border = as.character(C));
      } else {
         rect(xl, yb, xr, yt, col = as.character(C[,j]), border = as.character(C[,j]));
      }
      if (is.null(rowLabels))
      {
         text(as.character(j), pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      } else {
         text(rowLabels[j], pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      } 
   }
   for (j in 1:nSets) lines(x=c(0,1), y=c(ystep*j,ystep*j));
}
