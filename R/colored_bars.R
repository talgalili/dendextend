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


# source: http://stackoverflow.com/questions/5468280/scale-a-series-between-two-points-in-r
# from the {scales} package
# library("scales"); rescale
rescale <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE)) 
{
   if (zero_range(from) || zero_range(to)) 
      return(rep(mean(to), length(x)))
   (x - from[1])/diff(from) * diff(to) + to[1]
}



#' @export
colored_bars <- function(dend, colors, rowLabels = NULL, cex.rowLabels = 0.9, 
                       add = TRUE, 
                       y_scale = 1, y_shift = 0,
                       text_shift = 1,
                       #below_labels = TRUE,
                       ...) 
{
   # make sure we are working with a dend:
   if(!is.dendrogram(dend)) dend <- as.dendrogram(dend)
   
   # get labels' order:
   dend_order <- order.dendrogram(dend)   
   # moving the y location and scale of the bars
   # this allows us to have it underneath the dend
   # in a way that would look nice.
#    if(missing(y_scale)) y_scale <- 1
   
   # this makes sure that the original shift we had is always compared to the top of the bars (instead of the bottom)
   # the if-else lowers the y further, to make sure it is below the labels.
   y_shift <- y_shift-y_scale
#    - if(below_labels) max(strheight(labels(dend))) else 0
   
   colors <- as.matrix(colors);
   dimC <- dim(colors)
   
   if (is.null(rowLabels) & (length(dimnames(colors)[[2]])==dimC[2])) rowLabels = names(as.data.frame(colors));
   
   op <- options()
   pr <- par()
   
   options(stringsAsFactors=FALSE);   
   
   if (length(dend_order) != dimC[1] ) 
      stop("ERROR: length of colors vector not compatible with number of objects in the hierarchical tree.");
   nSets <- dimC[2];
   C <- colors[dend_order, ]; 
   step <- 1/(dimC[1]-1);
   ystep <- 1/nSets;
   
   old_mar <- par()$mar
   par(mar = c(0, 4.1, 0, 2.1))
   if(!add) {
      barplot(height=1, col = "white", border=FALSE, space=0, axes=FALSE, ...)
   }
   

   charWidth <- strwidth("W")/2;
   for (j in 1:nSets)
   {
      ind <- (1:dimC[1]);
      xl <- (ind-1.5) * step; xr = (ind-0.5) * step; 
      yb <- rep(ystep*(j-1), dimC[1]); yt = rep(ystep * j, dimC[1]);
      
      if(add) {
         n_leaves <- length(dend_order)
         xl <- rescale(xl, to = c(1-.5, n_leaves-.5))
         xr <- rescale(xl, to = c(1+.5, n_leaves+.5))
         yb <- yb*y_scale + y_shift
         yt <- yt*y_scale + y_shift         
      }
      
      if (is.null(dim(C))) {
         rect(xl, yb, xr, yt, col = as.character(C), border = as.character(C));
      } else {
         rect(xl, yb, xr, yt, col = as.character(C[,j]), border = as.character(C[,j]));
      }
      if (is.null(rowLabels))
      {
         text(as.character(j), pos=2, x=  charWidth*text_shift, y=ystep*(j-0.5)*y_scale + y_shift, cex=cex.rowLabels, xpd = TRUE);
#          text(as.character(j), pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      } else {
         text(rowLabels[j], pos=2, x=  charWidth*text_shift, y=ystep*(j-0.5)*y_scale + y_shift, cex=cex.rowLabels, xpd = TRUE);
#          text(rowLabels[j], pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      } 
   }
   # we start j from 0 so that it will add a line below the bars (and not just above them)
   for (j in 0:nSets) {
      the_x <- rescale(c(0,1), to = c(1-.5, n_leaves+.5))
      lines(x=the_x, y=c(ystep*j,ystep*j)*y_scale + y_shift);
   }
   
   options(op)     # reset (all) initial options   
   par(mar = old_mar)
   
   return(C)
}



if(FALSE) {

rows_picking <- c(1:5, 25:30)
dend <- (iris[rows_picking,-5]*10) %>% dist %>% hclust %>% as.dendrogram 
odd_numbers <- rows_picking %% 2
cols <- c("gold", "grey")[odd_numbers+1]
# scale is off
plot(dend)
colored_bars(dend, cols)
# move and scale a bit
plot(dend)
colored_bars(dend, cols, y_shift = -1,
           rowLabels = "Odd\n numbers")
# Now let's cut the tree and add that info to the plot:
k2 <- cutree(dend, k = 2)
cols2 <- c("#0082CE", "#CC476B")[k2]
plot(dend)
# sadly, the shift paramteres need to be handled manually...
colored_bars(dend, cbind(cols2, cols), y_shift = -1,
           rowLabels = c("2 clusters", "Odd numbers"),
            text_shift = 1)

# let's add clusters color
# notice how we need to play with the colors a bit
# this is because color_branches places colors from
# left to right. Which means we need to give colored_bars
# the colors of the items so that ofter sorting they would be
# from left to right. Here is how it can be done:
the_k <- 3
cols3 <- rainbow_hcl(the_k, c=90, l=50)
dend %>% update("branches_k_color", k = the_k, with = cols3) %>% plot

kx <- cutree(dend, k = the_k)
ord <- order.dendrogram(dend)
kx  <- sort_levels_values(kx[ord])   
kx  <- kx[match(seq_along(ord), ord)]

colored_bars(dend, cbind(cols3[kx], cols2, cols), 
           y_shift = -1, y_scale = 1.4,
           rowLabels = c("3 clusters", "2 clusters", "Odd numbers"),
           text_shift = 1)



colored_bars(dend, the_cols[cutree(dend, k = the_k, sort_cluster_numbers = T)], y_shift = -2)
plot(1:5, pch = 19, cex = 2, col = the_cols)

cutree(dend, k = the_k)[order.dendrogram(dend)]

kx <- cutree(dend, k = the_k)
ord <- order.dendrogram(dend)
kx  <- sort_levels_values(kx[ord])   
kx  <- kx[match(seq_along(ord), ord)]

require(colorspace)
dend %>% update("branches_k_color", k = the_k, with = the_cols) %>% plot
colored_bars(dend, cols3[kx], y_shift = -2)




# of course the pattern we see is just fake.

k4 <- cutree(dend, k = 4)
plot(dend)
colored_bars(dend, k4)
require(colorspace)
dend %>% update("branches_k_color", k = 4) %>% plot
colored_bars(dend, rainbow_hcl(4)[c(4,2,3,1)][k4], y_shift = -2)

the_k <- 5
the_cols <- rainbow_hcl(the_k, c=90, l=50)
dend %>% update("branches_k_color", k = the_k, with = the_cols) %>% plot
colored_bars(dend, the_cols[cutree(dend, k = the_k, sort_cluster_numbers = T)], y_shift = -2)
plot(1:5, pch = 19, cex = 2, col = the_cols)

cutree(dend, k = the_k)[order.dendrogram(dend)]

kx <- cutree(dend, k = the_k)
ord <- order.dendrogram(dend)
kx  <- sort_levels_values(kx[ord])   
kx  <- kx[match(seq_along(ord), ord)]

require(colorspace)
dend %>% update("branches_k_color", k = the_k, with = the_cols) %>% plot
colored_bars(dend, the_cols[kx], y_shift = -2)

the_cols <- apply(cutree(dend, k = 2:5), 2, function(x) heat_hcl(5)[x])

plot(dend, las = 1)
colored_bars(dend, the_cols)
plot(dend, las = 1)
colored_bars(dend, the_cols,y_scale = 3, y_shift = -2)

}