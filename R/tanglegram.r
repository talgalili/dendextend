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


# tanglegram.r




# source for this code: http://stackoverflow.com/questions/12456768/duelling-dendrograms-in-r-placing-dendrograms-back-to-back-in-r

tanglegram <- function (...) UseMethod("tanglegram")


tanglegram.default <- function (object, ..., value) stop("no default function for tanglegram-")


tanglegram.dendrogram <- function(dendo1,dendo2 , sort = F, 
                                   color_lines, lwd = 1.2,
                                   columns_width = c(5,2,3,2,5),
                                   margin_top = 3,
                                   margin_bottom = 3,
                                   left_dendo_mar = c(margin_bottom,0,margin_top,0),
                                   right_dendo_mar=c(margin_bottom,0,margin_top,0),
                                   characters_to_trim = NULL,
                                   dLeaf = 0,
                                   axes = F, 
                                   dendo_type = "r", # can also be "t"
                                   text_cex = 1,
                                   remove_nodePar =T,
                                   main_left = "",
                                   main_right = ""																	 
)
{
   # characters_to_trim = the number of characters to leave after trimming the labels.		
   # remove_nodePar = makes sure that we won't have any dots at the end of leaves
   # Future TODO:
   
   # remove colors from the tips of leaves
   if(remove_nodePar) {
      colLab_0 <- function(n) {
         if(is.leaf(n)) {
            a <- attributes(n)			
            attr(n, "nodePar") <- NULL
         }
         n
      }
      dendo1 <- dendrapply(dendo1, colLab_0)
      dendo2 <- dendrapply(dendo2, colLab_0)
   }
   # sort them for better graph
   if(sort == T) {	# based on the "rotate.dendrogram" function
      dendo1 <- sort(dendo1)
      dendo2 <- sort(dendo2)
   }
   
   # using this we lose some meta-data on hclust, but nothing majour...
   # 	hc1 <- as.hclust(dendo1)
   # 	hc2 <- as.hclust(dendo2)
   
   # dendo1$order
   
   l <- length(order.dendrogram(dendo1))
   # The matrix to draw the arrows:
   ord_arrow <- cbind((1:l)[order(order.dendrogram(dendo1))],(1:l)[order(order.dendrogram(dendo2))]) 
   # 	ord_arrow <- cbind((1:l)[(order.dendrogram(dendo1))],(1:l)[(order.dendrogram(dendo2))]) 
   # 	ord_arrow <- cbind(order.dendrogram(dendo1),order.dendrogram(dendo2)) 
   # 	labels(dendo1)
   # The two vectors of ordered leave labels:
   leaves1 <- labels(dendo1)
   leaves2 <- labels(dendo2)
   
   if(!is.null(characters_to_trim) & is.numeric(characters_to_trim)) {
      # find out where we will trim
      ss_leaves1_add... <- nchar(leaves1) > characters_to_trim
      ss_leaves2_add... <- nchar(leaves2) > characters_to_trim
      
      leaves1 <- substr(leaves1,1,characters_to_trim)
      leaves2 <- substr(leaves2,1,characters_to_trim)		
      
      leaves1[ss_leaves1_add...] <- paste(leaves1[ss_leaves1_add...], "...", sep = "")
      leaves2[ss_leaves2_add...] <- paste(leaves2[ss_leaves2_add...], "...", sep = "")			
      leaves1[!ss_leaves1_add...] <- paste(leaves1[!ss_leaves1_add...], "   ", sep = "")
      leaves2[!ss_leaves2_add...] <- paste(leaves2[!ss_leaves2_add...], "   ", sep = "")			
   }
   
   # 	labels(dendo1)[order.dendrogram(dendo1)]->leaves1
   # 	labels(dendo2)[order.dendrogram(dendo2)]->leaves2
   # 	cbind(leaves1, leaves2)
   
   # And the plot:
   # Set the layout of the plot elements
   layout(matrix(1:5,nrow=1),width=columns_width)
   #par(mfrow = c(1,2))
   
   # The first dendrogram:	
   par(mar=left_dendo_mar)
   plot(dendo1,horiz=TRUE,leaflab="none", ylim=c(0,l),
        dLeaf = dLeaf, type = dendo_type, axes = axes,
        main = main_left
   )
   # plot(dendo1,horiz=TRUE,ylim=c(0,l))
   
   # The first serie of labels (i draw them separately because, for the second serie, I didn't find a simple way to draw them nicely on the cluster):
   par(mar=c(margin_bottom,0,margin_top,0))
   plot(NA, bty="n",axes=FALSE,xlim=c(0,1), ylim=c(0,l),ylab="",xlab="")
   sapply(1:l,function(x)text(x=0,y=x,labels=leaves1[x], pos=4, cex=text_cex, offset = 0))
   
   # The arrows:
   # arros colors:
   if(missing(color_lines)) color_lines <- rep("blue", l)
   if(length(color_lines) < l) color_lines <- rep.int(color_lines, l)
   color_lines <- color_lines[ord_arrow[,1]]	
   
   par(mar=c(margin_bottom,0,margin_top,0))
   plot(NA, bty="n",axes=FALSE,xlim=c(0,1), ylim=c(0,l),ylab="",xlab="")
   col_indx <- 0
   apply(ord_arrow,1,
         function(x){
            col_indx <<- col_indx + 1
            arrows(0,x[1],1,x[2],code=3, length=0.05, col= color_lines[col_indx], lwd = lwd)
         }
   )
   
   # The second serie of labels:
   par(mar=c(margin_bottom,0,margin_top,0))
   plot(NA, bty="n",axes=FALSE, xlim=c(0,1), ylim=c(0,l), ylab="",xlab="")
   sapply(1:l,function(x)text(x=1,y=x,labels=leaves2[x], pos=2, cex=text_cex, offset = 0))
   
   # And the second dendrogram (to reverse it I reversed the xlim vector:
   par(mar=right_dendo_mar)
   plot(dendo2,horiz=TRUE, xlim=c(0,attr(dendo2,"height")), 
        leaflab="none", ylim=c(0,l), 
        dLeaf = dLeaf, type = dendo_type, axes = axes,
        main = main_right)
   
}


# tanglegram.hclust <- function(hc1,hc2)

dendobackback <- tanglegram.dendrogram # another name for the same function.
# hclustbackback <- tanglegram.hclust # another name for the same function.




