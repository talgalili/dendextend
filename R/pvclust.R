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




#' @title Last Observation Carried Forward
#' @export
#' @description
#' A function for replacing each NA with the most recent non-NA prior to it.
#' @param x some vector
#' @param first_na_value If the first observation is NA, fill it with "first_na_value"
#' @param recursive logical (TRUE). Should na_locf be re-run until all NA values are filled?
#' @param ... ignored.
#' @return 
#' The original vector, but with all the missing values filled by the value 
#' before them.
#' @seealso 
#' \link[zoo]{na.locf}
#' 
#' @source
#' \url{https://stat.ethz.ch/pipermail/r-help/2003-November/042126.html}
#' \url{http://stackoverflow.com/questions/5302049/last-observation-carried-forward-na-locf-on-panel-cross-section-time-series}
#' 
#' This could probably be solved MUCH faster using Rcpp.
#' 
#' @examples
#' na_locf(c(NA, NA))
#' na_locf(c(1, NA))
#' na_locf(c(1, NA, NA, NA))
#' na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4))
#' na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), recursive = FALSE)
#' 
#' \dontrun{
#' 
#' library(microbenchmark)
#' library(zoo)
#' 
#' microbenchmark(
#'    na_locf = na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4)),
#'    na.locf = na.locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4))
#' ) # my implementation is 6 times faster :)
#' 
#' microbenchmark(
#'    na_locf = na_locf(rep(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), 1000)),
#'    na.locf = na.locf(rep(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), 1000))
#' ) # my implementation is 3 times faster 
#' 
#' }
#' 
#' 
na_locf <- function(x, first_na_value = 0, recursive = TRUE, ...) {   
   # where are the NA's:
   x_na <- is.na(x)
   
   # IF we have no NA's, return x:
   if(!any(x_na)) {
      return(x)
      # Else - fill one next observation:
   } else {
      # If the first observation is NA, fill it with "first_na_value"
      if(x_na[1]) x[1] <- first_na_value
      
      x_na_loc <- which(x_na)
      x[x_na_loc] <- x[x_na_loc-1]
      
      if(recursive) {
         Recall(x)         
      } else {
         return(x)
      }      
   }   
}




# 
# 
# 
# if(F) {
#    
#    
#    # # require2(pvclust, F)
#    # 
#    # 
#    library(dendextend)
#    
#    
#    library(pvclust)
#    data(lung) # 916 genes for 73 subjects
#    set.seed(13134)
#    result <- pvclust(lung[, 1:20], method.dist="cor", method.hclust="average", nboot=100)
#    
#    
#    plot(result)
#    pvrect(result, alpha=0.95)
#    
#    # pvclust:::plot.pvclust
#    # pvclust:::text.pvclust
#    
#    # reproduce:
#    hc <- result[[1]]
#    plot(hc)
#    pvclust:::text.pvclust(result)
#    pvrect(result, alpha=0.95)
#    
#    # reproduce with a dendrogram:
#    dend <- as.dendrogram(hc)
#    dend %>% hang.dendrogram %>% plot
#    pvclust:::text.pvclust(result)
#    pvrect(result, alpha=0.95)
#    
#    # str(result)
#    
#    # Reproduce with a dendrogram, but with information in the branches!):
#    
#    # these two are sorted from node number 1 to nnodes.
#    result$edges$au
#    result$edges$bp
#    get_branches_heights(dend, sort = FALSE)
#    ord <- rank(get_branches_heights(dend, sort = FALSE))
#    au_by_nodes <- result$edges$au[ord]
#    bp_by_nodes <- result$edges$bp[ord]
#    
#    # plot, but ignore the leaves:
#    nnodes(dend)-nleaves(dend) # number of branches nodes
#    alpha <- .05
#    signif_bp  <- bp_by_nodes > (1-alpha)
#    signif_bp[1] <- FALSE
#    lwd_signif_bp <- ifelse(signif_bp, 5, 1)
#    assign_values_to_branches_edgePar(dend, lwd_signif_bp, "lwd", skip_leaves = TRUE) %>% plot
#    pvclust:::text.pvclust(result)
#    pvrect(result, alpha=0.95,pv = "bp")
#    
#    
#    # Let's do it again, but include the leaves this time!
#    
#    lwd_signif_bp_with_leaves <- rep(NA, nnodes(dend))
#    ss_leaf <- which_leaf(dend)
#    lwd_signif_bp_with_leaves[!ss_leaf] <- lwd_signif_bp
#    lwd_signif_bp_with_leaves <- na_locf(lwd_signif_bp_with_leaves)
#    
#    assign_values_to_branches_edgePar(dend, lwd_signif_bp_with_leaves, "lwd") %>% plot
#    pvclust:::text.pvclust(result)
#    pvrect(result, alpha=0.95,pv = "bp")
#    
#    signif_col_fun <- colorRampPalette(c("black", "darkred", "red"))
#    signif_col <- signif_col_fun(100)
#    plot(1:100, col = signif_col, pch = 19)
#    
#    
#    bp_by_all_nodes <- rep(NA, nnodes(dend))
#    ss_leaf <- which_leaf(dend)
#    bp_by_all_nodes[!ss_leaf] <- bp_by_nodes
#    bp_by_all_nodes <- na_locf(bp_by_all_nodes)
#    
#    the_cols <- signif_col[round(bp_by_all_nodes*100)]
#    dend %>% 
#       assign_values_to_branches_edgePar(the_cols, "col") %>% 
#       assign_values_to_branches_edgePar(lwd_signif_bp_with_leaves, "lwd") %>% 
#       plot
#    pvclust:::text.pvclust(result)
#    pvrect(result, alpha=0.95,pv = "bp")
#    
#    
#    
#    
#    #
#    
#    
#    
#    
#    
#    
#    
#    # it numbers the nodes from 1 to nnodes, so that 1 is the lowest node, the next one is after it
#    # and so on. This numbering is interesting. This is something that can be 
#    # manually added to a dend. (as extra attr), or just tracked
#    # 
#    # it saves a vector
#    
#    get_branches_heights(dend, sort = FALSE)
#    
#    # after pvclust - we can get the values and use them!
#    
#    
#    
#    
#    # http://course.sdu.edu.cn/G2S/eWebEditor/uploadfile/20130605012427559.pdf
#    # http://scholar.google.co.il/scholar?cites=3917689774873650154&as_sdt=2005&sciodt=0,5&hl=en
#    # http://www.is.titech.ac.jp/~shimo/pub/Shimodaira%20and%20Hasegawa%20MBE1999.pdf
#    # http://scholar.google.co.il/scholar?hl=en&q=Ryota+Suzuki+&btnG=&as_sdt=1%2C5&as_sdtp=
#    # http://www.is.titech.ac.jp/~shimo/prog/pvclust/
#    # http://cran.r-project.org/web/packages/pvclust/index.html
#    
# }
# 
