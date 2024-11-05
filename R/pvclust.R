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
#' \url{https://stackoverflow.com/questions/5302049/last-observation-carried-forward-na-locf-on-panel-cross-section-time-series}
#'
#' This could probably be solved MUCH faster using Rcpp.
#'
#' @examples
#' na_locf(c(NA, NA))
#' na_locf(c(1, NA))
#' na_locf(c(1, NA, NA, NA))
#' na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4))
#' na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), recursive = FALSE)
#' \dontrun{
#'
#' # library(microbenchmark)
#' # library(zoo)
#'
#' # microbenchmark(
#' #  na_locf = na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4)),
#' #  na.locf = na.locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4))
#' #) # my implementation is 6 times faster :)
#'
#' #microbenchmark(
#' #  na_locf = na_locf(rep(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), 1000)),
#' #  na.locf = na.locf(rep(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), 1000))
#' # ) # my implementation is 3 times faster
#' 
#' }
#'
na_locf <- function(x, first_na_value = 0, recursive = TRUE, ...) {
  # where are the NA's:
  x_na <- is.na(x)

  # IF we have no NA's, return x:
  if (!any(x_na)) {
    return(x)
    # Else - fill one next observation:
  } else {
    # If the first observation is NA, fill it with "first_na_value"
    if (x_na[1]) x[1] <- first_na_value
    x_na[1] <- FALSE
    
    x_na_loc <- which(x_na)
    x[x_na_loc] <- x[x_na_loc - 1]

    if (recursive) {
      Recall(x)
    } else {
      return(x)
    }
  }
}




# str(result)
# class(result)
# result %>% as.dendrogram %>% plot


#' @export
as.hclust.pvclust <- function(x, ...) {
  # library(pvclust)
  x[["hclust"]]
}



#' @export
as.dendrogram.pvclust <- function(object, ...) {
  # library(pvclust)
  as.dendrogram(as.hclust(object))
}



# Imported from:
# pvclust:::hc2axes
# pvclust:::hc2axes(result[["hclust"]])


hc2axes <- function(x) {
  A <- x$merge
  n <- nrow(A) + 1
  x.axis <- c()
  y.axis <- x$height
  x.tmp <- rep(0, 2)
  zz <- match(1:length(x$order), x$order)
  for (i in 1:(n - 1)) {
    ai <- A[i, 1]
    if (ai < 0) {
      x.tmp[1] <- zz[-ai]
    } else {
      x.tmp[1] <- x.axis[ai]
    }
    ai <- A[i, 2]
    if (ai < 0) {
      x.tmp[2] <- zz[-ai]
    } else {
      x.tmp[2] <- x.axis[ai]
    }
    x.axis[i] <- mean(x.tmp)
  }
  return(data.frame(x.axis = x.axis, y.axis = y.axis, stringsAsFactors = TRUE))
}


# Imported from:
# pvclust:::hc2split
# pvclust:::hc2split(result[["hclust"]])

hc2split <- function(x) {
  A <- x$merge
  n <- nrow(A) + 1
  B <- list()
  for (i in 1:(n - 1)) {
    ai <- A[i, 1]
    if (ai < 0) {
      B[[i]] <- -ai
    } else {
      B[[i]] <- B[[ai]]
    }
    ai <- A[i, 2]
    if (ai < 0) {
      B[[i]] <- sort(c(B[[i]], -ai))
    } else {
      B[[i]] <- sort(c(B[[i]], B[[ai]]))
    }
  }
  CC <- matrix(rep(0, n * (n - 1)), nrow = (n - 1), ncol = n)
  for (i in 1:(n - 1)) {
    bi <- B[[i]]
    m <- length(bi)
    for (j in 1:m) CC[i, bi[j]] <- 1
  }
  split <- list(
    pattern = apply(CC, 1, paste, collapse = ""),
    member = B
  )
  return(split)
}













# Imported from:
# pvclust:::text.pvclust

#' @export
text.pvclust <- function(x, col = c(2, 3, 8), print.num = TRUE, float = 0.01,
                         cex = NULL, font = NULL, ...) {
  #    library(pvclust)
  axes <- hc2axes(x$hclust)
  usr <- par()$usr
  wid <- usr[4] - usr[3]
  au <- as.character(round(x$edges[, "au"] * 100))
  bp <- as.character(round(x$edges[, "bp"] * 100))
  rn <- as.character(row.names(x$edges))
  au[length(au)] <- "au"
  bp[length(bp)] <- "bp"
  rn[length(rn)] <- "edge #"
  a <- text(
    x = axes[, 1], y = axes[, 2] + float * wid, au,
    col = col[1], pos = 2, offset = 0.3, cex = cex, font = font
  )
  a <- text(
    x = axes[, 1], y = axes[, 2] + float * wid, bp,
    col = col[2], pos = 4, offset = 0.3, cex = cex, font = font
  )
  if (print.num) {
    a <- text(
      x = axes[, 1], y = axes[, 2], rn, col = col[3],
      pos = 1, offset = 0.3, cex = cex, font = font
    )
  }
}







#' @title The significant branches in a dendrogram, based on a pvclust object
#' @description Shows the significant branches in a dendrogram, based on a pvclust object
#' @export
#' @param dend a dendrogram object
#' @param pvclust_obj a pvclust object
#' @param signif_type a character scalar (either "bp" or "au"), indicating
#' which of the two should be used to update the dendrogram.
#' @param alpha a number between 0 to 1, default is .05. Indicates what is the
#' cutoff from which branches will be updated.
#' @param signif_value a 2d vector (deafult: c(5,1)),
#' with the first element tells us what the significant branches will get,
#' and the second element which value the non-significant branches will get.
#' @param show_type a character scalar (either "lwd" or "col"), indicating
#' which parameter of the branches should be updated based on significance.
#' @param ... not used
#' @return
#' A dendrogram with updated branches
#' @seealso \link{pvclust_show_signif}, \link{pvclust_show_signif_gradient}
#'
#' @examples
#' \dontrun{
#' library(pvclust)
#' data(lung) # 916 genes for 73 subjects
#' set.seed(13134)
#' result <- pvclust(lung[, 1:20], method.dist = "cor", method.hclust = "average", nboot = 100)
#'
#' dend <- as.dendrogram(result)
#' result %>%
#'   as.dendrogram() %>%
#'   hang.dendrogram() %>%
#'   plot(main = "Cluster dendrogram with AU/BP values (%)")
#' result %>% text()
#' result %>% pvrect(alpha = 0.95)
#'
#' dend %>%
#'   pvclust_show_signif(result) %>%
#'   plot()
#' dend %>%
#'   pvclust_show_signif(result, show_type = "lwd") %>%
#'   plot()
#' result %>% text()
#' result %>% pvrect(alpha = 0.95)
#'
#' dend %>%
#'   pvclust_show_signif_gradient(result) %>%
#'   plot()
#'
#' dend %>%
#'   pvclust_show_signif_gradient(result) %>%
#'   pvclust_show_signif(result) %>%
#'   plot(main = "Cluster dendrogram with AU/BP values (%)\n bp values are highlighted by signif")
#' result %>% text()
#' result %>% pvrect(alpha = 0.95)
#' }
pvclust_show_signif <- function(dend, pvclust_obj, signif_type = c("bp", "au"), alpha = .05, signif_value = c(5, 1), show_type = c("lwd", "col"), ...) {

  # these two are sorted from node number 1 to nnodes.
  #    result$edges$au
  #    result$edges$bp

  signif_type <- match.arg(signif_type)
  show_type <- match.arg(show_type)

  pvalue_per_node <- pvclust_obj$edges[[signif_type]]
  ord <- rank(get_branches_heights(dend, sort = FALSE))
  pvalue_per_node <- pvalue_per_node[ord]
  # Well, it is not exactly p-value. At elast, it is 1-Pv. And I am not yet sure of that...

  # plot, but ignore the leaves:
  #    nnodes(dend)-nleaves(dend) # number of branches nodes

  signif_TF <- pvalue_per_node > (1 - alpha)
  signif_TF[1] <- FALSE
  show_signif_TF <- ifelse(signif_TF, signif_value[1], signif_value[2])
  show_signif_TF_with_leaves <- rep(NA, nnodes(dend))
  ss_leaf <- which_leaf(dend)
  show_signif_TF_with_leaves[!ss_leaf] <- show_signif_TF
  show_signif_TF_with_leaves <- na_locf(show_signif_TF_with_leaves)

  assign_values_to_branches_edgePar(dend, show_signif_TF_with_leaves, show_type) # %>% plot
}





#' @title Significance gradient of branches in a dendrogram (via pvclust)
#' @description Shows the gradient of significance of branches in a dendrogram, based on a pvclust object
#' @export
#' @param dend a dendrogram object
#' @param pvclust_obj a pvclust object
#' @param signif_type a character scalar (either "bp" or "au"), indicating
#' which of the two should be used to update the dendrogram.
#' @param signif_col_fun a function to create colors for the significant
#' gradient. Default is: colorRampPalette(c("black", "darkred", "red"))
#' @param ... not used
#' @return
#' A dendrogram with updated branches
#' @seealso \link{pvclust_show_signif}, \link{pvclust_show_signif_gradient}
#'
#' @examples
#' \dontrun{
#' library(pvclust)
#' data(lung) # 916 genes for 73 subjects
#' set.seed(13134)
#' result <- pvclust(lung[, 1:20], method.dist = "cor", method.hclust = "average", nboot = 100)
#'
#' dend <- as.dendrogram(result)
#' result %>%
#'   as.dendrogram() %>%
#'   hang.dendrogram() %>%
#'   plot(main = "Cluster dendrogram with AU/BP values (%)")
#' result %>% text()
#' result %>% pvrect(alpha = 0.95)
#'
#' dend %>%
#'   pvclust_show_signif(result) %>%
#'   plot()
#' dend %>%
#'   pvclust_show_signif(result, show_type = "lwd") %>%
#'   plot()
#' result %>% text()
#' result %>% pvrect(alpha = 0.95)
#'
#' dend %>%
#'   pvclust_show_signif_gradient(result) %>%
#'   plot()
#'
#' dend %>%
#'   pvclust_show_signif_gradient(result) %>%
#'   pvclust_show_signif(result) %>%
#'   plot(main = "Cluster dendrogram with AU/BP values (%)\n bp values are highlighted by signif")
#' result %>% text()
#' result %>% pvrect(alpha = 0.95)
#' }
pvclust_show_signif_gradient <- function(dend, pvclust_obj, signif_type = c("bp", "au"), signif_col_fun = colorRampPalette(c("black", "darkred", "red")), ...) {

  # these two are sorted from node number 1 to nnodes.
  #    result$edges$au
  #    result$edges$bp

  signif_type <- match.arg(signif_type)

  pvalue_per_node <- pvclust_obj$edges[[signif_type]]
  ord <- rank(get_branches_heights(dend, sort = FALSE))
  pvalue_per_node <- pvalue_per_node[ord]
  # Well, it is not exactly p-value. At elast, it is 1-Pv. And I am not yet sure of that...

  # plot, but ignore the leaves:
  #    nnodes(dend)-nleaves(dend) # number of branches nodes



  signif_col <- signif_col_fun(100)
  #    plot(1:100, col = signif_col, pch = 19)
  #
  #
  pvalue_by_all_nodes <- rep(NA, nnodes(dend))
  ss_leaf <- which_leaf(dend)
  pvalue_by_all_nodes[!ss_leaf] <- pvalue_per_node
  pvalue_by_all_nodes <- na_locf(pvalue_by_all_nodes)
  #
  the_cols <- signif_col[round(pvalue_by_all_nodes * 100)]

  assign_values_to_branches_edgePar(dend, the_cols, "col") #  %>% plot
}











##################################
#### Required functions
##################################


# Based on:
# https://stat.ethz.ch/pipermail/r-help/2007-November/145106.html
# strheight for rotated text
strheight2 <- function(s, ...) {
  xusr <- par("usr")
  xh <- strwidth(s, cex = par("cex"), ...)
  yh <- strheight(s, cex = par("cex"), ...) * 5 / 3
  tmp <- xh
  xh <- yh / (xusr[4] - xusr[3]) * par("pin")[2]
  xh <- xh / par("pin")[1] * (xusr[2] - xusr[1])
  yh <- tmp / (xusr[2] - xusr[1]) * par("pin")[1]
  yh <- yh / par("pin")[2] * (xusr[4] - xusr[3])
  yh
}


# strwidth for rotated text
strwidth2 <- function(s, ...) {
  xusr <- par("usr")
  xh <- strwidth(s, cex = par("cex"), ...)
  yh <- strheight(s, cex = par("cex"), ...) * 5 / 3
  tmp <- xh
  xh <- yh / (xusr[4] - xusr[3]) * par("pin")[2]
  xh <- xh / par("pin")[1] * (xusr[2] - xusr[1])
  yh <- tmp / (xusr[2] - xusr[1]) * par("pin")[1]
  yh <- yh / par("pin")[2] * (xusr[4] - xusr[3])
  xh
}


# http://r-posts.com/adding-sinew-to-roxygen2-skeletons/
# sinew::makeOxygen(pvrect2)





#' @title Draw Rectangles Around a Dendrogram's Clusters with High/Low P-values
#' @export
#' @description
#' Draws rectangles around the branches of a dendrogram highlighting the corresponding clusters with low p-values.
#' This is based on \link[pvclust]{pvrect}, allowing to draw the rects till the bottom of the labels.
#' @param x object of class pvclust.
#' @param alpha threshold value for p-values., Default: 0.95
#' @param pv character string which specifies the p-value to be used. It should be either of "au" or "bp", corresponding to AU p-value or BP value, respectively. See plot.pvclust for details. , Default: 'au'
#' @param type one of "geq", "leq", "gt" or "lt". If "geq" is specified, clusters with p-value greater than or equals the threshold given by "alpha" are returned or displayed. Likewise "leq" stands for lower than or equals, "gt" for greater than and "lt" for lower than the threshold value. The default is "geq"., Default: 'geq'
#' @param max.only logical. If some of clusters with high/low p-values have inclusion relation, only the largest cluster is returned (or displayed) when max.only=TRUE., Default: TRUE
#' @param border numeric value which specifies the color of borders of rectangles., Default: 2
#' @param xpd A logical value (or NA.), passed to par. Default is TRUE, in order to allow the rect to be below the labels. If FALSE, all plotting is clipped to the plot region, if TRUE, all plotting is clipped to the figure region, and if NA, all plotting is clipped to the device region. See also clip., Default: TRUE
#' @param lower_rect a (scalar) value of how low should the lower part of the rect be. If missing, it will take the value of par("usr")[3L] (or par("usr")[2L], depending if horiz = TRUE or not), with also the width of the labels. (notice that we would like to keep xpd = TRUE if we want the rect to be after the labels!) You can use a value such as 0, to get the rect above the labels.
#' @param ... passed to \link{rect}
#' @return NULL
#' @seealso
#' \link[pvclust]{pvrect}, \link{pvclust_show_signif}
#' @examples
#' \dontrun{
#'
#'
#' library(dendextend)
#' library(pvclust)
#' data(lung) # 916 genes for 73 subjects
#' set.seed(13134)
#' result <- pvclust(lung[, 1:20], method.dist = "cor", method.hclust = "average", nboot = 10)
#'
#' par(mar = c(9, 2.5, 2, 0))
#' dend <- as.dendrogram(result)
#' dend %>%
#'   pvclust_show_signif(result, signif_value = c(3, .5)) %>%
#'   pvclust_show_signif(result, signif_value = c("black", "grey"), show_type = "col") %>%
#'   plot(main = "Cluster dendrogram with AU/BP values (%)")
#' pvrect2(result, alpha = 0.95)
#' # getting the rects to the tips / above the labels
#' pvrect2(result, lower_rect = .15, border = 4, alpha = 0.95, lty = 2)
#' # Original function
#' # pvrect(result, alpha=0.95)
#' text(result, alpha = 0.95)
#' }
#'
pvrect2 <- function(x, alpha = 0.95, pv = "au", type = "geq", max.only = TRUE,
                    border = 2,
                    xpd = TRUE, lower_rect,
                    ...) {
  dend <- as.dendrogram(x)

  len <- nrow(x$edges)
  member <- hc2split(x$hclust)$member
  order <- x$hclust$order
  usr <- par("usr")
  xwd <- usr[2] - usr[1]
  ywd <- usr[4] - usr[3]
  cin <- par()$cin
  ht <- c()
  j <- 1
  if (is.na(pm <- pmatch(type, c("geq", "leq", "gt", "lt")))) {
    stop("Invalid type argument: see help(pvrect)")
  }


  old_xpd <- par()["xpd"]
  par(xpd = xpd)


  for (i in (len - 1):1) {
    if (pm == 1) {
      wh <- (x$edges[i, pv] >= alpha)
    } else if (pm == 2) {
      wh <- (x$edges[i, pv] <= alpha)
    } else if (pm == 3) {
      wh <- (x$edges[i, pv] > alpha)
    } else if (pm == 4) {
      wh <- (x$edges[i, pv] > alpha)
    }
    if (wh) {
      mi <- member[[i]]
      ma <- match(mi, order)
      if (max.only == FALSE || (max.only && sum(match(ma,
        ht,
        nomatch = 0
      )) == 0)) {
        xl <- min(ma)
        xr <- max(ma)
        yt <- x$hclust$height[i]
        yb <- usr[3]
        mx <- xwd / length(member) / 3
        my <- ywd / 200

        # if(missing(lower_rect)) lower_rect <- par("usr")[3L] - strwidth("W")*(max(nchar(labels(dend))) + 1)
        if (missing(lower_rect)) lower_rect <- -max(strheight2(labels(dend)))

        dLeaf <- -0.75 * strheight("x")
        extra_space <- -strheight2("_")

        rect(xl - mx,
          # 0,
          lower_rect + dLeaf + extra_space,
          # yb + my + lower_rect,
          xr + mx, yt + my,
          border = border,
          shade = NULL, ...
        )
        j <- j + 1
      }
      ht <- c(ht, ma)
    }
  }

  par(xpd = old_xpd)

  invisible()
}














# !is.infinite(Inf)
#
#
#    show_signif_bp_with_leaves <- rep(NA, nnodes(dend))
#    ss_leaf <- which_leaf(dend)
#    show_signif_bp_with_leaves[!ss_leaf] <- show_signif_bp
#    show_signif_bp_with_leaves <- na_locf(show_signif_bp_with_leaves)
#
#    assign_values_to_branches_edgePar(dend, show_signif_bp_with_leaves, "lwd") %>% plot
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
#       assign_values_to_branches_edgePar(show_signif_bp_with_leaves, "lwd") %>%
#       plot
#    pvclust:::text.pvclust(result)
#    pvrect(result, alpha=0.95,pv = "bp")
#


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
#    show_signif_bp <- ifelse(signif_bp, 5, 1)
#    assign_values_to_branches_edgePar(dend, show_signif_bp, "lwd", skip_leaves = TRUE) %>% plot
#    pvclust:::text.pvclust(result)
#    pvrect(result, alpha=0.95,pv = "bp")
#
#
#    # Let's do it again, but include the leaves this time!
#
#    show_signif_bp_with_leaves <- rep(NA, nnodes(dend))
#    ss_leaf <- which_leaf(dend)
#    show_signif_bp_with_leaves[!ss_leaf] <- show_signif_bp
#    show_signif_bp_with_leaves <- na_locf(show_signif_bp_with_leaves)
#
#    assign_values_to_branches_edgePar(dend, show_signif_bp_with_leaves, "lwd") %>% plot
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
#       assign_values_to_branches_edgePar(show_signif_bp_with_leaves, "lwd") %>%
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
#    # https://cran.r-project.org/package=pvclust
#
# }
#



#' @title Get Pvclust Edges Information
#' @export
#' @description
#' Get pvclust edges information such as au and bp and return dataframe with proper sample labels.
#' This function is useful when there are a lot of samples involved.
#'
#' @param pvclust_obj pvclust object
#' @return data.frame with leaves on column 1 and 2, followed by the rest of the information from edge
#' @references hclust object descriptions \url{https://stat.ethz.ch/R-manual/R-patched/library/stats/html/hclust.html}
#'
#' @examples
#' \dontrun{
#'
#' library(pvclust)
#' data(lung) # 916 genes for 73 subjects
#' set.seed(13134)
#' result <- pvclust(lung[, 1:20], method.dist = "cor", method.hclust = "average", nboot = 100)
#' pvclust_edges(result)
#' }
pvclust_edges <- function(pvclust_obj) {
  hclust_merge <- pvclust_obj$hclust$merge
  hclust_merge[hclust_merge < 0] <- pvclust_obj$hclust$labels[abs(hclust_merge[hclust_merge < 0])] # get sample name
  hclust_merge <- cbind(hclust_merge, pvclust_obj$edges) # combine with edge table
  colnames(hclust_merge)[1:2] <- c("branch_L", "branch_R")
  return(hclust_merge)
}
