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

zero_range <- function(x, tol = .Machine$double.eps * 100) {
  if (length(x) == 1) {
    return(TRUE)
  }
  if (length(x) != 2) {
    stop("x must be length 1 or 2")
  }
  if (any(is.na(x))) {
    return(NA)
  }
  if (x[1] == x[2]) {
    return(TRUE)
  }
  if (all(is.infinite(x))) {
    return(FALSE)
  }
  m <- min(abs(x))
  if (m == 0) {
    return(FALSE)
  }
  abs((x[1] - x[2]) / m) < tol
}

rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  if (zero_range(from) || zero_range(to)) {
    return(rep(mean(to), length(x)))
  }
  (x - from[1]) / diff(from) * diff(to) + to[1]
}






#    if(T) 2 else 1
#    if(F) 2 else 1



# This function is require in order to know the height of the rotated labels in the dendrogram!
# Thanks to Prof. Brian Ripley
# # http://r.789695.n4.nabble.com/strwidth-and-strheight-for-rotated-text-td839105.html
rotated_str_dim <- function(s) {
  cha <- s
  xusr <- par("usr")
  xh <- strwidth(cha, cex = par("cex"))
  yh <- strheight(cha, cex = par("cex")) * 5 / 3
  tmp <- xh
  xh <- yh / (xusr[4] - xusr[3]) * par("pin")[2]
  xh <- xh / par("pin")[1] * (xusr[2] - xusr[1])
  yh <- tmp / (xusr[2] - xusr[1]) * par("pin")[1]
  yh <- yh / par("pin")[2] * (xusr[4] - xusr[3])
  c(xh = xh, yh = yh)
}
V_rotated_str_dim <- Vectorize(rotated_str_dim)
max_labels_height <- function(s) {
  # s <- paste0(dend_labels, " ")
  a <- V_rotated_str_dim(s)
  max(a["yh", ])
}



#' @title Add colored bars to a dendrogram
#' @export
#' @description
#' Add colored bars to a dendrogram, usually
#' corresponding to either clusters or some outside
#' categorization.
#' @param colors Coloring of objects on the dendrogram. Either a vector (one color per object)
#' or a matrix (can also be an array or a data frame)
#' with each column giving one group with color per object.
#' Each column will be plotted as a horizontal row of colors (when horiz = FALSE)
#' under the dendrogram.
#' As long as the sort_by_labels_order paramter is TRUE (default), the colors vector/matrix should
#' be provided in the order of the original data order (and it will be re-ordered automaticall to
#' the order of the dendrogram)
#' @param dend a dendrogram object. If missing, the colors are plotted without and re-ordering
#' (this assumes that the colors are already ordered based on the dend's labels)
#' This is also important in order to get the correct height/location of the colored bars
#' (i.e.: adjusting the y_scale and y_shift)
#' @param rowLabels Labels for the colorings given in \code{colors}. The labels will be printed to the
#' left of the color rows in the plot. If the argument is given, it must be a vector of length
#' equal to the number of columns in \code{colors}. If not given, \code{names(colors)}
#' will be used if available. If not, sequential numbers
#' starting from 1 will be used.
#' @param cex.rowLabels Font size scale factor for the row labels. See \code{\link[graphics]{par}}.
#' @param add logical(TRUE), should the colored bars be added to an existing
#' dendrogram plot?
#' @param y_scale how much should the bars be stretched on the y axis?
#' If no dend is supplied - the default will be 1
#' @param y_shift where should the bars be plotted underneath the x axis?
#' By default it will try to locate the bars underneath the labels (it may miss,
#' in which case you would need to enter a number manually)
#' If no dend is supplied - the default will be 0
#' @param text_shift a dendrogram object
#' @param sort_by_labels_order logical(TRUE) - if TRUE (default), then the order of the
#' colored bars will be sorted based on the order needed to change the original
#' order of the observations to the current order of the labels in the dendrogram.
#' If FALSE the colored bars are plotted as-is, based on the order
#' of the colors vector.
#' @param horiz logical (FALSE by default). Set to TRUE when using plot(dend, horiz = TRUE)
#' @param ... ignored at this point.
#' @author Steve Horvath \email{SHorvath@@mednet.ucla.edu},
#' Peter Langfelder \email{Peter.Langfelder@@gmail.com},
#' Tal Galili \email{Tal.Galili@@gmail.com}
#' @details
#' You will often needs to adjust the y_scale, y_shift and the text_shift
#' parameters, in order to get the bars in the location you would want.
#'
#' (this can probably be done automatically, but will require more work.
#' since it has to do with the current mar settings,
#' the number of groups, and each computer's specific graphic device.
#' patches for smarter defaults will be appreciated)
#' @return
#' An invisible vector/matrix with the ordered colors.
#'
#' @source
#' This function is based on the \link[moduleColor]{plotHclustColors} from the
#' {moduleColor} R package. It was modified so that it would
#' work with dendrograms (and not just hclust objects), as well allow to
#' add the colored bars on top of an existing plot (and not only as a seperate plot).
#'
#' See: \url{https://cran.r-project.org/package=moduleColor}
#' For more details.
#'
#' @seealso
#' \link{branches_attr_by_clusters},
#' \link[WGCNA]{plotDendroAndColors}
#' @examples
#'
#'
#' rows_picking <- c(1:5, 25:30)
#' dend <- (iris[rows_picking, -5] * 10) %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' odd_numbers <- rows_picking %% 2
#' cols <- c("gold", "grey")[odd_numbers + 1]
#' # scale is off
#' plot(dend)
#' colored_bars(cols, dend)
#' # move and scale a bit
#' plot(dend)
#' colored_bars(cols, dend,
#'   y_shift = -1,
#'   rowLabels = "Odd\n numbers"
#' )
#' # Now let's cut the tree and add that info to the plot:
#' k2 <- cutree(dend, k = 2)
#' cols2 <- c("#0082CE", "#CC476B")[k2]
#' plot(dend)
#' colored_bars(cbind(cols2, cols), dend,
#'   rowLabels = c("2 clusters", "Odd numbers")
#' )
#'
#' # The same, but with an horizontal plot!
#' par(mar = c(6, 2, 2, 4))
#' plot(dend, horiz = TRUE)
#' colored_bars(cbind(cols2, cols), dend,
#'   rowLabels = c("2 clusters", "Odd numbers"),
#'   horiz = TRUE
#' )
#'
#'
#'
#' # let's add clusters color
#' # notice how we need to play with the colors a bit
#' # this is because color_branches places colors from
#' # left to right. Which means we need to give colored_bars
#' # the colors of the items so that ofter sorting they would be
#' # from left to right. Here is how it can be done:
#' the_k <- 3
#' library(colorspace)
#' cols3 <- rainbow_hcl(the_k, c = 90, l = 50)
#' dend %>%
#'   set("branches_k_color", k = the_k, with = cols3) %>%
#'   plot()
#'
#' kx <- cutree(dend, k = the_k)
#' ord <- order.dendrogram(dend)
#' kx <- sort_levels_values(kx[ord])
#' kx <- kx[match(seq_along(ord), ord)]
#'
#' par(mar = c(5, 5, 2, 2))
#' plot(dend)
#' colored_bars(cbind(cols3[kx], cols2, cols), dend,
#'   rowLabels = c("3 clusters", "2 clusters", "Odd numbers")
#' )
#'
#'
#'
#' ## mtcars example
#'
#' # Create the dend:
#' dend <- as.dendrogram(hclust(dist(mtcars)))
#'
#' # Create a vector giving a color for each car to which company it belongs to
#' car_type <- rep("Other", length(rownames(mtcars)))
#' is_x <- grepl("Merc", rownames(mtcars))
#' car_type[is_x] <- "Mercedes"
#' is_x <- grepl("Mazda", rownames(mtcars))
#' car_type[is_x] <- "Mazda"
#' is_x <- grepl("Toyota", rownames(mtcars))
#' car_type[is_x] <- "Toyota"
#' car_type <- factor(car_type)
#' n_car_types <- length(unique(car_type))
#' col_car_type <- colorspace::rainbow_hcl(n_car_types, c = 70, l = 50)[car_type]
#'
#' # extra: showing the various clusters cuts
#' k234 <- cutree(dend, k = 2:4)
#'
#' # color labels by car company:
#' labels_colors(dend) <- col_car_type[order.dendrogram(dend)]
#' # color branches based on cutting the tree into 4 clusters:
#' dend <- color_branches(dend, k = 4)
#'
#' ### plots
#' par(mar = c(12, 4, 1, 1))
#' plot(dend)
#' colored_bars(cbind(k234[, 3:1], col_car_type), dend,
#'   rowLabels = c(paste0("k = ", 4:2), "Car Type")
#' )
#'
#' # horiz version:
#' par(mar = c(4, 1, 1, 12))
#' plot(dend, horiz = TRUE)
#' colored_bars(cbind(k234[, 3:1], col_car_type), dend,
#'   rowLabels = c(paste0("k = ", 4:2), "Car Type"), horiz = TRUE
#' )
colored_bars <- function(colors, dend, rowLabels = NULL, cex.rowLabels = 0.9,
                         add = TRUE,
                         y_scale, y_shift,
                         text_shift = 1,
                         sort_by_labels_order = TRUE,
                         horiz = FALSE,
                         # below_labels = TRUE,
                         ...) {
  # should either be a vector or a matrix (it could also work with data.frames)
  # if(!(is.vector(colors) | is.matrix(colors))) stop("colors must be either a vector or a matrix")


  # number of color boxes per row (i.e.: number of dend leaves)
  n_colors <- if (is.null(dim(colors))) length(colors) else nrow(colors)
  # number of rows (groups) of colors
  n_groups <- if (is.null(dim(colors))) 1 else ncol(colors)



  if (!missing(dend)) {
    # make sure we are working with a dend:
    if (is.hclust(dend)) dend <- as.dendrogram(dend)
    if (!is.dendrogram(dend)) stop("'dend' should be a dendrogram.")
    # get labels' order:
    # dend_size <- nleaves(dend)
    dend_labels <- labels(dend)
    dend_order <- order.dendrogram(dend)
  } else { # if we ARE missing dend
    dend_labels <- rep("W", n_colors) # assume all labels are W
    dend_order <- seq_len(n_colors)
  }

  if (!sort_by_labels_order) dend_order <- seq_len(n_colors)

  #    # Get y_shift to be underneath the labels
  #    if(!horiz) {
  #       if(missing(y_shift)) y_shift <- -max(strwidth(dend_labels))+par()$usr[3L]-2*strheight("x") # a bit of a hack, oh well...
  #       if(missing(y_scale)) y_scale <- median(strheight(dend_labels)) * nrow_colors * .8
  #    } else {
  #       if(missing(y_shift)) y_shift <- max(abs(strwidth(dend_labels)))+par()$usr[3L]-3*strwidth("x") # a bit of a hack, oh well...
  #       if(missing(y_scale)) y_scale <- median(abs(strwidth(dend_labels))) * nrow_colors * .8
  #    }


  # Get y_shift to be underneath the labels
  if (!horiz) {
    # We shift y with the max height of the rotated labels + plot region + one row of values (the height of X)
    if (missing(y_shift)) y_shift <- -max_labels_height(dend_labels) + par("usr")[3L] - strheight("X")
    if (missing(y_scale)) y_scale <- strheight("X") * n_groups
  } else {
    if (missing(y_shift)) y_shift <- -(min(strwidth(dend_labels)) + par("usr")[2L] + strwidth("X"))
    if (missing(y_scale)) y_scale <- strwidth("X") * n_groups
  }



  # moving the y location and scale of the bars
  # this allows us to have it underneath the dend
  # in a way that would look nice.
  #    if(missing(y_scale)) y_scale <- 1

  # this makes sure that the original shift we had is always compared to the top of the bars (instead of the bottom)
  # the if-else lowers the y further, to make sure it is below the labels.
  y_shift <- y_shift - y_scale
  #    - if(below_labels) max(strheight(labels(dend))) else 0

  colors <- as.matrix(colors)
  dimC <- dim(colors)

  if (is.null(rowLabels) & (length(dimnames(colors)[[2]]) == dimC[2])) rowLabels <- names(as.data.frame(colors))

  op <- options()
  pr <- par(no.readonly = TRUE)

  options(stringsAsFactors = FALSE)
  # par(mar = c(0, 4.1, 0, 2.1))
  par(xpd = TRUE)

  if (length(dend_order) != dimC[1]) {
    stop("ERROR: length of colors vector not compatible with number of objects in the hierarchical tree.")
  }
  C <- colors[dend_order, ] # new colors vector
  C <- as.matrix(C)
  step <- 1 / (n_colors - 1)
  ystep <- 1 / n_groups

  if (!add) {
    barplot(height = 1, col = "white", border = FALSE, space = 0, axes = FALSE, ...)
  }


  charWidth <- strwidth("W") / 2
  charHeight <- strheight("W") / 2
  for (j in 1:n_groups)
  {
    ind <- (1:n_colors)
    xl <- (ind - 1.5) * step # locations of the x left
    xr <- (ind - 0.5) * step
    yb <- rep(ystep * (j - 1), n_colors)
    yt <- rep(ystep * j, n_colors)

    if (add) {
      xl <- rescale(xl, to = c(1 - .5, n_colors - .5))
      xr <- rescale(xl, to = c(1 + .5, n_colors + .5))
      yb <- yb * y_scale + y_shift
      yt <- yt * y_scale + y_shift
    }

    if (horiz) {
      rect(-yb, xl, -yt, xr, col = as.character(C[, j]), border = as.character(C[, j]))

      # plot the rowLabels text:
      par(srt = 90)

      if (is.null(rowLabels)) {
        s <- as.character(j)
        text(s, pos = 1, offset = 0.5, y = charHeight * text_shift - rotated_str_dim(s)[2] / 2, x = -(ystep * (j) * y_scale + y_shift), cex = cex.rowLabels)
        #          text(as.character(j), pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      } else {
        s <- rowLabels[j]
        text(s, pos = 1, offset = 0.5, y = charHeight * text_shift - rotated_str_dim(s)[2] / 2, x = -(ystep * (j) * y_scale + y_shift), cex = cex.rowLabels)
        #          text(rowLabels[j], pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      }
    } else { # default
      rect(xl, yb, xr, yt, col = as.character(C[, j]), border = as.character(C[, j]))

      if (is.null(rowLabels)) {
        text(as.character(j), pos = 2, x = charWidth * text_shift, y = ystep * (j - 0.5) * y_scale + y_shift, cex = cex.rowLabels)
        #          text(as.character(j), pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      } else {
        text(rowLabels[j], pos = 2, x = charWidth * text_shift, y = ystep * (j - 0.5) * y_scale + y_shift, cex = cex.rowLabels)
        #          text(rowLabels[j], pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      }
    }
  }
  # we start j from 0 so that it will add a line below the bars (and not just above them)


  # Adds lines to seperate the clusters
  for (j in 0:n_groups) {
    the_x <- rescale(c(0, 1), to = c(1 - .5, n_colors + .5))
    if (horiz) {
      lines(y = the_x, x = -(c(ystep * j, ystep * j) * y_scale + y_shift))
    } else {
      lines(x = the_x, y = c(ystep * j, ystep * j) * y_scale + y_shift)
    }
  }



  options(op) # reset (all) initial options
  par(pr) # suppressWarnings(par(pr))

  return(invisible(C))
}






#
# if(FALSE) {
#
#
#
#    colored_bars(dend, the_cols[cutree(dend, k = the_k)], y_shift = -2)
#    plot(1:5, pch = 19, cex = 2, col = the_cols)
#
#    cutree(dend, k = the_k)[order.dendrogram(dend)]
#
#    kx <- cutree(dend, k = the_k)
#    ord <- order.dendrogram(dend)
#    kx  <- sort_levels_values(kx[ord])
#    kx  <- kx[match(seq_along(ord), ord)]
#
#    library(colorspace)
#    dend %>% set("branches_k_color", k = the_k, with = the_cols) %>% plot
#    colored_bars(dend, cols3[kx], y_shift = -2)
#
#
#
#
#    # of course the pattern we see is just fake.
#
#    k4 <- cutree(dend, k = 4)
#    plot(dend)
#    colored_bars(dend, k4)
#    library(colorspace)
#    dend %>% set("branches_k_color", k = 4) %>% plot
#    colored_bars(dend, rainbow_hcl(4)[c(4,2,3,1)][k4], y_shift = -2)
#
#    the_k <- 5
#    the_cols <- rainbow_hcl(the_k, c=90, l=50)
#    dend %>% set("branches_k_color", k = the_k, with = the_cols) %>% plot
#    colored_bars(dend, the_cols[cutree(dend, k = the_k)], y_shift = -2)
#    plot(1:5, pch = 19, cex = 2, col = the_cols)
#
#    cutree(dend, k = the_k)[order.dendrogram(dend)]
#
#    kx <- cutree(dend, k = the_k)
#    ord <- order.dendrogram(dend)
#    kx  <- sort_levels_values(kx[ord])
#    kx  <- kx[match(seq_along(ord), ord)]
#
#    library(colorspace)
#    dend %>% set("branches_k_color", k = the_k, with = the_cols) %>% plot
#    colored_bars(dend, the_cols[kx], y_shift = -2)
#
#    the_cols <- apply(cutree(dend, k = 2:5), 2, function(x) heat_hcl(5)[x])
#
#    plot(dend, las = 1)
#    colored_bars(dend, the_cols)
#    plot(dend, las = 1)
#    colored_bars(dend, the_cols,y_scale = 3, y_shift = -2)
#
# }
