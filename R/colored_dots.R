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




#' @title Add colored dots beside a dendrogram
#' @export
#' @description
#' Add colored dots next to a dendrogram, usually
#' corresponding to either clusters or some outside
#' categorization.
#' @param colors Coloring of the dots beside the dendrogram. Either a vector (one color per object)
#' or a matrix (can also be an array or a data frame)
#' with each column giving one group with color per object.
#' Each column will be plotted as a colored point (when horiz = FALSE)
#' under the dendrogram.
#' As long as the sort_by_labels_order paramter is TRUE (default), the colors vector/matrix should
#' be provided in the order of the original data order (and it will be re-ordered automatically to
#' the order of the dendrogram)
#' @param dend a dendrogram object. If missing, the colors are plotted without and re-ordering
#' (this assumes that the colors are already ordered based on the dend's labels)
#' This is also important in order to get the correct height/location of the colored dots
#' (i.e.: adjusting the y_scale and y_shift)
#' @param rowLabels Labels for the colorings given in \code{colors}. The labels will be printed to the
#' left of the color rows in the plot. If the argument is given, it must be a vector of length
#' equal to the number of columns in \code{colors}. If not given, \code{names(colors)}
#' will be used if available. If not, sequential numbers
#' starting from 1 will be used.
#' @param cex.rowLabels Font size scale factor for the row labels. See \code{\link[graphics]{par}}.
#' @param add logical(TRUE), should the colored dots be added to an existing
#' dendrogram plot?
#' @param y_scale how much should the dots be stretched on the y axis?
#' If no dend is supplied - the default will be 1
#' @param y_shift where should the dots be plotted underneath the x axis?
#' By default it will try to locate the dots underneath the labels (it may miss,
#' in which case you would need to enter a number manually)
#' If no dend is supplied - the default will be 0
#' @param text_shift a dendrogram object
#' @param sort_by_labels_order logical(TRUE) - if TRUE (default), then the order of the
#' colored dots will be sorted based on the order needed to change the original
#' order of the observations to the current order of the labels in the dendrogram.
#' If FALSE the colored dots are plotted as-is, based on the order
#' of the colors vector.
#' @param horiz logical (FALSE by default). Set to TRUE when using plot(dend, horiz = TRUE)
#' @param ... ignored at this point.
#' @author Steve Horvath \email{SHorvath@@mednet.ucla.edu},
#' Tal Galili \email{Tal.Galili@@gmail.com},
#' Peter Langfelder \email{Peter.Langfelder@@gmail.com},
#' Chase Clark \email{chasec288@@gmail.com}
#' @details
#' The reason you might choose colored_dots over colored_bars is when you have
#' a lot of group types and/or a really large dendrogram.
#' Hint: Make a group for each categorical factor and color it one color when true,
#' and assign a fully transparent color when false.
#'
#' You will often need to adjust the y_scale, y_shift and the text_shift
#' parameters, in order to get the dots in the location you would want.
#'
#' (This can probably be done automatically, but will require more work.
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
#' add the colored dots on top of an existing plot (and not only as a seperate plot).
#'
#' See: \url{https://cran.r-project.org/package=moduleColor}
#' For more details.
#'
#' @seealso
#' \link{branches_attr_by_clusters},
#' \link[WGCNA]{plotDendroAndColors}
#' @examples
#'
#' rows_picking <- c(1:5, 25:30)
#' dend <- (iris[rows_picking, -5] * 10) %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' odd_numbers <- rows_picking %% 2
#' cols <- c("red", "white")[odd_numbers + 1]
#' plot(dend)
#' colored_dots(cols, dend)
#' # Example of adjusting postion of dots
#' plot(dend)
#' colored_dots(cols, dend,
#'   y_shift = -1,
#'   rowLabels = "Odd\n numbers"
#' )
#'
#'
#'
#' rows_picking <- c(1:5, 25:30)
#' dend <- (iris[rows_picking, -5] * 10) %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' odd_numbers <- rows_picking %% 2
#' # For leaves that shouldn't have dots, make them the same color as the background,
#' # or set the alpha value to fully transparant
#' cols <- c("black", "white")[odd_numbers + 1]
#' # scale is off
#' plot(dend)
#' colored_dots(cols, dend)
#' # move and scale a bit
#' plot(dend)
#' colored_dots(cols, dend,
#'   y_shift = -1,
#'   rowLabels = "Odd\n numbers"
#' )
#' # Now let's cut the tree and add that info to the plot:
#' k2 <- cutree(dend, k = 2)
#' cols2 <- c("#1b9e77", "#d95f02")[k2]
#'
#' par(mar = c(5, 6, 1, 1))
#' plot(dend)
#' colored_dots(cbind(cols2, cols), dend,
#'   rowLabels = c("2 clusters", "Even numbers")
#' )
#'
#' # The same, but with an horizontal plot!
#' par(mar = c(6, 2, 2, 4))
#' plot(dend, horiz = TRUE)
#' colored_dots(cbind(cols2, cols), dend,
#'   rowLabels = c("2 clusters", "Even numbers"),
#'   horiz = TRUE
#' )
#'
#' # ==============================
#' # ==============================
#'
#' ## mtcars example
#'
#' # Create the dend:
#' dend <- as.dendrogram(hclust(dist(mtcars)))
#'
#' # Get all company names
#' comp_names <- unlist(lapply(rownames(mtcars), function(x) strsplit(x, " ")[[1]][[1]]))
#' # Get the top three occurring companies
#' top_three <- sort(table(comp_names), decreasing = TRUE)[1:3]
#' # Match the top three companies to where they are found in the dendrogram labels
#' top_three <- sapply(names(top_three), function(x) grepl(x, labels(dend)))
#' top_three <- as.data.frame(top_three)
#' # "top_three" is now a data frame of the top three companies as columns.
#' # Each column represents a vector (rows) which is the length of labels(dend).
#' # The vector has values TRUE and FALSE, for whether the company name matched
#' # labels(dend)[i]
#'
#' # Colorblind friendly vector of HEX colors
#' colorblind_friendly <- c("#1b9e77", "#d95f02", "#7570b3")
#'
#' # If we run the for-loop on "top_three" we will turn the vectors into a character-type too early,
#' # so make a copy to "colored_dataframe" which we will work on
#' colored_dataframe <- top_three
#'
#' for (i in 1:3) {
#'   # This replaces TRUE values with a color from our vector of colors
#'   colored_dataframe[top_three[, i], i] <- colorblind_friendly[[i]]
#'   # This replaces FALSE values with black HEX, but fully transparent (invisible on plot)
#'   colored_dataframe[!top_three[, i], i] <- "#00000000"
#' }
#'
#' # Color branches and labels by "cutting" the dendrogram at an arbitrary height
#' dend <- color_branches(dend, h = 170)
#' dend <- color_labels(dend, h = 170)
#'
#' ### plots
#' par(mar = c(12, 4, 1, 1))
#' plot(dend)
#' colored_dots(colored_dataframe, dend,
#'   rowLabels = colnames(colored_dataframe), horiz = FALSE, sort_by_labels_order = FALSE
#' )
#' # Show a dotted line where tree was "cut"
#' abline(h = 170, lty = 3)
#'
#' # horiz version:
#' par(mar = c(4, 1, 1, 12))
#' plot(dend, horiz = TRUE)
#' colored_dots(colored_dataframe, dend,
#'   rowLabels = colnames(colored_dataframe), horiz = TRUE, sort_by_labels_order = FALSE
#' )
#' # Show a dotted line where the tree was "cut"
#' abline(v = 170, lty = 3)
colored_dots <- function(colors, dend, rowLabels = NULL, cex.rowLabels = 0.9,
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



  # moving the y location and scale of the dots
  # this allows us to have it underneath the dend
  # in a way that would look nice.
  #    if(missing(y_scale)) y_scale <- 1

  # this makes sure that the original shift we had is always compared to the top of the dots (instead of the bottom)
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
      # Instead of rectangles, put in points at 1/2 x and y coordinates
      # rect(-yb, xl, -yt, xr, col = as.character(C[, j]),
      #     border = as.character(C[, j]))

      points((-yb + (-yt)) / 2, (xl + xr) / 2, pch = 19, col = as.character(C[, j]))


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
      # Instead of rectangles, put in points at 1/2 x and y coordinates
      # rect(xl, yb, xr, yt, col = as.character(C[, j]),
      #    border = as.character(C[, j]))
      points((xl + xr) / 2, (yb + yt) / 2, pch = 19, col = as.character(C[, j]))


      if (is.null(rowLabels)) {
        text(as.character(j), pos = 2, x = charWidth * text_shift, y = ystep * (j - 0.5) * y_scale + y_shift, cex = cex.rowLabels)
        #          text(as.character(j), pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      } else {
        text(rowLabels[j], pos = 2, x = charWidth * text_shift, y = ystep * (j - 0.5) * y_scale + y_shift, cex = cex.rowLabels)
        #          text(rowLabels[j], pos=2, x= -charWidth -0.5*step, y=ystep*(j-0.5), cex=cex.rowLabels, xpd = TRUE);
      }
    }
  }
  # we start j from 0 so that it will add a line below the dots (and not just above them)


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
