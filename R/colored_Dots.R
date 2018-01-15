# Same as colored_bars.R, except with circle representation for each point 

zero_range <- function (x, tol = .Machine$double.eps * 100) 
{
  if (length(x) == 1) 
    return(TRUE)
  if (length(x) != 2) 
    stop("x must be length 1 or 2")
  if (any(is.na(x))) 
    return(NA)
  if (x[1] == x[2]) 
    return(TRUE)
  if (all(is.infinite(x))) 
    return(FALSE)
  m <- min(abs(x))
  if (m == 0) 
    return(FALSE)
  abs((x[1] - x[2])/m) < tol
}

rescale <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE)) 
{
  if (zero_range(from) || zero_range(to)) 
    return(rep(mean(to), length(x)))
  (x - from[1])/diff(from) * diff(to) + to[1]
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
  yh <- strheight(cha, cex = par("cex")) * 5/3 
  tmp <- xh 
  xh <- yh/(xusr[4]-xusr[3])*par("pin")[2] 
  xh <- xh/ par("pin")[1] * (xusr[2]-xusr[1]) 
  yh <- tmp/(xusr[2]-xusr[1])* par("pin")[1] 
  yh <- yh/ par("pin")[2] * (xusr[4]-xusr[3]) 
  c(xh = xh, yh = yh)
}
V_rotated_str_dim <- Vectorize(rotated_str_dim)
max_labels_height <- function(s) {
  # s <- paste0(dend_labels, " ")
  a <- V_rotated_str_dim(s)
  max(a["yh",]) 
}




colored_dots<-function (colors, dend, rowLabels = NULL, cex.rowLabels = 0.9, 
                        add = TRUE, y_scale, y_shift, text_shift = 1, sort_by_labels_order = TRUE, 
                        horiz = FALSE, ...) 
{
  n_colors <- if (is.null(dim(colors))) 
    length(colors)
  else nrow(colors)
  n_groups <- if (is.null(dim(colors))) 
    1
  else ncol(colors)
  if (!missing(dend)) {
    if (is.hclust(dend)) 
      dend <- as.dendrogram(dend)
    if (!is.dendrogram(dend)) 
      stop("'dend' should be a dendrogram.")
    dend_labels <- labels(dend)
    dend_order <- order.dendrogram(dend)
  }
  else {
    dend_labels <- rep("W", n_colors)
    dend_order <- seq_len(n_colors)
  }
  if (!sort_by_labels_order) 
    dend_order <- seq_len(n_colors)
  if (!horiz) {
    if (missing(y_shift)) 
      y_shift <- -max_labels_height(dend_labels) + par("usr")[3L] - 
        strheight("X")
    if (missing(y_scale)) 
      y_scale <- strheight("X") * n_groups
  }
  else {
    if (missing(y_shift)) 
      y_shift <- -(min(strwidth(dend_labels)) + par("usr")[2L] + 
                     strwidth("X"))
    if (missing(y_scale)) 
      y_scale <- strwidth("X") * n_groups
  }
  y_shift <- y_shift - y_scale
  colors <- as.matrix(colors)
  dimC <- dim(colors)
  if (is.null(rowLabels) & (length(dimnames(colors)[[2]]) == 
                            dimC[2])) 
    rowLabels = names(as.data.frame(colors))
  op <- options()
  pr <- par(no.readonly = TRUE)
  options(stringsAsFactors = FALSE)
  par(xpd = TRUE)
  if (length(dend_order) != dimC[1]) 
    stop("ERROR: length of colors vector not compatible with number of objects in the hierarchical tree.")
  C <- colors[dend_order, ]
  C <- as.matrix(C)
  step <- 1/(n_colors - 1)
  ystep <- 1/n_groups
  if (!add) {
    barplot(height = 1, col = "white", border = FALSE, space = 0, 
            axes = FALSE, ...)
  }
  charWidth <- strwidth("W")/2
  charHeight <- strheight("W")/2
  for (j in 1:n_groups) {
    ind <- (1:n_colors)
    xl <- (ind - 1.5) * step
    xr <- (ind - 0.5) * step
    yb <- rep(ystep * (j - 1), n_colors)
    yt <- rep(ystep * j, n_colors)
    if (add) {
      xl <- rescale(xl, to = c(1 - 0.5, n_colors - 0.5))
      xr <- rescale(xl, to = c(1 + 0.5, n_colors + 0.5))
      yb <- yb * y_scale + y_shift
      yt <- yt * y_scale + y_shift
    }
    if (horiz) {
      # Instead of rectangles, put in points at 1/2 x and y coordinates 
      #rect(-yb, xl, -yt, xr, col = as.character(C[, j]), 
      #     border = as.character(C[, j]))
      
      points((-yb+(-yt))/2,(xl+xr)/2,pch=19,col = as.character(C[, j]))
      
      
      par(srt = 90)
      if (is.null(rowLabels)) {
        s <- as.character(j)
        text(s, pos = 1, offset = 0.5, y = charHeight * 
               text_shift - rotated_str_dim(s)[2]/2, x = -(ystep * 
                                                             (j) * y_scale + y_shift), cex = cex.rowLabels)
      }
      else {
        s <- rowLabels[j]
        text(s, pos = 1, offset = 0.5, y = charHeight * 
               text_shift - rotated_str_dim(s)[2]/2, x = -(ystep * 
                                                             (j) * y_scale + y_shift), cex = cex.rowLabels)
      }
    }
    else {
      # Instead of rectangles, put in points at 1/2 x and y coordinates
      #rect(xl, yb, xr, yt, col = as.character(C[, j]), 
      #    border = as.character(C[, j]))
      points((xl1+xr)/2,(yb+yt)/2,pch=19,col = as.character(C[, j]))
      
      
      
      if (is.null(rowLabels)) {
        text(as.character(j), pos = 2, x = charWidth * 
               text_shift, y = ystep * (j - 0.5) * y_scale + 
               y_shift, cex = cex.rowLabels)
      }
      else {
        text(rowLabels[j], pos = 2, x = charWidth * text_shift, 
             y = ystep * (j - 0.5) * y_scale + y_shift, 
             cex = cex.rowLabels)
      }
    }
  }
  for (j in 0:n_groups) {
    the_x <- rescale(c(0, 1), to = c(1 - 0.5, n_colors + 
                                       0.5))
    if (horiz) {
      lines(y = the_x, x = -(c(ystep * j, ystep * j) * 
                               y_scale + y_shift))
    }
    else {
      lines(x = the_x, y = c(ystep * j, ystep * j) * y_scale + 
              y_shift)
    }
  }
  options(op)
  par(pr)
  return(invisible(C))
}
