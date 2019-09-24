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


# This file includes hidden functions from "stats" that have been imported into this package
# in case their usage would be changed in future versions of R.

# Unexported objects imported by ':::' calls:
# 'stats:::.memberDend' 'stats:::.midDend'
# 'stats:::midcache.dendrogram' 'stats:::plotNode'
# 'stats:::plotNodeLimit'








# stats:::.memberDend
stats_.memberDend <- function(x) {
  r <- attr(x, "x.member")
  if (is.null(r)) {
    r <- attr(x, "members")
    if (is.null(r)) {
      r <- 1L
    }
  }
  r
}
.memberDend <- stats_.memberDend

# stats:::.midDend
stats_.midDend <- function(x) {
  if (is.null(mp <- attr(x, "midpoint"))) 0 else mp
}
.midDend <- stats_.midDend # copied so that they would work inside the various functions here...


# stats:::midcache.dendrogram
stats_midcache.dendrogram <- function(x, type = "hclust", quiet = FALSE) {
  type <- match.arg(type)
  stopifnot(inherits(x, "dendrogram"))
  setmid <- function(d, type) {
    if (is.leaf(d)) {
      return(d)
    }
    k <- length(d)
    if (k < 1) {
      stop("dendrogram node with non-positive #{branches}")
    }
    r <- d
    midS <- 0
    for (j in 1L:k) {
      r[[j]] <- unclass(setmid(d[[j]], type))
      midS <- midS + .midDend(r[[j]])
    }
    if (!quiet && type == "hclust" && k != 2) {
      warning("midcache() of non-binary dendrograms only partly implemented")
    }
    attr(r, "midpoint") <- (.memberDend(d[[1L]]) + midS) / 2
    r
  }
  setmid(x, type = type)
}
midcache.dendrogram <- stats_midcache.dendrogram


# stats:::plotNode
stats_plotNode <- function(x1, x2, subtree, type, center, leaflab, dLeaf, nodePar,
                           edgePar, horiz = FALSE) {
  inner <- !is.leaf(subtree) && x1 != x2
  yTop <- attr(subtree, "height")
  bx <- plotNodeLimit(x1, x2, subtree, center)
  xTop <- bx$x
  hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
  if (!hasP) {
    nPar <- nodePar
  }
  if (getOption("verbose")) {
    cat(if (inner) {
      "inner node"
    } else {
      "leaf"
    }, ":")
    if (!is.null(nPar)) {
      cat(" with node pars\n")
      str(nPar)
    }
    cat(if (inner) {
      paste(" height", formatC(yTop), "; ")
    }, "(x1,x2)= (",
    formatC(x1, width = 4), ",", formatC(x2, width = 4),
    ")", "--> xTop=", formatC(xTop, width = 8), "\n",
    sep = ""
    )
  }
  Xtract <- function(nam, L, default, indx) rep(if (nam %in%
      names(L)) {
      L[[nam]]
    } else {
      default
    }, length.out = indx)[indx]
  asTxt <- function(x) if (is.character(x) || is.expression(x) ||
      is.null(x)) {
      x
    } else {
      as.character(x)
    }
  i <- if (inner || hasP) {
    1
  } else {
    2
  }
  if (!is.null(nPar)) {
    pch <- Xtract("pch", nPar, default = 1L:2, i)
    cex <- Xtract("cex", nPar, default = c(1, 1), i)
    col <- Xtract("col", nPar, default = par("col"), i)
    bg <- Xtract("bg", nPar, default = par("bg"), i)
    points(if (horiz) {
      cbind(yTop, xTop)
    } else {
      cbind(xTop, yTop)
    },
    pch = pch, bg = bg, col = col,
    cex = cex
    )
  }
  if (leaflab == "textlike") {
    p.col <- Xtract("p.col", nPar, default = "white", i)
  }
  lab.col <- Xtract("lab.col", nPar,
    default = par("col"),
    i
  )
  lab.cex <- Xtract("lab.cex", nPar, default = c(1, 1), i)
  lab.font <- Xtract("lab.font", nPar,
    default = par("font"),
    i
  )
  lab.xpd <- Xtract("xpd", nPar, default = c(TRUE, TRUE), i)
  if (is.leaf(subtree)) {
    if (leaflab == "perpendicular") {
      if (horiz) {
        X <- yTop + dLeaf * lab.cex
        Y <- xTop
        srt <- 0
        adj <- c(0, 0.5)
      }
      else {
        Y <- yTop - dLeaf * lab.cex
        X <- xTop
        srt <- 90
        adj <- 1
      }
      nodeText <- asTxt(attr(subtree, "label"))
      text(X, Y, nodeText,
        xpd = lab.xpd, srt = srt, adj = adj,
        cex = lab.cex, col = lab.col, font = lab.font
      )
    }
  }
  else if (inner) {
    segmentsHV <- function(x0, y0, x1, y1) {
      if (horiz) {
        segments(y0, x0, y1, x1,
          col = col, lty = lty,
          lwd = lwd
        )
      } else {
        segments(x0, y0, x1, y1,
          col = col, lty = lty,
          lwd = lwd
        )
      }
    }
    for (k in seq_along(subtree)) {
      child <- subtree[[k]]
      yBot <- attr(child, "height")
      if (getOption("verbose")) {
        cat("ch.", k, "@ h=", yBot, "; ")
      }
      if (is.null(yBot)) {
        yBot <- 0
      }
      xBot <- if (center) {
        mean(bx$limit[k:(k + 1)])
      } else {
        bx$limit[k] + .midDend(child)
      }
      hasE <- !is.null(ePar <- attr(child, "edgePar"))
      if (!hasE) {
        ePar <- edgePar
      }
      i <- if (!is.leaf(child) || hasE) {
        1
      } else {
        2
      }
      col <- Xtract("col", ePar,
        default = par("col"),
        i
      )
      lty <- Xtract("lty", ePar,
        default = par("lty"),
        i
      )
      lwd <- Xtract("lwd", ePar,
        default = par("lwd"),
        i
      )
      if (type == "triangle") {
        segmentsHV(xTop, yTop, xBot, yBot)
      }
      else {
        segmentsHV(xTop, yTop, xBot, yTop)
        segmentsHV(xBot, yTop, xBot, yBot)
      }
      vln <- NULL
      if (is.leaf(child) && leaflab == "textlike") {
        nodeText <- asTxt(attr(child, "label"))
        if (getOption("verbose")) {
          cat("-- with \"label\"", format(nodeText))
        }
        hln <- 0.6 * strwidth(nodeText, cex = lab.cex) / 2
        vln <- 1.5 * strheight(nodeText, cex = lab.cex) / 2
        rect(xBot - hln, yBot, xBot + hln, yBot + 2 *
          vln, col = p.col)
        text(xBot, yBot + vln, nodeText,
          xpd = lab.xpd,
          cex = lab.cex, col = lab.col, font = lab.font
        )
      }
      if (!is.null(attr(child, "edgetext"))) {
        edgeText <- asTxt(attr(child, "edgetext"))
        if (getOption("verbose")) {
          cat("-- with \"edgetext\"", format(edgeText))
        }
        if (!is.null(vln)) {
          mx <- if (type == "triangle") {
            (xTop + xBot + ((xTop - xBot) / (yTop - yBot)) *
              vln) / 2
          } else {
            xBot
          }
          my <- (yTop + yBot + 2 * vln) / 2
        }
        else {
          mx <- if (type == "triangle") {
            (xTop + xBot) / 2
          } else {
            xBot
          }
          my <- (yTop + yBot) / 2
        }
        p.col <- Xtract("p.col", ePar,
          default = "white",
          i
        )
        p.border <- Xtract("p.border", ePar,
          default = par("fg"),
          i
        )
        p.lwd <- Xtract("p.lwd", ePar,
          default = lwd,
          i
        )
        p.lty <- Xtract("p.lty", ePar,
          default = lty,
          i
        )
        t.col <- Xtract("t.col", ePar,
          default = col,
          i
        )
        t.cex <- Xtract("t.cex", ePar, default = 1, i)
        t.font <- Xtract("t.font", ePar,
          default = par("font"),
          i
        )
        vlm <- strheight(c(edgeText, "h"), cex = t.cex) / 2
        hlm <- strwidth(c(edgeText, "m"), cex = t.cex) / 2
        hl3 <- c(hlm[1L], hlm[1L] + hlm[2L], hlm[1L])
        if (horiz) {
          polygon(my + c(-hl3, hl3), mx + sum(vlm) *
            c(-1L:1L, 1L:-1L),
          col = p.col, border = p.border,
          lty = p.lty, lwd = p.lwd
          )
          text(my, mx, edgeText,
            cex = t.cex, col = t.col,
            font = t.font
          )
        }
        else {
          polygon(mx + c(-hl3, hl3), my + sum(vlm) *
            c(-1L:1L, 1L:-1L),
          col = p.col, border = p.border,
          lty = p.lty, lwd = p.lwd
          )
          text(mx, my, edgeText,
            cex = t.cex, col = t.col,
            font = t.font
          )
        }
      }
      plotNode(bx$limit[k], bx$limit[k + 1],
        subtree = child,
        type, center, leaflab, dLeaf, nodePar, edgePar,
        horiz
      )
    }
  }
  invisible()
}
plotNode <- stats_plotNode


# stats:::plotNodeLimit
stats_plotNodeLimit <- function(x1, x2, subtree, center) {
  inner <- !is.leaf(subtree) && x1 != x2
  if (inner) {
    K <- length(subtree)
    mTop <- .memberDend(subtree)
    limit <- integer(K)
    xx1 <- x1
    for (k in 1L:K) {
      m <- .memberDend(subtree[[k]])
      xx1 <- xx1 + (if (center) {
        (x2 - x1) * m / mTop
      } else {
        m
      })
      limit[k] <- xx1
    }
    limit <- c(x1, limit)
  }
  else {
    limit <- c(x1, x2)
  }
  mid <- attr(subtree, "midpoint")
  center <- center || (inner && !is.numeric(mid))
  x <- if (center) {
    mean(c(x1, x2))
  } else {
    x1 + (if (inner) {
      mid
    } else {
      0
    })
  }
  list(x = x, limit = limit)
}
plotNodeLimit <- stats_plotNodeLimit
