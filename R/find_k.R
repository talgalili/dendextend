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


### Remove dependancy on fpc by importing pamk to this file.
### #' @importFrom fpc pamk
### NULL
# fpc::pamk
# Author: Christian Hennig c.hennig@ucl.ac.uk http://www.homepages.ucl.ac.uk/~ucakche/

# fpc::dudahart2
dudahart2 <- function(x, clustering, alpha = 0.001) {
  x <- as.matrix(x)
  p <- ncol(x)
  n <- nrow(x)
  cln <- rep(0, 2)
  W <- matrix(0, p, p)
  for (i in 1:2) cln[i] <- sum(clustering == i)
  for (i in 1:2) {
    clx <- x[clustering == i, ]
    cclx <- cov(as.matrix(clx))
    if (cln[i] < 2) {
      cclx <- 0
    }
    W <- W + ((cln[i] - 1) * cclx)
  }
  W1 <- (n - 1) * cov(as.matrix(x))
  dh <- sum(diag(W)) / sum(diag(W1))
  z <- qnorm(1 - alpha)
  compare <- 1 - 2 / (pi * p) - z * sqrt(2 * (1 - 8 / (pi^2 * p)) / (n *
    p))
  qz <- (-dh + 1 - 2 / (pi * p)) / sqrt(2 * (1 - 8 / (pi^2 * p)) / (n *
    p))
  p.value <- 1 - pnorm(qz)
  cluster1 <- dh >= compare
  out <- list(
    p.value = p.value, dh = dh, compare = compare,
    cluster1 = cluster1, alpha = alpha, z = z
  )
  out
}

# fpc::calinhara
calinhara <- function(x, clustering, cn = max(clustering)) {
  x <- as.matrix(x)
  p <- ncol(x)
  n <- nrow(x)
  cln <- rep(0, cn)
  W <- matrix(0, p, p)
  for (i in 1:cn) cln[i] <- sum(clustering == i)
  for (i in 1:cn) {
    clx <- x[clustering == i, ]
    cclx <- cov(as.matrix(clx))
    if (cln[i] < 2) {
      cclx <- 0
    }
    W <- W + ((cln[i] - 1) * cclx)
  }
  S <- (n - 1) * cov(x)
  B <- S - W
  out <- (n - cn) * sum(diag(B)) / ((cn - 1) * sum(diag(W)))
  out
}

# fpc::distcritmulti
distcritmulti <- function(x, clustering, part = NULL, ns = 10, criterion = "asw",
                          fun = "dist", metric = "euclidean", count = FALSE,
                          seed = NULL, ...) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  n <- length(clustering)
  if (is.null(part)) {
    pn1 <- n %/% ns
    pn2 <- n %% ns
    part <- rep(pn1, ns)
    part[ns] <- part[ns] + pn2
  }
  np <- sum(part)
  ns <- length(part)
  n <- length(clustering)
  npsam <- sample(n, np)
  cp <- cumsum(part)
  ss <- list()
  ss[[1]] <- npsam[1:cp[1]]
  asw <- numeric(0)
  for (i in 2:ns) ss[[i]] <- npsam[(cp[i - 1] + 1):cp[i]]
  for (i in 1:ns) {
    if (count) {
      cat("Subset ", i, "\n")
    }
    if (fun == "dist") {
      dx <- dist(x[ss[[i]], ], method = metric, ...)
    } else {
      dx <- cluster::daisy(x[ss[[i]], ], metric = metric, ...)
    }
    if (criterion == "asw") {
      asw[i] <- summary(cluster::silhouette(
        clustering[ss[[i]]],
        dx
      ))$avg.width
    }
    if (criterion == "pearsongamma") {
      asw[i] <- cluster.stats(dx, clustering[ss[[i]]],
        silhouette = FALSE
      )$pearsongamma
    }
  }
  aswav <- sum(part * asw) / np
  crit.sd <- sd(asw)
  out <- list(
    crit.overall = aswav, crit.sub = asw, crit.sd = crit.sd,
    subsets = ss
  )
  out
}

# fpc::cluster.stats
cluster.stats <- function(d = NULL, clustering, alt.clustering = NULL, noisecluster = FALSE,
                          silhouette = TRUE, G2 = FALSE, G3 = FALSE, wgap = TRUE, sepindex = TRUE,
                          sepprob = 0.1, sepwithnoise = TRUE, compareonly = FALSE,
                          aggregateonly = FALSE) {
  if (!is.null(d)) {
    d <- as.dist(d)
  }
  cn <- max(clustering)
  clusteringf <- as.factor(clustering)
  clusteringl <- levels(clusteringf)
  cnn <- length(clusteringl)
  if (cn != cnn) {
    warning("clustering renumbered because maximum != number of clusters")
    for (i in 1:cnn) clustering[clusteringf == clusteringl[i]] <- i
    cn <- cnn
  }
  n <- length(clustering)
  noisen <- 0
  cwn <- cn
  if (noisecluster) {
    noisen <- sum(clustering == cn)
    cwn <- cn - 1
  }
  diameter <- average.distance <- median.distance <- separation <- average.toother <- cluster.size <- within.dist <- between.dist <- numeric(0)
  for (i in 1:cn) cluster.size[i] <- sum(clustering == i)
  pk1 <- cluster.size / n
  pk10 <- pk1[pk1 > 0]
  h1 <- -sum(pk10 * log(pk10))
  corrected.rand <- vi <- NULL
  if (!is.null(alt.clustering)) {
    choose2 <- function(v) {
      out <- numeric(0)
      for (i in 1:length(v)) {
        out[i] <- ifelse(v[i] >= 2,
          choose(v[i], 2), 0
        )
      }
      out
    }
    cn2 <- max(alt.clustering)
    clusteringf <- as.factor(alt.clustering)
    clusteringl <- levels(clusteringf)
    cnn2 <- length(clusteringl)
    if (cn2 != cnn2) {
      warning("alt.clustering renumbered because maximum != number of clusters")
      for (i in 1:cnn2) alt.clustering[clusteringf == clusteringl[i]] <- i
      cn2 <- cnn2
    }
    nij <- table(clustering, alt.clustering)
    dsum <- sum(choose2(nij))
    cs2 <- numeric(0)
    for (i in 1:cn2) cs2[i] <- sum(alt.clustering == i)
    sum1 <- sum(choose2(cluster.size))
    sum2 <- sum(choose2(cs2))
    pk2 <- cs2 / n
    pk12 <- nij / n
    corrected.rand <- (dsum - sum1 * sum2 / choose2(n)) / ((sum1 +
      sum2) / 2 - sum1 * sum2 / choose2(n))
    pk20 <- pk2[pk2 > 0]
    h2 <- -sum(pk20 * log(pk20))
    icc <- 0
    for (i in 1:cn) {
      for (j in 1:cn2) {
        if (pk12[i, j] > 0) {
          icc <- icc + pk12[i, j] * log(pk12[i, j] / (pk1[i] *
            pk2[j]))
        }
      }
    }
    vi <- h1 + h2 - 2 * icc
  }
  if (compareonly) {
    out <- list(corrected.rand = corrected.rand, vi = vi)
  }
  else {
    dmat <- as.matrix(d)
    within.cluster.ss <- 0
    overall.ss <- nonnoise.ss <- sum(d^2) / n
    if (noisecluster) {
      nonnoise.ss <- sum(as.dist(dmat[
        clustering <= cwn,
        clustering <= cwn
      ])^2) / sum(clustering <= cwn)
    }
    ave.between.matrix <- separation.matrix <- matrix(0,
      ncol = cn, nrow = cn
    )
    di <- list()
    for (i in 1:cn) {
      cluster.size[i] <- sum(clustering == i)
      di <- as.dist(dmat[clustering == i, clustering ==
        i])
      if (i <= cwn) {
        within.cluster.ss <- within.cluster.ss + sum(di^2) / cluster.size[i]
        within.dist <- c(within.dist, di)
      }
      if (length(di) > 0) {
        diameter[i] <- max(di)
      } else {
        diameter[i] <- NA
      }
      average.distance[i] <- mean(di)
      median.distance[i] <- median(di)
      bv <- numeric(0)
      for (j in 1:cn) {
        if (j != i) {
          sij <- dmat[clustering == i, clustering ==
            j]
          bv <- c(bv, sij)
          if (i < j) {
            separation.matrix[i, j] <- separation.matrix[
              j,
              i
            ] <- min(sij)
            ave.between.matrix[i, j] <- ave.between.matrix[
              j,
              i
            ] <- mean(sij)
            if (i <= cwn & j <= cwn) {
              between.dist <- c(between.dist, sij)
            }
          }
        }
      }
      separation[i] <- min(bv)
      average.toother[i] <- mean(bv)
    }
    average.between <- mean(between.dist)
    average.within <- mean(within.dist)
    nwithin <- length(within.dist)
    nbetween <- length(between.dist)
    between.cluster.ss <- nonnoise.ss - within.cluster.ss
    ch <- between.cluster.ss * (n - noisen - cwn) / (within.cluster.ss *
      (cwn - 1))
    clus.avg.widths <- avg.width <- NULL
    if (silhouette) {
      sii <- silhouette(clustering, dmatrix = dmat)
      sc <- summary(sii)
      clus.avg.widths <- sc$clus.avg.widths
      if (noisecluster) {
        avg.width <- mean(sii[clustering <= cwn, 3])
      } else {
        avg.width <- sc$avg.width
      }
    }
    g2 <- g3 <- cn2 <- cwidegap <- widestgap <- sindex <- NULL
    if (G2) {
      splus <- sminus <- 0
      for (i in 1:nwithin) {
        splus <- splus + sum(within.dist[i] < between.dist)
        sminus <- sminus + sum(within.dist[i] > between.dist)
      }
      g2 <- (splus - sminus) / (splus + sminus)
    }
    if (G3) {
      sdist <- sort(c(within.dist, between.dist))
      sr <- nwithin + nbetween
      dmin <- sum(sdist[1:nwithin])
      dmax <- sum(sdist[(sr - nwithin + 1):sr])
      g3 <- (sum(within.dist) - dmin) / (dmax - dmin)
    }
    pearsongamma <- cor(c(within.dist, between.dist), c(rep(
      0,
      nwithin
    ), rep(1, nbetween)))
    dunn <- min(separation[1:cwn]) / max(diameter[1:cwn], na.rm = TRUE)
    acwn <- ave.between.matrix[1:cwn, 1:cwn]
    dunn2 <- min(acwn[upper.tri(acwn)]) / max(average.distance[1:cwn],
      na.rm = TRUE
    )
    if (wgap) {
      cwidegap <- rep(0, cwn)
      for (i in 1:cwn) {
        if (sum(clustering == i) > 1) {
          cwidegap[i] <- max(hclust(as.dist(dmat[clustering ==
            i, clustering == i]), method = "single")$height)
        }
      }
      widestgap <- max(cwidegap)
    }
    if (sepindex) {
      psep <- rep(NA, n)
      if (sepwithnoise | !noisecluster) {
        for (i in 1:n) {
          psep[i] <- min(dmat[i, clustering !=
            clustering[i]])
        }
        minsep <- floor(n * sepprob)
      }
      else {
        dmatnn <- dmat[clustering <= cwn, clustering <=
          cwn]
        clusteringnn <- clustering[clustering <= cwn]
        for (i in 1:(n - noisen)) {
          psep[i] <- min(dmatnn[
            i,
            clusteringnn != clusteringnn[i]
          ])
        }
        minsep <- floor((n - noisen) * sepprob)
      }
      sindex <- mean(sort(psep)[1:minsep])
    }
    if (!aggregateonly) {
      out <- list(
        n = n, cluster.number = cn, cluster.size = cluster.size,
        min.cluster.size = min(cluster.size[1:cwn]),
        noisen = noisen, diameter = diameter, average.distance = average.distance,
        median.distance = median.distance, separation = separation,
        average.toother = average.toother, separation.matrix = separation.matrix,
        ave.between.matrix = ave.between.matrix, average.between = average.between,
        average.within = average.within, n.between = nbetween,
        n.within = nwithin, max.diameter = max(diameter[1:cwn],
          na.rm = TRUE
        ), min.separation = sepwithnoise *
          min(separation) + (!sepwithnoise) * min(separation[1:cwn]),
        within.cluster.ss = within.cluster.ss, clus.avg.silwidths = clus.avg.widths,
        avg.silwidth = avg.width, g2 = g2, g3 = g3, pearsongamma = pearsongamma,
        dunn = dunn, dunn2 = dunn2, entropy = h1, wb.ratio = average.within / average.between,
        ch = ch, cwidegap = cwidegap, widestgap = widestgap,
        sindex = sindex, corrected.rand = corrected.rand,
        vi = vi
      )
    } else {
      out <- list(
        n = n, cluster.number = cn, min.cluster.size = min(cluster.size[1:cwn]),
        noisen = noisen, average.between = average.between,
        average.within = average.within, max.diameter = max(diameter[1:cwn],
          na.rm = TRUE
        ), min.separation = sepwithnoise *
          min(separation) + (!sepwithnoise) * min(separation[1:cwn]),
        ave.within.cluster.ss = within.cluster.ss / (n - noisen),
        avg.silwidth = avg.width, g2 = g2, g3 = g3, pearsongamma = pearsongamma,
        dunn = dunn, dunn2 = dunn2, entropy = h1, wb.ratio = average.within / average.between,
        ch = ch, widestgap = widestgap, sindex = sindex,
        corrected.rand = corrected.rand, vi = vi
      )
    }
  }
  out
}

pamk <- function(data, krange = 2:10, criterion = "asw", usepam = TRUE,
                 scaling = FALSE, alpha = 0.001, diss = inherits(data, "dist"),
                 critout = FALSE, ns = 10, seed = NULL, ...) {
  ddata <- as.matrix(data)
  if (!identical(scaling, FALSE)) {
    sdata <- scale(ddata, scale = scaling)
  } else {
    sdata <- ddata
  }
  cluster1 <- 1 %in% krange
  critval <- numeric(max(krange))
  pams <- list()
  for (k in krange) {
    if (usepam) {
      pams[[k]] <- cluster::pam(sdata, k, diss = diss, ...)
    } else {
      pams[[k]] <- cluster::clara(sdata, k, ...)
    }
    if (k != 1) {
      critval[k] <- switch(criterion, asw = pams[[k]]$silinfo$avg.width,
        multiasw = distcritmulti(sdata, pams[[k]]$clustering,
          seed = seed, ns = ns
        )$crit.overall, ch = ifelse(diss,
          cluster.stats(sdata, pams[[k]]$clustering)$ch,
          calinhara(sdata, pams[[k]]$clustering)
        )
      )
    }
    if (critout) {
      cat(k, " clusters ", critval[k], "\\n")
    }
  }
  k.best <- (1:max(krange))[which.max(critval)]
  if (cluster1) {
    if (diss) {
      cluster1 <- FALSE
    } else {
      cxx <- dudahart2(sdata, pams[[2]]$clustering, alpha = alpha)
      critval[1] <- cxx$p.value
      cluster1 <- cxx$cluster1
    }
  }
  if (cluster1) {
    k.best <- 1
  }
  out <- list(pamobject = pams[[k.best]], nc = k.best, crit = critval)
  out
}

#' @title Find the (estimated) number of clusters for a dendrogram using average silhouette width
#' @rdname find_k
#' @export
#' @description
#' This function estimates the number of clusters based on the maximal average \link[cluster]{silhouette} width
#' derived from running \link[cluster]{pam} on the \link[stats]{cophenetic} distance matrix of
#' the \link[stats]{dendrogram}. The output is based on the \link[fpc]{pamk} output.
#' @param dend A dendrogram (or hclust) tree object
#' @param krange integer vector. Numbers of clusters which are to be compared
#' by the average silhouette width criterion.
#' Note: average silhouette width and Calinski-Harabasz can't estimate number
#' of clusters nc=1. If 1 is included, a Duda-Hart test is applied and 1 is
#' estimated if this is not significant.
#' @param x An object of class "find_k" (has its own S3 plot method).
#' @param xlab,ylab,main parameters passed to plot.
#' @param ... passed to \link[fpc]{pamk} (the current defaults criterion="asw" and usepam=TRUE can not be changes).
#' @seealso
#' \link[fpc]{pamk}, \link[cluster]{pam}, \link[cluster]{silhouette}.
#' @return
#' A \link[fpc]{pamk} output. This is a list with the following components:
#' 1) pamobject - The output of the optimal run of the pam-function.
#' 2) nc	- the optimal number of clusters.
#' 3) crit - vector of criterion values for numbers of clusters. crit[1] is the p-value of the Duda-Hart test if 1 is in krange and diss=FALSE.
#' 4) k - a copy of nc (just to make it easier to extract - since k is often used in other functions)
#' @examples
#'
#' dend <- iris[, -5] %>%
#'   dist() %>%
#'   hclust() %>%
#'   as.dendrogram()
#' dend_k <- find_k(dend)
#' plot(dend_k)
#' plot(color_branches(dend, k = dend_k$nc))
#'
#' library(cluster)
#' sil <- silhouette(dend_k$pamobject)
#' plot(sil)
#'
#' dend <- USArrests %>%
#'   dist() %>%
#'   hclust(method = "ave") %>%
#'   as.dendrogram()
#' dend_k <- find_k(dend)
#' plot(dend_k)
#' plot(color_branches(dend, k = dend_k$nc))
find_k <- function(dend, krange = 2:min(10, (nleaves(dend) - 1)), ...) {
  # library(fpc)
  # krange = 2:10
  # criterion = "asw"
  d <- cophenetic(dend) # this will work for both a dendrogram and an hclust object.
  out <- pamk(d, krange = krange, criterion = "asw", usepam = TRUE, ...)
  out$k <- out$nc # just to make it easier to find.
  class(out) <- "find_k"
  out
}


#' @export
#' @rdname find_k
plot.find_k <- function(x,
                        xlab = "Number of clusters (k)",
                        ylab = "Average silhouette width",
                        main = "Estimating the number of clusters using\n average silhouette width",
                        ...) {
  asw <- x$crit
  k <- seq_along(x$crit)

  col <- rep("black", length(k))
  col[which.max(asw)] <- "red"

  plot(asw ~ k,
    xlab = xlab,
    ylab = ylab,
    main = main,
    type = "b",
    las = 1,
    col = col,
    ...
  )
}
