# library(testthat)


# fixing how the tests are printed
# cat("\n")

context("ggdend")


test_that("as.ggdend.dendrogram works", {


  # get_nodes_attr(d1, "nodePar", simplify = FALSE)
  # plot(d1)

  dend <- 1:3 %>%
    dist() %>%
    hclust() %>%
    as.dendrogram() %>%
    set("branches_k_color", k = 2) %>%
    set("branches_lwd", c(1.5, 1, 1.5)) %>%
    set("branches_lty", c(1, 1, 3, 1, 1, 2)) %>%
    set("labels_colors") %>%
    set("labels_cex", c(.9, 1.2)) %>%
    set("nodes_pch", 19) %>%
    set("nodes_col", c("orange", "black", "NA"))

  gg1 <- as.ggdend(dend)
  # ggplot(gg1)
  # dput(gg1)
  should_be <- structure(list(
    segments = structure(list(x = c(
      1.75, 1, 1.75,
      2.5, 2.5, 2, 2.5, 3
    ), y = c(2, 2, 2, 2, 1, 1, 1, 1), xend = c(
      1,
      1, 2.5, 2.5, 2, 2, 3, 3
    ), yend = c(2, 0, 2, 1, 1, 0, 1, 0), col = c(
      "#CC476B",
      "#CC476B", "#009681", "#009681", "#009681", "#009681", "#009681",
      "#009681"
    ), lwd = c(1, 1, 1.5, 1.5, 1.5, 1.5, 1, 1), lty = c(
      1,
      1, 3, 3, 1, 1, 1, 1
    )), .Names = c(
      "x", "y", "xend", "yend", "col",
      "lwd", "lty"
    ), row.names = c(NA, 8L), class = "data.frame"),
    labels = structure(list(x = c(1, 2, 3), y = c(0, 0, 0), label = structure(1:3, .Label = c(
      "3",
      "1", "2"
    ), class = "factor"), col = c(
      "#CC476B", "#228B00",
      "#0082CE"
    ), cex = c(0.9, 1.2, 0.9)), .Names = c(
      "x", "y",
      "label", "col", "cex"
    ), row.names = c(NA, 3L), class = "data.frame"),
    nodes = structure(list(x = c(1.75, 1, 2.5, 2, 3), y = c(
      2,
      0, 1, 0, 0
    ), pch = c(19, 19, 19, 19, 19), cex = c(
      NA, NA,
      NA, NA, NA
    ), col = c("orange", "black", "NA", "orange", "black"), members = c(3L, 1L, 2L, 1L, 1L), midpoint = c(
      0.75, NA,
      0.5, NA, NA
    ), height = c(2, 0, 1, 0, 0), leaf = c(
      NA, TRUE,
      NA, TRUE, TRUE
    )), .Names = c(
      "x", "y", "pch", "cex", "col",
      "members", "midpoint", "height", "leaf"
    ), row.names = c(
      NA,
      -5L
    ), class = "data.frame")
  ), .Names = c(
    "segments", "labels",
    "nodes"
  ), class = "ggdend")
  expect_identical(gg1, should_be)



  dend <- 1:3 %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  gg2 <- as.ggdend(dend)
  #    dput(gg2)
  should_be <- structure(list(segments = structure(list(x = c(
    1.75, 1, 1.75,
    2.5, 2.5, 2, 2.5, 3
  ), y = c(2, 2, 2, 2, 1, 1, 1, 1), xend = c(
    1,
    1, 2.5, 2.5, 2, 2, 3, 3
  ), yend = c(2, 0, 2, 1, 1, 0, 1, 0), col = c(
    NA,
    NA, NA, NA, NA, NA, NA, NA
  ), lwd = c(
    NA, NA, NA, NA, NA, NA,
    NA, NA
  ), lty = c(NA, NA, NA, NA, NA, NA, NA, NA)), .Names = c(
    "x",
    "y", "xend", "yend", "col", "lwd", "lty"
  ), row.names = c(
    NA,
    8L
  ), class = "data.frame"), labels = structure(list(x = c(
    1,
    2, 3
  ), y = c(0, 0, 0), label = structure(1:3, .Label = c(
    "3",
    "1", "2"
  ), class = "factor"), col = c(NA, NA, NA), cex = c(
    NA,
    NA, NA
  )), .Names = c("x", "y", "label", "col", "cex"), row.names = c(
    NA,
    3L
  ), class = "data.frame"), nodes = structure(list(
    x = c(
      1.75,
      1, 2.5, 2, 3
    ), y = c(2, 0, 1, 0, 0), pch = c(
      NA, NA, NA, NA,
      NA
    ), cex = c(NA, NA, NA, NA, NA), col = c(NA, NA, NA, NA, NA),
    members = c(3L, 1L, 2L, 1L, 1L), midpoint = c(
      0.75, NA, 0.5,
      NA, NA
    ), height = c(2, 0, 1, 0, 0), leaf = c(
      NA, TRUE, NA,
      TRUE, TRUE
    )
  ), .Names = c(
    "x", "y", "pch", "cex", "col", "members",
    "midpoint", "height", "leaf"
  ), row.names = c(NA, -5L), class = "data.frame")), .Names = c(
    "segments",
    "labels", "nodes"
  ), class = "ggdend")
  ###
  expect_identical(gg2, should_be)
})

test_that("ggplot doesn't have warnings for dendrograms", {
  g <- ggplot(as.dendrogram(hclust(dist(mtcars))))
  expect_warning(ggplot_build(g), NA)
})
