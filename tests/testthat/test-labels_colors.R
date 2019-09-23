context("Coloring tree's labels")


test_that("Leaves color is NULL", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  # Defaults:
  expect_equal(labels_colors(dend), NULL)
})




test_that("Setting and extracting leaves color", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  # let's add some color:
  labels_colors(dend) <- 2:4
  # getting it (without labels)
  expect_equal(labels_colors(dend, labels = FALSE), 2:4)
  # getting it (with labels)
  expect_equal(labels_colors(dend), structure(2:4, .Names = c("Arizona", "Alabama", "Alaska")))
})


test_that("Setting and extracting leaves color (some are NA)", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  # let's add some color:
  labels_colors(dend) <- c(2, NA, 4)
  # getting it (without labels)
  expect_equal(labels_colors(dend, labels = FALSE), c(2, NA, 4))
  # getting it (with labels)
  expect_equal(labels_colors(dend), structure(c(2, NA, 4), .Names = c("Arizona", "Alabama", "Alaska")))
})



test_that("Color recycling", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  # changing color to black, see that recycling works
  dendextend_options("warn", TRUE)
  expect_warning(labels_colors(dend) <- 1)
  dendextend_options("warn", FALSE)
  expect_equal(labels_colors(dend, labels = FALSE), rep(1, 3))
})


test_that("Color removing", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  labels_colors(dend) <- 2:4

  # removing color (and the nodePar completely - if it has no other attributed but lab.col)
  expect_warning(labels_colors(dend) <- NULL)
  expect_equal(labels_colors(dend), NULL)
})


test_that("Can't color a non-dendrogram object", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")

  expect_error(labels_colors(hc))
  expect_error(labels_colors(hc) <- NULL)
})




test_that("Can color the labels of super flat trees", {
  dend_flat <- structure(list(
    structure(88L, members = 1L, height = 0, label = "mel Ch   m pe", leaf = TRUE, value = 83),
    structure(list(
      structure(92L, members = 1L, height = 0, label = "mel Ch   f fo", leaf = TRUE, value = 84),
      structure(list(
        structure(90L, members = 1L, height = 0, label = "mel Ch   f he", leaf = TRUE, value = 85),
        structure(list(
          structure(84L, label = "mel Ch   m he", members = 1L, height = 0, leaf = TRUE, value = 86),
          structure(86L, label = "mel Ch   m fo", members = 1L, height = 0, leaf = TRUE, value = 87)
        ), members = 2L, midpoint = 0.5, height = 0, value = 86.5)
      ), members = 3L, midpoint = 0.75, height = 0, value = 85.75)
    ), members = 4L, midpoint = 0.875, height = 0, value = 84.875)
  ), members = 5L, midpoint = 0.9375, height = 0.0697674418604651, value = 83.9375, class = "dendrogram")
  ### didn't work in dendextend 0.17.2
  #    dend_flat %>% color_labels %>% plot
  #    dend_flat %>% color_labels(col = 1:2) %>% plot # doesn't works

  # no colors by default
  expect_null(dend_flat %>% labels_colors())
  # but now we do get colors:
  dend_col_1 <- dend_flat %>%
    color_labels() %>%
    labels_colors() %>%
    unname()
  expect_identical(
    dend_col_1,
    c("#CC476B", "#917600", "#009232", "#008FB7", "#A352D1")
  )
  dend_col_2 <- dend_flat %>%
    color_labels(col = 1:2) %>%
    labels_colors() %>%
    unname()
  expect_identical(
    dend_col_2,
    c(1L, 2L, 1L, 2L, 1L)
  )
  # dput(dend_col_2)
  # dput(dend_col_2)
})




test_that("Using set leaves_col still allows for set labels_colors", {
  dend <- USArrests[1:5, ] %>%
    dist() %>%
    hclust("ave") %>%
    as.dendrogram()
  dend <- dend %>%
    # set("leaves_pch", c(4, 1)) %>%
    set("leaves_col", c(5, 1)) %>%
    set("labels_colors", c(3, 1))

  # plot(dend)
  # str(unclass(dend))
  # dput(unname(labels_colors(dend)))

  expect_identical(unname(labels_colors(dend)), c(3, 1, 3, 1, 3))
})



# library(testthat)
