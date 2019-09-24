# library(testthat)

context("attr access (get/set)")


test_that("Get a dendrogram leaves attributes", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  expect_identical(get_leaves_attr(dend, "label"), labels(dend, "label"))
  expect_identical(get_leaves_attr(dend, "height"), rep(0, 3))
  expect_identical(get_leaves_attr(dend, "leaf"), rep(TRUE, 3))
  expect_identical(get_leaves_attr(dend, "members"), rep(1L, 3))

  expect_false(
    identical(get_leaves_attr(hang.dendrogram(dend), "height"), rep(0, 3))
  )

  # when using simplify=FALSE, we get back a list...
  expect_true(is.list(get_leaves_attr(dend, "height", simplify = FALSE)))
  expect_identical(unlist(get_leaves_attr(dend, "height", simplify = FALSE)), rep(0, 3))
})


test_that("Get a dendrogram nodes attributes", {
  hc <- USArrests[1:3, ] %>%
    dist() %>%
    hclust("ave")
  dend <- as.dendrogram(hc)

  expect_error(get_nodes_attr(dend)) # we need attribute!


  # NA's are from the nodes which are not leaves:
  expect_identical(
    get_nodes_attr(dend, "label"),
    c(NA, "Arizona", NA, "Alabama", "Alaska")
  )

  # removing NA's
  expect_identical(get_nodes_attr(dend, "label", na.rm = TRUE), labels(dend, "label"))

  # leaves have 0 height
  expect_equal(get_nodes_attr(dend, "height"), c(54.8004107236398, 0, 37.1770090243957, 0, 0))

  # when excluding leaves - it replaces them with NA:
  expect_equal(get_nodes_attr(dend, "height", include_leaves = FALSE), c(54.8004107236398, NA, 37.1770090243957, NA, NA))

  # this gives ONLY the attribute of the branches nodes (not that of the leaves)
  expect_equal(get_nodes_attr(dend, "height", include_leaves = FALSE, na.rm = TRUE), c(54.8004107236398, 37.1770090243957))

  expect_identical(get_nodes_attr(dend, "leaf", na.rm = TRUE), rep(TRUE, 3))

  # how to make get_nodes_attr act like get_leaves_attr
  expect_identical(
    get_leaves_attr(dend, "members"), # should be 1's
    get_nodes_attr(dend, "members", include_branches = FALSE, na.rm = TRUE)
  )

  expect_identical(
    get_leaves_attr(dend, "height"), # should be 1's
    get_nodes_attr(dend, "height", include_branches = FALSE, na.rm = TRUE)
  )

  expect_identical(get_nodes_attr(dend, "members"), c(3L, 1L, 2L, 1L, 1L))


  expect_identical(
    get_nodes_attr(dend, "members", simplify = FALSE),
    list(3L, 1L, 2L, 1L, 1L)
  )

  # dealing with a missing/junk attribute:
  expect_identical(
    get_nodes_attr(dend, "blablabla"),
    c(NA, NA, NA, NA, NA)
  )

  # check the id paramter:
  expect_identical(
    get_nodes_attr(dend, "member", id = c(1, 3)),
    c(3L, 2L)
  )


  dend <- 1:3 %>%
    dist() %>%
    hclust() %>%
    as.dendrogram() %>%
    set("branches_k_color", k = 2) %>%
    set("branches_lwd", c(1.5, 1, 1.5)) %>%
    set("branches_lty", c(1, 1, 3, 1, 1, 2)) %>%
    set("labels_colors") %>%
    set("labels_cex", c(.9, 1.2))
  # plot(dend)
  # dput(get_nodes_attr(dend, "nodePar"))
  should_be <- list(
    NA, structure(list(lab.col = "#CC476B", pch = NA, lab.cex = 0.9), .Names = c(
      "lab.col",
      "pch", "lab.cex"
    )), NA, structure(list(
      lab.col = "#228B00", pch = NA,
      lab.cex = 1.2
    ), .Names = c("lab.col", "pch", "lab.cex")),
    structure(list(lab.col = "#0082CE", pch = NA, lab.cex = 0.9), .Names = c(
      "lab.col",
      "pch", "lab.cex"
    ))
  )
  expect_identical(get_nodes_attr(dend, "nodePar"), should_be)
})


# \dontrun{
# library(microbenchmark)
# # get_leaves_attr is twice faster than get_nodes_attr
# microbenchmark(   get_leaves_attr(dend, "members"), # should be 1's
#                     get_nodes_attr(dend, "members", include_branches = FALSE, na.rm = TRUE)
# )
# }



test_that("Get a dendrogram's branches heights", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  # expect_equal(dendextend_get_branches_heights(dend),c(37.1770090243957, 54.8004107236398))
  # expect_equal(dendextend_options("get_branches_heights")(dend),c(37.1770090243957, 54.8004107236398))
  expect_equal(get_branches_heights(dend), c(37.1770090243957, 54.8004107236398))

  expect_identical(
    get_branches_heights(dend),
    sort(get_nodes_attr(dend, "height", include_leaves = FALSE, na.rm = TRUE))
  )
})




test_that("get_leaves_nodePar works", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  # get_leaves_attr(dend) # error :)
  expect_identical(get_leaves_nodePar(dend, simplify = TRUE), rep(NA, 3))

  labels_colors(dend) <- 1:3
  should_be <- structure(c(1L, NA, 2L, NA, 3L, NA), .Names = c(
    "lab.col", "pch",
    "lab.col", "pch", "lab.col", "pch"
  ))
  expect_identical(get_leaves_nodePar(dend, simplify = TRUE), should_be)

  dend <- assign_values_to_leaves_nodePar(dend, 2, "lab.cex")
  #    dput(get_leaves_nodePar(dend))
  should_be <- list(structure(list(lab.col = 1L, pch = NA, lab.cex = 2), .Names = c(
    "lab.col",
    "pch", "lab.cex"
  )), structure(list(lab.col = 2L, pch = NA, lab.cex = 2), .Names = c(
    "lab.col",
    "pch", "lab.cex"
  )), structure(list(lab.col = 3L, pch = NA, lab.cex = 2), .Names = c(
    "lab.col",
    "pch", "lab.cex"
  )))
  expect_identical(get_leaves_nodePar(dend), should_be)
})



test_that("Hanging dendrogram works just like hclust", {
  hc <- hclust(dist(USArrests[1:5, ]), "ave")
  dend <- as.dendrogram(hc)

  expect_true(identical(
    as.dendrogram(hc, hang = 0.1),
    hang.dendrogram(dend, hang = 0.1)
  ))
})



test_that("Hanging dendrogram works for unbranched trees", {
  hc <- hclust(dist(USArrests[1:5, ]), "ave")
  dend <- as.dendrogram(hc)

  unbranched_dend <- unbranch(dend, 2)

  # we can't hclust an unbranched tree...
  expect_error(as.hclust(unbranched_dend))

  #    plot(hang.dendrogram(unbranched_dend, hang = 0.1))

  # showing that we can hang an unbranched tree
  expect_false(identical(
    unbranched_dend,
    hang.dendrogram(unbranched_dend, hang = 0.1)
  ))
})






test_that("Assigning several values to a tree's leaves nodePar", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  dend <- suppressWarnings(assign_values_to_leaves_nodePar(dend, 2, "lab.cex"))
  dend <- suppressWarnings(assign_values_to_leaves_nodePar(dend, value = c(3, 2), nodePar = "lab.col"))

  dend_leaf_nodePar <- get_leaves_attr(dend, "nodePar", simplify = FALSE)[[1]]

  # notice how pch is added automatically!
  expect_identical(length(dend_leaf_nodePar), 3L)
  expect_identical(names(dend_leaf_nodePar), c("lab.cex", "pch", "lab.col"))
  should_be <- structure(list(lab.cex = 2, pch = NA, lab.col = 3), .Names = c(
    "lab.cex",
    "pch", "lab.col"
  ))
  expect_identical(dend_leaf_nodePar, should_be)
})




test_that("We can remove leaves nodePar", {
  dend <- as.dendrogram(hclust(dist(USArrests[1:5, ])))
  dend <- color_labels(dend, 3)

  expect_false(is.null(get_leaves_attr(dend, "nodePar")))
  expect_true(is.null(get_leaves_attr(remove_leaves_nodePar(dend), "nodePar")))
})



test_that("rank_branches work", {
  dend <- as.dendrogram(hclust(dist(USArrests[1:5, ])))

  expect_equal(round(get_branches_heights(dend)), c(23, 37, 63, 109))

  expect_equal(get_branches_heights(rank_branches(dend)), c(1, 1, 2, 3))
})




test_that("fix_members_attr.dendrogram work", {
  dend <- as.dendrogram(hclust(dist(USArrests[1:5, ])))

  # ruin members
  attr(dend, "members") <- 1
  fixed_dend <- fix_members_attr.dendrogram(dend)

  expect_equal(attr(fixed_dend, "members"), 5)
})
