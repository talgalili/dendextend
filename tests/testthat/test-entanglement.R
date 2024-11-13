# library(testthat)


# fixing how the tests are printed
# cat("\n")

context("Measuring entanglement of two trees")


test_that("Match order of one dend based on another (using their labels)", {
  dend <- USArrests[1:4, ] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  expect_identical(order.dendrogram(dend), c(4L, 3L, 1L, 2L))

  dend_changed <- dend
  order.dendrogram(dend_changed) <- 1:4
  expect_identical(order.dendrogram(dend_changed), c(1:4))

  # now let's fix the order of the new object to be as it was:
  dend_changed <- match_order_by_labels(dend_changed, dend)
  expect_identical(order.dendrogram(dend_changed), order.dendrogram(dend))

  # producing an error due to different labels in the two trees:
  # Fails when tree sizes are different:
  expect_error(match_order_by_labels(dend_changed[[-1]], dend))
  # It would also fail when one of the labels is different:
  labels(dend_changed)[1] <- "CAT"
  expect_error(match_order_by_labels(dend_changed, dend))
})




test_that("Match order of one dend based on another (using their order valuess)", {
  dend <- as.dendrogram(hclust(dist(USArrests[1:4, ])))
  expect_identical(order.dendrogram(dend), c(4L, 3L, 1L, 2L))

  dend_changed <- dend
  order.dendrogram(dend_changed) <- 1:4
  expect_identical(order.dendrogram(dend_changed), c(1:4))

  # now let's fix the order of the new object to be as it was:
  dend_changed <- match_order_dendrogram_by_old_order(
    dend_changed, dend,
    order.dendrogram(dend_changed)
  )
  expect_identical(order.dendrogram(dend_changed), order.dendrogram(dend))

  # if check_that_labels_match and check_that_leaves_order_match parameters used
  dend_changed <- dend
  dend_changed <- match_order_dendrogram_by_old_order(
     dend_changed, dend,
     order.dendrogram(dend_changed),
     check_that_labels_match = T, check_that_leaves_order_match = T
  )
  expect_identical(order.dendrogram(dend_changed), order.dendrogram(dend))

  # Watch this!
  # take a dend and change it:
  dend_changed <- dend
  dend_changed <- rev(dend_changed)
  expect_false(identical(order.dendrogram(dend_changed), order.dendrogram(dend)))
  # we keep the order of dend_change, so that the leaves order are synced
  # with their labels JUST LIKE dend:
  old_dend_changed_order <- order.dendrogram(dend_changed)
  # now we change dend_changed leaves order values:
  order.dendrogram(dend_changed) <- 1:4
  # and we can fix them again, based on their old kept leaves order:

  #    # alabama and alaska are different
  #    data.frame(order.dendrogram(dend),
  #    labels(dend),
  #    order.dendrogram(dend_changed),
  #    labels(dend_changed))

  #    a = match_order_dendrogram_by_old_order(dend_changed, dend,
  #                                        order.dendrogram(dend))
  #    order.dendrogram(a)
  #    labels(a)

  # I am not sure this works right...
  #    dend_changed <- match_order_dendrogram_by_old_order(dend_changed, dend,
  #                                                        old_dend_changed_order)
  #    expect_identical(order.dendrogram(dend_changed), order.dendrogram(dend))


  #    expect_identical(order.dendrogram(dend_changed), order.dendrogram(dend))
  #    identical(match_order_by_labels(dend_changed, dend), dend_changed)
  
  # if labels do not match
  dend1 <- as.dendrogram(hclust(dist(iris[1:10, -5])))
  dend2 <- as.dendrogram(hclust(dist(iris[2:11, -5])))
  dend3 <- as.dendrogram(hclust(dist(iris[1:9, -5])))
  expect_error(
    match_order_dendrogram_by_old_order(dend1, dend2, check_that_labels_match = T)
  )
  # if label order does not match
  expect_error(expect_warning(
     match_order_dendrogram_by_old_order(dend1, dend3, check_that_leaves_order_match = T)
  ))
})





test_that("Entanglement works", {
  hc1 <- hclust(dist(datasets::iris[, -5]), "com")
  hc2 <- hclust(dist(datasets::iris[, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)

  expect_identical(round(entanglement(dend1, dend2, 0, "labels"), 2), 1)
  expect_identical(round(entanglement(dend1, dend2, 1, "labels"), 2), 0.93)
  expect_identical(round(entanglement(dend1, dend2, 1.5, "labels"), 2), 0.91)
  expect_identical(round(entanglement(dend1, dend2, 2, "labels"), 2), 0.89)

  expect_identical(
    round(entanglement(dend1, dend2, 2, "labels"), 2),
    round(entanglement(dend1, dend2, 2, "order"), 2)
  )

  #    library(microbenchmark)
  #     microbenchmark(entanglement(dend1 , dend2, 2, "labels"),
  #                    entanglement(dend1 , dend2, 2, "order"), times = 10L )   # order is about twice as fast...
  
  # if entanglement calculated for non dend objects
  expect_error(
     entanglement.default(dend1, dend2)
  )
  expect_identical(
     round(entanglement.hclust(hc1, hc2, 1.5, "labels"), 2),
     0.91
  )
  ph1 <- ape::as.phylo(hc1)
  ph2 <- ape::as.phylo(hc2)
  expect_identical(
     round(entanglement.phylo(ph1, ph2, 1.5, "labels"), 2),
     0.91
  )
  dend12 <- dendlist(dend1, dend2)
  expect_error(
     entanglement.dendlist(dend12, 1.5, "labels")
  )
  expect_error(
     entanglement.dendlist(dend12[1], 1.5, "labels")
  )
  
})





test_that("Entanglement with labels vs order", {
  hc1 <- hclust(dist(iris[, -5]), "com")
  hc2 <- hclust(dist(iris[, -5]), "single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)

  # massing up the order of leaves is dangerous:
  expect_identical(round(entanglement(dend1, dend2, 1.5, "order"), 2), 0.91)
  order.dendrogram(dend2) <- seq_len(nleaves(dend2))
  # this 0.95 number is NO LONGER correct!!
  expect_identical(round(entanglement(dend1, dend2, 1.5, "order"), 2), 0.95)
  # but if we use the "labels" method - we still get the correct number:
  expect_identical(round(entanglement(dend1, dend2, 1.5, "labels"), 2), 0.91)

  # however, we can fix our dend2, as follows:
  dend2 <- match_order_by_labels(dend2, dend1)
  # Now that labels and order are matched - entanglement is back at working fine:
  expect_identical(round(entanglement(dend1, dend2, 1.5, "order"), 2), 0.91)
})


test_that("Entanglement works for dendlist objects ", {
  dend1 <- iris[, -5] %>%
    dist() %>%
    hclust("com") %>%
    as.dendrogram()
  dend2 <- iris[, -5] %>%
    dist() %>%
    hclust("sin") %>%
    as.dendrogram()
  dend12 <- dendlist(dend1, dend2)
  expect_identical(
    entanglement(dend12, L = .25),
    entanglement(dend1, dend2, L = .25)
  )
})
