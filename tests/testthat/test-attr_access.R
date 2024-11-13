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
  
  # if non-dendrogram object is passed in and 'attribute' parameter is missing
  expect_warning(expect_error(
     get_leaves_attr(hc)   
  ))
  
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
  
  # if non-dendrogram object passed in
  expect_error(expect_warning(
     get_leaves_nodePar(hc)
  ))
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




test_that("get_branches_heights on a simple dendrogram", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   result <- get_branches_heights(dend)
   expect_true(is.numeric(result))
   expect_true(length(result) > 0)
})

test_that("hang.dendrogram modifies leaf heights", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   hanged_dend <- hang.dendrogram(dend, hang = 0.1)
   expect_true(is.dendrogram(hanged_dend))
   expect_true(all(get_leaves_attr(dend, "height") == 0))
   expect_false(any(get_leaves_attr(hanged_dend, "height") == 0))
})

test_that("get_childrens_heights on a simple dendrogram", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   result <- get_childrens_heights(dend)
   expect_true(is.numeric(result))
})

test_that("rank_branches adjusts branch heights", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   ranked_dend <- rank_branches(dend)
   expect_true(is.dendrogram(ranked_dend))
   # Check if heights are adjusted
   heights <- sapply(ranked_dend, function(x) attr(x, "height"))
   expect_true(all(diff(heights) == 1))
})

test_that("assign_values_to_leaves_nodePar updates nodePar", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   updated_dend <- assign_values_to_leaves_nodePar(dend, value = 1:5, nodePar = "col")
   expect_false(identical(get_leaves_nodePar(dend),
                          get_leaves_nodePar(updated_dend)))
})


test_that("remove_branches_edgePar removes edgePar", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   dend <- assign_values_to_branches_edgePar(dend, value = 1, edgePar = "col")
   cleaned_dend <- remove_branches_edgePar(dend)
   expect_true(all(is.na(unlist(get_leaves_nodePar(cleaned_dend)))))
   # if non dendrogram object passed in
   expect_error(
      remove_branches_edgePar(1:4)
   )
})


test_that("remove_nodes_nodePar removes nodePar", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   dend <- assign_values_to_nodes_nodePar(dend, value = 1:5, nodePar = "col")
   cleaned_dend <- remove_nodes_nodePar(dend)
   expect_true(all(is.na(unlist(get_leaves_nodePar(cleaned_dend)))))
   # if non dendrogram object passed in
   expect_error(
      remove_nodes_nodePar(1:4)
   )   
})

test_that("remove_leaves_nodePar removes nodePar from leaves", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   dend <- assign_values_to_leaves_nodePar(dend, value = 1:5, nodePar = "col")
   cleaned_dend <- remove_leaves_nodePar(dend)
   leaf_cols <- sapply(cleaned_dend, function(x) if(is.leaf(x)) attr(x, "nodePar")$col else NA)
   expect_true(all(is.na(leaf_cols)))
   # if non dendrogram object passed in
   expect_error(
      remove_leaves_nodePar(1:4)
   )      
})

test_that("fix_members_attr.dendrogram updates members attribute", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   fixed_dend <- fix_members_attr.dendrogram(dend)
   expect_true(is.dendrogram(fixed_dend))
   # Check if members attribute is correctly updated
   members_attr <- sapply(fixed_dend, function(x) attr(x, "members"))
   expect_true(all(!is.na(members_attr)))
   # if non dendrogram object passed in
   expect_error(
      fix_members_attr.dendrogram(1:4)
   )      
})

test_that("rank_order.dendrogram updates leaf order", {
   dend <- as.dendrogram(hclust(dist(1:5)))
   ranked_dend <- rank_order.dendrogram(dend)
   expect_true(is.dendrogram(ranked_dend))
   # Check if order is correctly updated
   expect_equal(order.dendrogram(ranked_dend), rank(order.dendrogram(dend), ties.method = "first"))
   # if non dendrogram object passed in
   expect_error(
      rank_order.dendrogram(1:4)
   )         
})

test_that("get_leaves_edgePar works", {
   hc <- hclust(dist(1:5))
   dend <- as.dendrogram(hc)
   result <- get_leaves_edgePar(dend, simplify = T)
   expect_true(
      all(is.na(result))
   )
   # if non-dendrogram object passed in
   expect_error(expect_warning(
      get_leaves_edgePar(hc)
   ))
})

test_that("get_leaves_branches_attr works", {
   hc <- hclust(dist(1:5))
   # if non-dendrogram object passed in
   expect_error(expect_warning(
      get_leaves_branches_attr(hc)
   ))
})

test_that("get_nodes_attr works", {
   hc <- hclust(dist(1:5))
   dend <- as.dendrogram(hc)
   # if non-dendrogram object passed in
   expect_error(expect_warning(
      get_nodes_attr(hc)
   )) 
   dendextend_options("warn", T)
   expect_warning(
      result <- get_nodes_attr(dend, "fake attribute")   
   )
   dendextend_options("warn", F)
   expect_true(
      all(is.na(result))
   )
})

test_that("rllply works", {
   x <- list(1, 2, list(31))
   result <- rllply(x, function(x) {x}, add_notation = TRUE)
   expect_identical(
      result[[1]][[1]],
      1
   )
   x <- 1:4
   result <- rllply(x, function(x) {x}, add_notation = TRUE)
   expect_identical(
      attributes(result)$position_type,
      "Leaf"
   )
})

test_that("hang.dendrogram works", {
   hc <- hclust(dist(1:5))
   dend <- as.dendrogram(hc)
   expect_no_error(
      hang.dendrogram(dend, hang = -1)
   )
   # if non-dendrogram object passed in
   expect_error(
      hang.dendrogram(hc)
   )
})

test_that("rank_branches works", {
   hc <- hclust(dist(1:5))
   # if non-dendrogram object passed in
   expect_error(
      rank_branches(hc)
   )
})

test_that("assign_values_to_leaves_nodePar works", {
   hc <- hclust(dist(1:5))
   dend <- as.dendrogram(hc)
   # if non-dendrogram object passed in
   expect_error(
      assign_values_to_leaves_nodePar(hc)
   )   
   dendextend_options("warn", T)
   # if 'value' parameter missing
   expect_warning(
      assign_values_to_leaves_nodePar(dend)
   )
   # if 'value' recycled
   expect_warning(
      assign_values_to_leaves_nodePar(dend, c(1), warn = T)
   )
   dendextend_options("warn", F)
})

test_that("assign_values_to_nodes_nodePar works", {
   hc <- hclust(dist(1:5))
   dend <- as.dendrogram(hc)
   # if non-dendrogram object passed in
   expect_error(
      assign_values_to_nodes_nodePar(hc)
   )   
   dendextend_options("warn", T)
   # if 'value' parameter missing
   expect_warning(
      assign_values_to_nodes_nodePar(dend)
   )
   # if 'value' recycled
   expect_warning(
      assign_values_to_nodes_nodePar(dend, c(1), warn = T)
   )
   dendextend_options("warn", F)
   
   # testing remove nodePar if it is empty
   attr(dend[[1]][[1]], "nodePar") <- list()
   expect_no_error(
      result <- assign_values_to_nodes_nodePar(dend, rep(Inf, 10))   
   )

})

test_that("assign_values_to_branches_edgePar works", {
   hc <- hclust(dist(1:5))
   dend <- as.dendrogram(hc)
   # if non-dendrogram object passed in
   expect_error(
      assign_values_to_branches_edgePar(hc)
   )   
   dendextend_options("warn", T)
   # if 'value' parameter missing
   expect_warning(
      assign_values_to_branches_edgePar(dend)
   )
   # if 'value' recycled
   expect_warning(
      assign_values_to_branches_edgePar(dend, c(1), warn = T)
   )
   dendextend_options("warn", F)
   
   # if leaf passed in
   result <- assign_values_to_branches_edgePar(dend[[1]][[1]], 1:10, skip_leaves = T)
   expect_identical(
      result,
      dend[[1]][[1]]
   )
   
})

test_that("assign_values_to_leaves_edgePar works", {
   hc <- hclust(dist(1:5))
   dend <- as.dendrogram(hc)
   # if non-dendrogram object passed in
   expect_error(
      assign_values_to_leaves_edgePar(hc)
   )   
   dendextend_options("warn", T)
   # if 'value' parameter missing
   expect_warning(
      assign_values_to_leaves_edgePar(dend)
   )
   # if 'value' recycled
   expect_warning(
      assign_values_to_leaves_edgePar(dend, c(1), warn = T)
   )
   dendextend_options("warn", F)
   
   # if leaf passed in
   result <- assign_values_to_leaves_edgePar(dend[[1]][[1]], 1:10, skip_leaves = T)
   expect_identical(
      result,
      dend[[1]][[1]]
   )
   
   # if edgePar already present for a leaf
   attr(dend[[1]][[1]], "edgePar") <- list(test = 1)
   expect_no_error(
      assign_values_to_leaves_edgePar(dend, 1:10)
   )
})
