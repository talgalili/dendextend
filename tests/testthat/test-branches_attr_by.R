# library(testthat)

context("branches_attr_by_")


test_that("is.infinite works", {
  expect_false(is.infinite(NA))
  expect_false(is.infinite(as.numeric(NA)))
  expect_true(is.infinite(Inf))
  expect_false(is.infinite("Inf"))
  expect_true(is.infinite(as.numeric("Inf")))
})


test_that("assign_values_to_branches_edgePar - when trying to keep a value un-touched", {
  a <- c(1:2) %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()

  # this is how we want it to look:
  # Original remains:
  #    str(assign_values_to_branches_edgePar(a, value = c(1,"Inf"), edgePar = "col"))
  #    str(assign_values_to_branches_edgePar(a, value = c(1,Inf), edgePar = "col"))
  # A change is made:
  #    str(assign_values_to_branches_edgePar(a, value = c(1,NULL), edgePar = "col")) # since it recycles 1
  #    str(assign_values_to_branches_edgePar(a, value = c(1,NA), edgePar = "col")) # One is 1, and the other NA.



  # this is false because one has
  # attr(*, "edgePar")= Named num 1
  # and the other:
  # attr(*, "edgePar")= Named chr "1"
  expect_false(
    identical(
      assign_values_to_branches_edgePar(a, value = c(1, "Inf"), edgePar = "col"),
      assign_values_to_branches_edgePar(a, value = c(1, Inf), edgePar = "col")
    )
  )
  #    str(unclass(assign_values_to_branches_edgePar(a, value = c(1,"Inf"), edgePar = "col")))
  #    str(unclass(assign_values_to_branches_edgePar(a, value = c(1,Inf), edgePar = "col")))


  # answer %>% unclass %>% str
  # plot(answer)
  # dput(answer)
  answer <- assign_values_to_branches_edgePar(a, value = c(1, "Inf"), edgePar = "col")
  true_answer <- structure(list(
    structure(1L, label = 1L, members = 1L, height = 0, leaf = TRUE),
    structure(2L, label = 2L, members = 1L, height = 0, leaf = TRUE, edgePar = structure(list(
      col = "1"
    ), .Names = "col"))
  ), members = 2L, midpoint = 0.5, height = 1, edgePar = structure(list(
    col = "1"
  ), .Names = "col"), class = "dendrogram")
  expect_true(
    identical(answer, true_answer)
  )

  answer <- assign_values_to_branches_edgePar(a, value = c(1, Inf), edgePar = "col")
  true_answer <- structure(list(
    structure(1L, label = 1L, members = 1L, height = 0, leaf = TRUE),
    structure(2L, label = 2L, members = 1L, height = 0, leaf = TRUE, edgePar = structure(list(
      col = 1
    ), .Names = "col"))
  ), members = 2L, midpoint = 0.5, height = 1, edgePar = structure(list(
    col = 1
  ), .Names = "col"), class = "dendrogram")

  expect_true(
    identical(answer, true_answer)
  )



  answer <- assign_values_to_branches_edgePar(a, value = c(1, NULL), edgePar = "col")
  true_answer <- structure(list(structure(1L, label = 1L, members = 1L, height = 0, leaf = TRUE, edgePar = structure(list(
    col = 1
  ), .Names = "col")), structure(2L, label = 2L, members = 1L, height = 0, leaf = TRUE, edgePar = structure(list(
    col = 1
  ), .Names = "col"))), members = 2L, midpoint = 0.5, height = 1, edgePar = structure(list(
    col = 1
  ), .Names = "col"), class = "dendrogram")
  expect_true(
    identical(answer, true_answer)
  )


  answer <- assign_values_to_branches_edgePar(a, value = c(1, NA), edgePar = "col")
  true_answer <- structure(list(structure(1L, label = 1L, members = 1L, height = 0, leaf = TRUE, edgePar = structure(list(
    col = NA_real_
  ), .Names = "col")), structure(2L, label = 2L, members = 1L, height = 0, leaf = TRUE, edgePar = structure(list(
    col = 1
  ), .Names = "col"))), members = 2L, midpoint = 0.5, height = 1, edgePar = structure(list(
    col = 1
  ), .Names = "col"), class = "dendrogram")
  expect_true(
    identical(answer, true_answer)
  )
})



# dend_node <- 1
# dend_node
# attr(dend_node, "edgePar")[["a"]] <- list(1) # doesn't work
# attr(dend_node, "edgePar")["a"] <- list(1) # works!
# attr(dend_node, "edgePar")["b"] <- list(1) # works!
# attr(dend_node, "edgePar")[["b"]] <- list(1) # doesn't work



test_that("branches_attr_by_labels works", {
  dend <- c(1:4) %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  L <- list(1:2, 3)
  tmp <- dend %>% branches_attr_by_lists(L)
  expect_equal(attr(tmp[[1]], "edgePar")$col, 2)
  expect_equal(attr(tmp[[2]], "edgePar")$col, 2)
  expect_equal(attr(tmp[[1]][[1]], "edgePar")$col, 1)
  expect_equal(attr(tmp[[1]][[2]], "edgePar")$col, 1)
  expect_equal(attr(tmp[[2]][[1]], "edgePar")$col, 2)
  expect_equal(attr(tmp[[2]][[2]], "edgePar")$col, 1)

  # with changed col/lwd/lty
  tmp <- dend %>%
    branches_attr_by_lists(L, TF_value = "blue") %>%
    branches_attr_by_lists(L, attr = "lwd", TF_value = 4) %>%
    branches_attr_by_lists(L, attr = "lty", TF_value = 3)
  expect_equal(attr(tmp[[1]], "edgePar")$col, "blue")
  expect_equal(attr(tmp[[1]], "edgePar")$lwd, 4)
  expect_equal(attr(tmp[[1]], "edgePar")$lty, 3)

  # when individual features are selected
  tmp <- dend %>% branches_attr_by_lists(c(1, 4))
  expect_equal(attr(tmp[[1]], "edgePar")$col, 2)
  expect_equal(attr(tmp[[2]], "edgePar")$col, 2)
  expect_equal(attr(tmp[[1]][[1]], "edgePar")$col, 2)
  expect_equal(attr(tmp[[1]][[2]], "edgePar")$col, 1)
  expect_equal(attr(tmp[[2]][[1]], "edgePar")$col, 1)
  expect_equal(attr(tmp[[2]][[2]], "edgePar")$col, 2)
})
