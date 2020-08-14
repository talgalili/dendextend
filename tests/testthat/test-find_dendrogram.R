# library(testthat)

context("find_dendrogram (in get_subdendrograms)")


test_that("find_dendrogram works", {
  # define dendrogram object to play with:
  dend <- 1:10 %>%
    dist() %>%
    hclust() %>%
    as.dendrogram() %>%
    set("labels_to_character") %>%
    color_branches(k = 5)
  
  selected_labels <- as.character(1:4)
  sub_dend <- find_dendrogram(dend, selected_labels)
  expect_true(nleaves(sub_dend) == 4L)
  
  selected_labels <- as.character(1:5)
  expect_null(find_dendrogram(dend, selected_labels))
  
  selected_labels <- as.character(1:20)
  expect_null(find_dendrogram(dend, selected_labels))

  # https://stackoverflow.com/questions/3903157/how-can-i-check-whether-a-function-call-results-in-a-warning
  # this function shouldn't return a warning
  selected_labels <- as.character(100:110)
  out <- tryCatch(find_dendrogram(dend, selected_labels),error=function(e) e, warning=function(w) w)
  expect_false(is(out, "warning"))
  
})

