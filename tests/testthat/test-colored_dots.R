# library(testthat)
# library(dendextend)


context("colored_dots")


test_that("zero_range works", {
   
   x <- c(1)
   expect_true(
      zero_range(x)
   )
   x <- c(1, 2, 3)
   expect_error(
      zero_range(x)
   )   
   x <- c(1, NA)
   expect_true(
      is.na(zero_range(x))
   )   
   x <- c(1, 1)
   expect_true(
      zero_range(x)
   )   
   x <- c(-Inf, Inf)
   expect_false(
      zero_range(x)
   )   
   x <- c(0, 1)
   expect_false(
      zero_range(x)
   )   
   x <- c(1,2)
   expect_false(
      zero_range(x)
   )   
   
})


test_that("rescale works", {
   
   x <- c(1, 2)
   expect_identical(
      rescale(x),
      c(0, 1)
   )
   x <- c(1, 1)
   expect_identical(
      rescale(x),
      c(0.5, 0.5)
   )
   
})


test_that("rotated_str_dim works", {
   
   plot.new()
   expect_identical(
      round(rotated_str_dim("input string"), 3),
      c(xh = 0.037, yh = 0.171)
   )
   
})


test_that("max_labels_height works", {
   
   plot.new()
   expect_identical(
      round(max_labels_height("input string"), 2),
      0.17
   )
   
})


test_that("colored_dots works", {
   
   dend <- as.dendrogram(hclust(dist(mtcars)))
   
   comp_names <- unlist(lapply(rownames(mtcars), function(x) strsplit(x, " ")[[1]][[1]]))
   top_three <- sort(table(comp_names), decreasing = TRUE)[1:3]
   top_three <- sapply(names(top_three), function(x) grepl(x, labels(dend)))
   top_three <- as.data.frame(top_three)
   
   colorblind_friendly <- c("#1b9e77", "#d95f02", "#7570b3")
   colored_dataframe <- top_three
   
   for (i in 1:3) {
      colored_dataframe[top_three[, i], i] <- colorblind_friendly[[i]]
      colored_dataframe[!top_three[, i], i] <- "#00000000"
   }
   colored_dataframe_2 <- colored_dataframe
   colnames(colored_dataframe_2) <- NULL
   
   dend <- color_branches(dend, h = 170)
   dend <- color_labels(dend, h = 170)
   
   # general use
   plot(dend)
   expect_no_error(
      colored_dots(colored_dataframe, as.hclust(dend), horiz = FALSE, add = T, sort_by_labels_order = FALSE)   
   )
   expect_no_error(
      colored_dots(colored_dataframe_2, as.hclust(dend), horiz = FALSE, add = T, sort_by_labels_order = FALSE)   
   )
   
   # if horizontal
   plot(dend, horiz = T)
   expect_no_error(
      colored_dots(colored_dataframe, add = F, horiz = TRUE)   
   )
   expect_no_error(
      colored_dots(colored_dataframe_2, add = F, horiz = TRUE)   
   )
   
   # if non-dendrogram object passed in
   plot(dend)
   expect_error(
      colored_dots(colored_dataframe, mtcars)   
   )
   # if wrong number of colors passed in
   expect_error(
      colored_dots(colored_dataframe[1:31,], dend)   
   )
   
})


