# library(testthat)

# we don't need the warning now... https://stackoverflow.com/questions/16194212/how-to-supress-warnings-globally-in-an-r-script
old_warn_opt <- options()$warn
options(warn = -1)


context("Checknig set.dendrogram")


test_that("labels options works", {
  suppressWarnings(RNGversion("3.5.0"))
  #    library(magrittr)
  set.seed(23235)
  ss <- sample(1:150, 10)
  dend <- iris[ss, -5] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()

  expect_equal(
    dend %>% set("labels", 1:10) %>% labels(),
    1:10
  )

  expect_equal(
    dend %>% set("labels", as.character(1:10)) %>% labels(),
    as.character(1:10)
  )

  dendextend_options("warn", TRUE)
  expect_warning(set(dend, "labels_color"))
  dendextend_options("warn", FALSE)


  # before doing anything, we have NULL labels colors:
  expect_null(dend %>% labels_colors())

  # piping is the same as not (just MUCH more readable)
  expect_equal(
    set(dend, "labels_color"),
    dend %>% set("labels_color")
  )

  # here we update the colors, and then try to see them:
  expect_equal(
    labels_colors(set(dend, "labels_color")),
    dend %>% set("labels_color") %>% labels_colors()
  )


  new_col_labels <- structure(c(
    "#CC476B", "#B76100", "#917600", "#518600", "#009232",
    "#009681", "#008FB7", "#1678D5", "#A352D1", "#CB39AA"
  ), .Names = c(
    "123",
    "145", "126", "109", "23", "29", "94", "59", "67", "97"
  ))
  #    dend %>% set("labels_color") %>% plot
  expect_equal(
    dend %>% set("labels_color") %>% labels_colors(),
    new_col_labels
  )
  #    dend %>% set("labels_color", new_col_labels) %>% plot


  # we get the correct attribue set...
  tmp <- dend %>%
    set("labels_col", 2) %>%
    set("labels_cex", 1.2)
  tmp <- tmp[[1]][[1]]
  #   unclass(tmp)
  expect_equal(
    attr(tmp, "nodePar")$lab.col,
    2
  )
  expect_equal(
    attr(tmp, "nodePar")$lab.cex,
    1.2
  )
})


test_that("leaves options works", {
  #    library(magrittr)
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(23235)
  ss <- sample(1:150, 10)
  dend <- iris[ss, -5] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()

  tmp <- dend
  tmp <- tmp %>%
    set("leaves_pch", 2) %>%
    set("leaves_cex", 1.5) %>%
    set("leaves_col", c(3:1)) %>%
    set("hang")
  tmp <- tmp[[1]][[1]]
  #    unclass(tmp)
  expect_equal(
    attr(tmp, "nodePar")[["pch"]],
    2
  )
  expect_equal(
    attr(tmp, "nodePar")[["cex"]],
    1.5
  )
  expect_equal(
    attr(tmp, "nodePar")[["col"]],
    3
  )
  expect_equal(
    attr(tmp, "height"),
    0.9030533
  )

  #    tmp  %>% plot
})


test_that("branches options works", {
  #    library(magrittr)
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(23235)
  ss <- sample(1:150, 10)
  dend <- iris[ss, -5] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()

  tmp <- dend %>%
    set("branches_k_col", c(3, 1, 2), k = 3)
  #    tmp %>% plot

  expect_equal(
    unname(unlist(get_nodes_attr(tmp, "edgePar"))[1:3]),
    c(NA, 3, 3)
  )

  #    as.data.frame(get_nodes_attr(tmp, "edgePar"))
  #    unclass(tmp)

  tmp <- dend
  tmp <- tmp %>%
    set("branches_col", c(1, 2, 1, 2, Inf)) %>%
    set("branches_lwd", c(2, 1, 2)) %>%
    set("branches_lty", c(1, 2, 1)) # %>% plot

  # checking we got a nice list:
  # dput(attr(tmp,"edgePar"))
  should_be <- structure(list(col = 1, lwd = 2, lty = 1), .Names = c(
    "col",
    "lwd", "lty"
  ))
  expect_equal(attr(tmp, "edgePar"), should_be)
})



test_that("clearing options works", {
  #    library(magrittr)
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(23235)
  ss <- sample(1:150, 10)
  # Getting the dend object onces
  dend <- iris[ss, -5] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()

  tmp <- dend
  tmp <- tmp %>%
    set("leaves_pch", c(19, 19, Inf)) %>%
    set("labels_color", c(19, 19, Inf)) # %>%
  # 	   set("clear_leaves") %>% plot
  expect_identical(dend, set(tmp, "clear_leaves"))

  tmp <- dend
  tmp <- tmp %>%
    set("branches_col", c(1, 2, 1, 2, Inf)) %>%
    set("branches_lwd", c(2, 1, 2)) %>%
    set("branches_lty", c(1, 2, 1)) # %>% plot
  # We can remove all the branch attributes
  expect_false(identical(dend, tmp))
  expect_identical(dend, set(tmp, "clear_branches"))
})


options(warn = old_warn_opt)
