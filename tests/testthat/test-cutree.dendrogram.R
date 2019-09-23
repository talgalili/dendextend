# library(testthat)
# library(dendextend)

# set this for testing
dendextend_options("warn", TRUE)

context("Cutting a dendrogram")


test_that("Checking if a number is natural", {
  expect_true(is.natural.number(1)) # is TRUE
  expect_true(all(is.natural.number(seq(1, 5, by = 1))))
  expect_false(all(is.natural.number(seq(0, 5, by = 1))))
  expect_false(all(is.natural.number(seq(-1, 5, by = 0.5))))
  expect_error(is.natural.number("a"))
})



test_that("Turning factor into an integer", {
  x <- factor(2:4)
  expect_equal(as.numeric(x), 1:3)
  names(x) <- letters[x]
  expect_equal(as.numeric(x), 1:3)
  #    dput(fac2num(x))
  expect_identical(
    fac2num(x),
    structure(c(2, 3, 4), .Names = c("a", "b", "c"))
  )
  expect_identical(fac2num(x, keep_names = FALSE), c(2, 3, 4))
})


test_that("cutree a dendrogram by height h", {
  # data
  hc <- hclust(dist(USArrests[c(1, 6, 13, 20, 23), ]), "ave")
  dend <- as.dendrogram(hc)

  # we need h!
  expect_error(cutree_1h.dendrogram(dend))

  # the same as cutree
  expect_identical(
    cutree_1h.dendrogram(dend, h = 100),
    cutree(hc, h = 100)
  )

  expect_identical(
    cutree_1h.dendrogram(dend, h = 30),
    cutree(hc, h = 30)
  )

  # the same as cutree - also when there are NO clusters
  expect_identical(
    cutree_1h.dendrogram(dend, h = 1000),
    cutree(hc, h = 1000)
  )

  # the same as cutree - also when there are NO clusters
  expect_identical(
    cutree_1h.dendrogram(dend, h = 0),
    cutree(hc, h = 0)
  )
  expect_identical(
    cutree_1h.dendrogram(dend, h = 1),
    cutree(hc, h = 1)
  )

  # get return in the order of the dendrogram:
  expect_identical(
    names(cutree_1h.dendrogram(dend, 100, order_clusters_as_data = FALSE)),
    labels(dend)
  )

  # dealing with cutree_1h.dendrogram in negative h!
  expect_identical(
    cutree_1h.dendrogram(dend, h = -1),
    stats::cutree(as.hclust(dend), h = -1)
  )
  expect_identical(
    stats::cutree(as.hclust(dend), k = 5),
    stats::cutree(as.hclust(dend), h = -1)
  )
})





test_that("get dendrogram heights for k clusters", {
  # data
  hc <- hclust(dist(USArrests[c(1, 6, 13, 20, 23), ]), "ave")
  dend <- as.dendrogram(hc)
  unbranch_dend <- unbranch(dend, 2)

  # plot(unbranch_dend)

  dend_heights <- heights_per_k.dendrogram(dend)
  unbranch_dend_heights <- heights_per_k.dendrogram(unbranch_dend)

  #    cutree_1h.dendrogram(dend, h=dend_heights[[3]])

  expect_equal(length(dend_heights), 5)
  expect_equal(length(unbranch_dend_heights), 4)

  expect_equal(nnodes(unbranch_dend), 8)

  # dput(names(unbranch_dend_heights))
  expect_equal(names(unbranch_dend_heights), c("1", "3", "4", "5"))
})






test_that("cutree a dendrogram to k clusters", {
  # data
  hc <- hclust(dist(USArrests[c(1, 6, 13, 20, 23), ]), "ave")
  dend <- as.dendrogram(hc)
  unbranch_dend <- unbranch(dend, 2)

  #    plot(unbranch_dend)


  # the same as cutree
  expect_identical(
    cutree_1k.dendrogram(dend, k = 3),
    cutree(hc, k = 3)
  )

  expect_identical(
    cutree_1k.dendrogram(dend, k = 1),
    cutree(hc, k = 1)
  )

  # the same as cutree - also when there are NO clusters
  expect_identical(
    cutree_1k.dendrogram(dend, k = 5),
    cutree(hc, k = 5)
  )

  # if ignoring the "names" on the vector - the numbers will be identical:
  expect_identical(
    unname(cutree_1k.dendrogram(dend, k = 3, use_labels_not_values = FALSE)),
    unname(cutree(hc, k = 3))
  )


  # errors:
  expect_error(cutree_1k.dendrogram(dend)) # we need h!
  expect_error(cutree_1k.dendrogram(dend, k = -1))
  expect_error(cutree_1k.dendrogram(dend, k = 0))
  #    expect_error( cutree_1k.dendrogram(dend, k = 1.5)) # I no longer expect an error since it is turned into "as.integer"
  expect_error(cutree_1k.dendrogram(dend, k = 50))
  expect_error(cutree(hc, k = 50))


  # get return in the order of the dendrogram:
  expect_identical(
    names(cutree_1k.dendrogram(dend, k = 3, order_clusters_as_data = FALSE)),
    labels(dend)
  )

  # cases of no possible k's:
  expect_warning(cutree_1k.dendrogram(unbranch_dend, 2, warn = TRUE))
  expect_equal(cutree_1k.dendrogram(unbranch_dend, 2, warn = FALSE), rep(NA, 5))
})






test_that("cutree dendrogram method works for k", {
  # data
  hc <- USArrests[c(1, 6, 13, 20, 23), ] %>%
    dist() %>%
    hclust("ave")
  dend <- as.dendrogram(hc)
  unbranch_dend <- unbranch(dend, 2)

  #    plot(unbranch_dend)


  # the same as cutree
  expect_identical(
    cutree(dend, k = 3),
    cutree(hc, k = 3)
  )

  expect_identical(
    cutree(dend, k = 1),
    cutree(hc, k = 1)
  )

  # the same as cutree - also when there are NO clusters
  expect_identical(
    cutree(dend, k = 5),
    cutree(hc, k = 5)
  )

  # if ignoring the "names" on the vector - the numbers will be identical:
  expect_identical(
    unname(cutree(dend, k = 3, use_labels_not_values = FALSE)),
    unname(cutree(hc, k = 3))
  )


  # use_labels_not_values doesn't harm cutree
  expect_identical(
    cutree(dend, k = 3, use_labels_not_values = TRUE),
    cutree(dend, k = 3, use_labels_not_values = FALSE)
  )

  # use_labels_not_values doesn't harm cutree also when try_cutree_hclust=FALSE
  expect_identical(
    unname(cutree(dend, k = 3, use_labels_not_values = TRUE, try_cutree_hclust = FALSE)),
    unname(cutree(dend, k = 3, use_labels_not_values = FALSE, try_cutree_hclust = FALSE))
  )


  # errors:
  expect_error(cutree(dend)) # we need h!
  expect_error(cutree(dend, k = -1))
  expect_error(cutree(dend, k = 0))
  #    expect_error( cutree(dend, k = 1.5)) # I no longer expect an error since it is turned into "as.integer"
  expect_error(cutree(dend, k = 50))
  expect_error(cutree(hc, k = 50))


  # get return in the order of the dendrogram:
  expect_identical(
    names(cutree(dend, k = 3, order_clusters_as_data = FALSE, try_cutree_hclust = FALSE)),
    labels(dend)
  )

  expect_identical(
    names(cutree(dend, k = 3, order_clusters_as_data = FALSE, try_cutree_hclust = TRUE)),
    labels(dend)
  )

  # cases of no possible k's:
  expect_warning(cutree(unbranch_dend, 2))
  expect_equal(
    suppressWarnings(cutree(unbranch_dend, 2, warn = FALSE)),
    rep(0, 5)
  )

  # now to check vectorization
})





test_that("cutree for flat edges", {

  #    cutree(hclust(dist(c(1,1,1,2,2))), k=5)
  #    cutree(hclust(dist(c(1,1,1,2,2))), k=1:5)

  dend <- as.dendrogram(hclust(dist(c(1, 1, 1, 2, 2))))
  # dendextend:::cutree.dendrogram(dend, k=5)
  # as.hclust(dend) # Error: all(vapply(s, is.integer, NA)) is not TRUE
  #    cutree(dend,k=5)
  #    plot(dend)
  #    dendextend_cut_lower_fun(dend, -.5, labels)
  #    cut_lower_fun(dend, -.5, labels)

  expect_equal(unname(cutree(dend, k = 2)), c(1, 1, 1, 2, 2))
  expect_equal(unname(cutree(dend, h = -1)), 1:5) # weird definition
  expect_equal(unname(cutree(dend, k = 5)), 1:5)
  expect_warning(cutree(dend, k = 4, try_cutree_hclust = FALSE))
  expect_equal(suppressWarnings(cutree(dend, k = 4, try_cutree_hclust = FALSE)), rep(0, 5))

  # as of R R 3.2.4 (or 3.3.0 -not sure ) - as.hclust was fixed to deal better with ties on the branch heights.
  # That means that:
  # cutree(as.hclust(dend), k=4)
  # would work (it will give hard-to-interpret results - but it would work)

  # as.hclust(dend)
})








test_that("cutree for dendrogram works (k,h and vectorization)", {

  # data
  hc <- hclust(dist(USArrests[c(1, 6, 13, 20, 23), ]), "ave")
  dend <- as.dendrogram(hc)
  unbranch_dend <- unbranch(dend, 2)

  #    plot(unbranch_dend)



  # the same as cutree
  expect_identical(
    cutree(dend, k = 1:4),
    cutree(hc, k = 1:4)
  )

  expect_identical(
    cutree(dend, h = c(20, 25.5, 50, 170)),
    cutree(hc, h = c(20, 25.5, 50, 170))
  )

  expect_warning(cutree(unbranch_dend, k = 1:2))

  # it still works for missing k's, it just returns NA's in the second column
  cutree_unbranch_dend <- suppressWarnings(cutree(unbranch_dend, k = 1:4, warn = FALSE))
  expect_true(is.matrix(cutree_unbranch_dend))
  expect_true(all(cutree_unbranch_dend[, 2] == 0)) # 2nd column is NA.

  cutree_unbranch_dend_2 <- suppressWarnings(cutree(unbranch_dend,
    k = 1:4,
    warn = FALSE, order_clusters_as_data = FALSE,
    try_cutree_hclust = FALSE
  ))
  expect_identical(rownames(cutree_unbranch_dend_2), labels(unbranch_dend))
})




# test_that("Making cutted clusters be numbered from left to right",{


test_that("Testing sort_levels_values works", {

  # the function can return the same object:
  x <- 1:4
  names(x) <- letters[x]
  attr(x, "keep_me") <- "a cat"
  expect_equal(x, sort_levels_values(x))
  expect_identical(names(x), names(sort_levels_values(x)))
  expect_identical(attributes(x), attributes(sort_levels_values(x)))

  x <- c(4:1)
  names(x) <- letters[x]
  attr(x, "keep_me") <- "a cat"
  # it will keep the attributes as they are:
  expect_identical(attributes(x), attributes(sort_levels_values(x)))
  expect_equivalent(sort(x), sort_levels_values(x)) # not equal since "sort" removes the attr!

  x <- c(4:1, 4, 2)
  #    dput(sort_levels_values(x)) # 1 2 3 4 1 3
  expect_identical(sort_levels_values(x), c(1, 2, 3, 4, 1, 3))

  x <- c(2, 2, 3, 2, 1)
  expect_identical(sort_levels_values(x), c(1, 1, 2, 1, 3))

  # works when used on matrices:
  x <- matrix(1:16, 4, 4)
  rownames(x) <- letters[1:4]
  #    x
  expect_equal(x, apply(x, 2, sort_levels_values))


  x <- matrix(4:1, 2, 2)
  rownames(x) <- letters[1:2]
  #    x
  #    dput(apply(x, 2, sort_levels_values))
  expect_identical(
    apply(x, 2, sort_levels_values),
    structure(c(3, 4, 1, 2), .Dim = c(2L, 2L), .Dimnames = list(c(
      "a",
      "b"
    ), NULL))
  )

  # checking that sort_levels_values can be used on a matrix!
  x <- matrix(4:1, 2, 2)
  rownames(x) <- letters[1:2]
  #    x
  #    dput(apply(x, 2, sort_levels_values))
  expect_identical(apply(x, 2, sort_levels_values), sort_levels_values(x))
  # Yay!
})


test_that("Making cutted clusters be numbered from left to right", {
  hc <- hclust(dist(USArrests[c(1, 6, 13, 20, 23), ]), "ave")
  dend <- as.dendrogram(hc)

  sorted_cutree_hc_orig <- stats::cutree(hc, k = 1:4)
  sorted_cutree_hc <- dendextend:::cutree.hclust(hc, k = 1:4)
  sorted_cutree_dend <- dendextend:::cutree.dendrogram(dend, k = 1:4, try_cutree_hclust = FALSE)

  expect_identical(
    sorted_cutree_hc_orig,
    sorted_cutree_hc
  )


  expect_identical(
    mode(sorted_cutree_hc),
    mode(sorted_cutree_dend)
  )

  expect_identical(
    attributes(sorted_cutree_hc),
    attributes(sorted_cutree_dend)
  )

  # the same as cutree
  expect_identical(
    as.integer(cutree(dend, k = 1:4, try_cutree_hclust = FALSE)),
    as.integer(cutree(hc, k = 1:4))
  )

  expect_identical(
    as.vector(sorted_cutree_hc),
    as.vector(sorted_cutree_dend)
  ) # this is identical since we are forcing the numbers to be integers!

  expect_identical(
    (sorted_cutree_hc),
    (sorted_cutree_dend)
  ) # this is identical since we are forcing the numbers to be integers!
})



test_that("Compare labels which are character vs integer", {
  iris <- datasets::iris

  # they seem to be identical - but they are not in the way the are coerced!
  expect_identical(
    iris[1:150, -5],
    iris[, -5]
  )

  # once they are coerced into a matrix - they are NOT identical!
  # the rownames are now NULL!
  expect_false(identical(
    as.matrix(iris[1:150, -5]),
    as.matrix(iris[, -5])
  ))

  expect_false(identical(
    attributes(as.matrix(iris[1:150, -5])),
    attributes(as.matrix(iris[, -5]))
  ))

  expect_false(identical(
    rownames(as.matrix(iris[1:150, -5])),
    rownames(as.matrix(iris[, -5]))
  ))

  # it now has no rownames!
  expect_true(is.null(rownames(as.matrix(iris[, -5]))))

  # what about their dist - not the same!:
  expect_false(identical(
    dist(iris[1:150, -5]),
    dist(iris[, -5])
  ))
  # the first one has "labels" and the second one doesn't
  expect_false(identical(
    attributes(dist(iris[1:150, -5])),
    attributes(dist(iris[, -5]))
  ))

  d_iris <- dist(iris[, -5])
  hc_iris <- hclust(d_iris)
  dend_iris <- as.dendrogram(hc_iris)
  expect_true(is.integer(labels(dend_iris))) # this is a source of BUGS!
  expect_false(is.character(labels(dend_iris)))
})





test_that("heights_per_k.dendrogram", {
  dend15 <- c(1:5) %>%
    dist() %>%
    hclust(method = "average") %>%
    as.dendrogram()
  tmp <- heights_per_k.dendrogram(dend15)
  tmp_should_be <- structure(c(2.75, 2.25, 1.25, 0.75), .Names = c("1", "2", "3", "5"))
  expect_equal(tmp, tmp_should_be)

  dend15 <- c(1:5) %>%
    dist() %>%
    hclust(method = "sin") %>%
    as.dendrogram()
  # dput(tmp)
  expect_warning(tmp <- heights_per_k.dendrogram(dend15))
  tmp_should_be <- structure(c(Inf, -Inf), .Names = c("1", "5"))
  expect_equal(tmp, tmp_should_be)
})








# library(stats)
# library(dendextendRcpp)

#
# test_that("Having cutree work when using a subsetted tree",{
#    # Wo
#    # get a dendrogram:
# #    data(iris)
#    d_iris <- dist(datasets::iris[1:10,-5])
#    hc_iris <- hclust(d_iris)
#    dend_iris <- as.dendrogram(hc_iris) # as.hclust.dendrogram - of course
#
#    # taking a subset of the dendrogram:
#    sub_dend_iris <- dend_iris[[1]]
#    hc_sub_dend_iris <- as.hclust(sub_dend_iris)
#    # We will have NA's:
#    expect_true(any(is.na(stats:::labels.dendrogram(as.dendrogram(hc_sub_dend_iris )))))
#
#
#    #if(require(dendextendRcpp)) {
#    if("package:dendextendRcpp" %in% search()) {
#
# 	   # notice that for Rcpp this would be false since the returned vector
# 	   # has "NA" characters instead of NA:
# 	   expect_false(any(is.na(dendextendRcpp::labels.dendrogram(as.dendrogram(hc_sub_dend_iris )))))
# 	   # e.g: "NA" "3"  "NA" "NA" "4"  "7"
# 	#    a[which(a == "NA")] <- NA # this is NOT a good idea, in the case we have a label with "NA" as a character.
#    }
#
#
#    # we will get warnings, but the functions would not collapse!
#    expect_warning(
#       dendextend:::cutree.dendrogram(as.dendrogram(hc_sub_dend_iris ), 3,  try_cutree_hclust = TRUE)
#    )
#    expect_warning(
#       dendextend:::cutree.dendrogram(as.dendrogram(hc_sub_dend_iris ), 3,  try_cutree_hclust = FALSE)
#    )
#
#    # remove "iris" from the last test...
#    # if(exists("iris")) # it says it doesn't exists - but it does (in the gloval env)!
# #    suppressWarnings()
# #    rm(iris, pos = 1)
#
# })
#
#


dendextend_options("warn", FALSE)
