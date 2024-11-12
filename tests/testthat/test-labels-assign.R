# library(testthat)

context("labels assignment")

test_that("labels assginment works for vectors", {
  x <- 1:3
  # expect_that(labels(x), equals(as.character(1:3))) # notice this is 1:3 as character
  expect_equal(labels(x), (as.character(1:3))) # notice this is 1:3 as character

  labels(x) <- letters[1:3]
  expect_equal(labels(x), (letters[1:3]))
  # labels(x) # [1] "a" "b" "c"
  labels(x)[1] <- "one"
  expect_equal(labels(x), c("one", "b", "c")) # checking specific assignment
})



# test_that("labels assginment works for matrix",{
# y <- matrix(1:9, 3,3)
# expect_equal(labels(y), (NULL))

# labels(y) <- letters[1:3] # defaults to assign labels to columns
# expect_equal(labels(y), (letters[1:3]))
# expect_equal(colnames(y), (letters[1:3]))

# labels(y, which = "rownames") <- letters[24:26]
# expect_equal(labels(y, which = "rownames"), letters[24:26])
# expect_equal(rownames(y), letters[24:26])

# labels(y)[1] <- "one"
# expect_equal(labels(y), c("one", "b", "c")) # checking specific assignment
# })




test_that("labels (with order=TRUE, by default), before and after assginment, works for hclust", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")

  #    plot(hc)
  #    expect_that(labels(hc), equals(c("Alabama", "Alaska", "Arizona")))
  expect_equal(labels(hc), c("Arizona", "Alabama", "Alaska"))

  labels(hc) <- letters[1:3]
  expect_equal(labels(hc), letters[1:3])

  labels(hc)[1] <- "one"
  expect_equal(labels(hc), c("one", "b", "c")) # checking specific assignment
})


test_that("labels (without order!) works for hclust", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  #    plot(hc)
  # expect_that(labels(hc, order = FALSE), equals(c("Alabama", "Alaska", "Arizona")))
  expect_equal(labels(hc, order = FALSE), c("Alabama", "Alaska", "Arizona"))
})


test_that("labels (without order!) works differently than labels assignment (which are WITH order) for hclust", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  #    plot(hc)
  # expect_that(labels(hc, order = FALSE), equals(c("Alabama", "Alaska", "Arizona")))
  expect_equal(labels(hc, order = FALSE), c("Alabama", "Alaska", "Arizona"))

  labels(hc) <- letters[1:3]

  # expect_that(identical(labels(hc, order = FALSE) , letters[1:3]),
  # is_false())
  expect_false(identical(labels(hc, order = FALSE), letters[1:3]))
})


test_that("labels assginment works for dendrogram", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  expect_equal(labels(dend), c("Arizona", "Alabama", "Alaska"))

  labels(dend) <- letters[1:3]
  expect_equal(labels(dend), letters[1:3])

  labels(dend)[1] <- "one"
  expect_equal(labels(dend), c("one", "b", "c")) # checking specific assignment
  
  # if 'value' parameter missing
  dendextend_options("warn", T)
  expect_warning(
     `labels<-`(dend)
  )
  dendextend_options("warn", F)
})


test_that("labels for hclust and dendrogram are (by default) the same", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)
  # hc and dend labels should NOT be identical.
  expect_true(identical(labels(dend), labels(hc)))
})




test_that("labels assginment recycles properly and consistently", {
  x <- 1:3
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)
  # y <- matrix(1:9, 3,3)

  suppressWarnings({
    labels(x) <- letters[1]
    labels(hc) <- letters[1]
    labels(dend) <- letters[1]
    # labels(y) <- letters[1] # defaults to assign labels to columns
    # labels(y, which = "rownames") <- letters[24]
  })

  expect_equal(labels(x), (rep(letters[1], 3)))
  expect_equal(labels(hc), (rep(letters[1], 3)))
  expect_equal(labels(dend), (rep(letters[1], 3)))
  # expect_equal(labels(y), (rep(letters[1], 3)))
  # expect_equal(labels(y, which = "rownames"), (rep(letters[24], 3)))
  # labels(x) # [1] "a" "b" "c"
})


test_that("labels assginment issues warning when using recycling", {
  x <- 1:3
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)
  # y <- matrix(1:9, 3,3)

  #    expect_that(labels(x) <- letters[1], gives_warning())
  expect_warning(labels(x) <- letters[1])
  expect_warning(labels(hc) <- letters[1])
  expect_warning(labels(dend) <- letters[1])
  # expect_warning(labels(y) <- letters[1])
  # expect_warning(labels(y, which = "rownames") <- letters[24])
})


test_that("labels assginment to dendrogram keeps the child nodes as NOT of dendrogram class", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)
  labels(dend) <- letters[1:3]

  expect_true(class(unclass(dend)[[2]]) == "list")
})



test_that("order of leaves in sub-dendrogram and as.hclust", {
  # For example:
  hc <- hclust(dist(USArrests[1:6, ]), "ave")
  dend <- as.dendrogram(hc)
  sub_dend <- dend[[1]]

  # bad order
  expect_equal(order.dendrogram(sub_dend), c(4, 6))
  # bad labels:
  #    expect_equal(labels(as.hclust(sub_dend)), as.character(rep(NA,2)))
  # since R 3.1.1-patched - the above will produce an error. (as it should)

  # let's fix it:
  order.dendrogram(sub_dend) <- rank(order.dendrogram(sub_dend), ties.method = "first")
  expect_equal(labels(as.hclust(sub_dend)), c("Arkansas", "Colorado"))
  # We now have labels :)
})



test_that("labels of hclust on data without rownames is an integer", {
  # this is a problem when some function assumes that labels are "character"
  DATA <- USArrests
  rownames(DATA) <- NULL
  hc <- hclust(dist(DATA))
  dend <- as.dendrogram(hc)
  expect_true(length(labels(hc)) == 0) # hc has NO labels
  expect_true(is.integer(labels(dend))) # the labels the dend gets are Integer
})






test_that("order of leaves can be extracted and changed", {
  hc <- hclust(dist(USArrests[1:3, ]), "ave")
  dend <- as.dendrogram(hc)

  expect_identical(order.dendrogram(dend), c(3L, 1L, 2L))

  # change order:
  order.dendrogram(dend) <- 1:3
  expect_identical(order.dendrogram(dend), 1:3)

  # change order (with replications):
  expect_warning(order.dendrogram(dend) <- c(1, 2))
  expect_identical(order.dendrogram(dend), as.integer(c(1, 2, 1)))
})


test_that("labels assignment works for phylo objects", {
   hc <- hclust(dist(USArrests[1:3, ]), "ave")
   ph <- ape::as.phylo(hc)
   
   labels(ph) <- c("Al", "Ak", "Az")
   expect_identical(
      labels.phylo(ph),
      c("Al", "Ak", "Az")
   )
   
   # if replacement labels are incorrect length
   expect_warning(
      labels(ph) <- c("Al", "Ak")   
   )
   # if 'value' parameter missing
   dendextend_options("warn", T)
   expect_warning(
      `labels<-.phylo`(ph)
   )
   dendextend_options("warn", F)
})


test_that("order.dendrogram works", {
   hc <- hclust(dist(USArrests[1:3, ]), "ave")
   dend <- as.dendrogram(hc)
   
   # if non-numeric order passed in
   expect_warning(
      order.dendrogram(dend) <- c("1", "2", "3")   
   )
   
})


test_that("order.hclust works", {
   x <- dist(USArrests[1:3, ])
   hc <- hclust(x, "ave")
   
   expect_equal(
      order.hclust(hc),
      c(3, 1, 2)
   )
   
   # if non-hclust object passed in
   expect_error(
      order.hclust(x)
   )
   
})


test_that("place_labels works", {
   hc <- hclust(dist(USArrests[1:3, ]), "ave")
   dend <- as.dendrogram(hc)
   
   result <- place_labels(dend, labels(dend))
   expect_identical(
      labels(result),
      c("Alaska", "Arizona", "Alabama")
   )
   
})
