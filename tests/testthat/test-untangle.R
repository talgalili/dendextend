# library(testthat)
RNGversion("3.5.0")

context("Untangle two dendrograms for plotting a tanglegram")


test_that("shuffle works", {
  #    library(magrittr)
  dend <- USArrests %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  set.seed(234238)
  dend2 <- dend %>% shuffle()

  # different tree layouts:
  #    tanglegram(dend, dend2, margin_inner=7)
  expect_identical(round(entanglement(dend, dend2), 3), 0.398)
  expect_false(identical(dend, dend2))

  # same topology
  expect_true(identical(sort(dend), sort(dend2)))

  dend12 <- dendlist(dend, dend2)
  expect_false(identical(dend12, shuffle(dend12)))
  expect_equal(length(shuffle(dend12)), 2)
})



test_that("Flip leaves work", {
  dend1 <- USArrests[1:5, ] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  dend2 <- flip_leaves(dend1, c(3, 5), c(1, 2))
  # tanglegram(dend1,dend2)
  # tanglegram(dend1,flip_leaves(dend1, c(3,5, 1,2), c(4)))

  expect_identical(entanglement(dend1, dend2, L = 2), 0.4)
})



test_that("all_couple_rotations_at_k work", {
  dend1 <- USArrests[1:5, ] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  dend2 <- all_couple_rotations_at_k(dend1, k = 2)[[2]]
  # tanglegram(dend1,dend2)

  expect_identical(entanglement(dend1, dend2, L = 2), 0.5)
})




test_that("untangle_step_rotate_1side work", {
  dend1 <- USArrests[1:10, ] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  set.seed(3525)
  dend2 <- shuffle(dend1)
  #    tanglegram(dend1,dend2)
  expect_identical(round(entanglement(dend1, dend2, L = 2), 2), 0.47)

  # Fixing the problem :)
  dend2_corrected <- untangle_step_rotate_1side(dend2, dend1)[[1]]
  #    tanglegram(dend1,dend2_corrected) # FIXED.
  expect_identical(round(entanglement(dend1, dend2_corrected, L = 2), 2), 0)

  # the other direction may also work:
  dend2_corrected <- untangle_step_rotate_1side(dend2, dend1, direction = "backward")[[1]]
  #    tanglegram(dend1,dend2_corrected) # FIXED.
  expect_identical(round(entanglement(dend1, dend2_corrected, L = 2), 2), 0)
})




test_that("untangle_step_rotate_2side work", {
  dend1 <- USArrests[1:10, ] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  set.seed(3525645)
  dend2 <- USArrests[1:10, ] %>%
    dist() %>%
    hclust(method = "med") %>%
    as.dendrogram() %>%
    shuffle()
  #    tanglegram(dend1,dend2)
  dend1 <- sort(dend1)
  dend2 <- sort(dend2)
  expect_identical(round(entanglement(dend1, dend2, L = 2), 2), 0.21)


  # this is behaving different for R 3.3.3 and 3.4 - I'm not sure why...
  #
  # # Fixing the problem :)
  # dend12_corrected <- suppressWarnings(
  #    untangle_step_rotate_2side(dend1, dend2, L = 2, print_times=FALSE, max_n_iterations = 20)
  #    )
  #
  # #    tanglegram(dend12_corrected[[1]],dend12_corrected[[2]]) # FIXED.
  # expect_identical(round(entanglement(dend12_corrected[[1]],dend12_corrected[[2]], L = 2),3) ,  0.036)
  #
  #
})




#
# # This one is an example of how 2side rotation is better. But it takes longer
# # So I don't run it:
#
# test_that("untangle_step_rotate_2side works better than 1side",{
#
#
#    dend1 <- as.dendrogram(hclust(dist(USArrests[1:20,])))
#    dend2 <- as.dendrogram(hclust(dist(USArrests[1:20,]), method = "single"))
#    set.seed(3525)
#    dend2 <- shuffle(dend2)
# #    tanglegram(dend1,dend2, margin_inner=6.5)
#    expect_identical(round(entanglement(dend1,dend2, L = 2),2) ,  0.79)
#
#    dend2_corrected <- untangle_step_rotate_1side(dend2, dend1)
# #    tanglegram(dend1,dend2_corrected, margin_inner=6.5) # Good.
#    expect_identical(round(entanglement(dend1,dend2_corrected, L = 2),3) ,  0.007)
#    # it is better, but not perfect. Can we improve it?
#
#    dend12_corrected <- untangle_step_rotate_2side(dend1, dend2,print_times=FALSE)
# #    tanglegram(dend12_corrected[[1]],dend12_corrected[[2]], margin_inner=6.5) # Better...
#    expect_identical(round(entanglement(dend12_corrected[[1]],dend12_corrected[[2]], L=2),3) ,  0.005)
#
#    # best combination:
#    dend12_corrected_1 <- untangle_random_search(dend1, dend2)
#    dend12_corrected_2 <- untangle_step_rotate_2side(dend12_corrected_1[[1]],dend12_corrected_1[[2]],print_times=FALSE)
# #    tanglegram(dend12_corrected_2[[1]],dend12_corrected_2[[2]], margin_inner=6.5) # Better...
#    expect_identical(round(entanglement(dend12_corrected_2[[1]],dend12_corrected_2[[2]], L=2),3) ,  0)
#     # 0 - PERFECT.
#
# })
#




test_that("untangle (main function) works for random search", {
  dend1 <- USArrests[1:5, ] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  dend2 <- flip_leaves(dend1, c(3, 5), c(1, 2))
  # tanglegram(dend1,dend2)
  # tanglegram(dend1,flip_leaves(dend1, c(3,5, 1,2), c(4)))
  dend12 <- dendlist(dend1, dend2)

  set.seed(123123)
  out1 <- untangle(dend12, method = "ran", R = 10)
  set.seed(123123)
  out2 <- untangle_random_search(dend1, dend2, R = 10)
  expect_identical(out1, out2)
})




test_that("untangle (main function) works for 2 step", {
  dend1 <- USArrests[1:5, ] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  dend2 <- flip_leaves(dend1, c(3, 5), c(1, 2))
  # tanglegram(dend1,dend2)
  # tanglegram(dend1,flip_leaves(dend1, c(3,5, 1,2), c(4)))
  dend12 <- dendlist(dend1, dend2)

  out1 <- untangle(dend12, method = "step2")
  set.seed(123123)
  out2 <- untangle_step_rotate_2side(dend1, dend2)
  expect_identical(out1, out2)
})
