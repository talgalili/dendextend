library(testthat)
context("Untangle two dendrograms for plotting a tanglegram")

test_that("shuffle works", {
  suppressWarnings(RNGversion("3.5.0"))
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
  
  # returns original dend if k == 1
  dend2 <- all_couple_rotations_at_k(dend1, k = 1)
  expect_identical(dend1, dend2)
  # uses first element of k if k is a vector and raises a warning
  expect_warning(
    dend2 <- all_couple_rotations_at_k(dend1, k = c(2, 3))
  )
  
  # if k_cluster_leaves is NA
  trace("all_couple_rotations_at_k", quote(k_cluster_leaves <- c(NA)), at = 8, print = FALSE)
  result <- all_couple_rotations_at_k(dend1, k = 4)
  untrace("all_couple_rotations_at_k")
  expect_identical(
     labels(result),
     "1"
  )
  
  # if km1_cluster_leaves is NA
  trace("all_couple_rotations_at_k", quote(km1_cluster_leaves <- c(NA)), at = 8, print = FALSE)
  expect_warning(
     all_couple_rotations_at_k(dend1, k = 4)     
  )
  untrace("all_couple_rotations_at_k")
  
})




test_that("untangle_step_rotate_1side work", {
  suppressWarnings(RNGversion("3.5.0"))
  dend1 <- USArrests[1:10, ] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
  set.seed(3525)
  dend2 <- shuffle(dend1)
  #    tanglegram(dend1,dend2)
  expect_identical(round(entanglement(dend1, dend2, L = 2), 2), 0.47)

  # Fixing the problem :)
  dend2_corrected <- untangle.dendrogram(dend2, dend1, "step1side", leaves_matching_method = "order")[[1]]
  #    tanglegram(dend1,dend2_corrected) # FIXED.
  expect_identical(round(entanglement(dend1, dend2_corrected, L = 2), 2), 0)

  # the other direction may also work:
  dend2_corrected <- untangle.dendrogram(dend2, dend1, "step1side", direction = "backward")[[1]]
  #    tanglegram(dend1,dend2_corrected) # FIXED.
  expect_identical(round(entanglement(dend1, dend2_corrected, L = 2), 2), 0)
  
  # if leaf mismatch in dend1 and dend after ordering
  dendextend_options("warn", T)
  expect_warning(expect_error(with_mock(
     match_order_by_labels = function(dend1, dend2, ...) return(dend2),
     untangle_step_rotate_1side(dend2, dend1, "step1side", leaves_matching_method = "order")
  )))
  dendextend_options("warn", F)
  
})




test_that("untangle_step_rotate_2side work", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)
   
   dend1 <- USArrests[1:20, ] %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   noisy_USArrests = USArrests[1:20, ] %>% scale() + rnorm(80) 
   dend2 <- noisy_USArrests %>%
      dist() %>%
      hclust(method = "med") %>%
      as.dendrogram() %>%
      shuffle()
   
   expect_identical(round(entanglement(dend1, dend2, L = 2), 2), 0.24)
   
   # enable print_times for test case but avoid cluttering output by capturing print statement
   # warnings suppressed as they were previously
   suppressWarnings(capture.output(
      # Fixing the problem :)
      dend12_corrected <- untangle_step_rotate_2side(dend1, dend2, L = 2, print_times = T, max_n_iterations = 20)
   ))
   expect_identical(round(entanglement(dend12_corrected[[1]],dend12_corrected[[2]], L = 2),3) ,  0.059)
   
   # if times reaches 2+ iterations
   expect_output(with_mock(
      untangle_step_rotate_1side = function(dend1, dend2, ...) return(list(shuffle(dend2), dend1)),
      result <- untangle_step_rotate_2side(dend1, dend2, k = 4, print_times = T)
   ))
   
})



test_that("untangle_step_rotate_both_side work", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)
   # Entanglement should be zero after applying algorithm, per Fig. 4 of 'Shuffle & untangle: novel untangle methods for solving the tanglegram layout problem' (Nguyen et al. 2022)
   example_labels <- c("Versicolor 90", "Versicolor 54", "Versicolor 81", "Versicolor 63", "Versicolor 72", "Versicolor 99", "Virginica 135", "Virginica 117", "Virginica 126", "Virginica 108", "Virginica 144", "Setosa 27", "Setosa 18", "Setosa 36", "Setosa 45", "Setosa 9")

   iris_modified <- datasets::iris
   iris_modified$Row <- seq_len(nrow(iris_modified))
   iris_modified$Label <- paste(tools::toTitleCase(as.character(iris_modified$Species)), iris_modified$Row)
   iris_modified <- iris_modified[iris_modified$Label %in% example_labels, ]
   
   iris_numeric <- iris_modified[,1:4]
   rownames(iris_numeric) <- iris_modified$Label
   
   dend1 <- as.dendrogram(hclust(dist(iris_numeric), method = "single"))
   dend2 <- as.dendrogram(hclust(dist(iris_numeric), method = "complete"))
   # enable print_times for test case but avoid cluttering output by capturing print statement
   capture.output(
      result <- untangle.dendrogram(dend1, dend2, "stepBothSides", print_times = T)  
   )
   dend1 <- result[[1]]
   dend2 <- result[[2]]
   expect_identical(entanglement(dend1, dend2, L = 2), 0)
   
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
  suppressWarnings(RNGversion("3.5.0"))
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
  suppressWarnings(RNGversion("3.5.0"))
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




test_that("untangle_best_k_to_rotate_by_1side works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(226)
   
   dend1 <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   dend2 <- shuffle(dend1)
   original_entanglement <- entanglement(dend1, dend2)
   expect_identical(round(original_entanglement, 3), 0.248)
   # resolve entanglement
   dend1_corrected <- untangle_best_k_to_rotate_by_1side(dend1, dend2)
   corrected_entanglement <- entanglement(dend1_corrected, dend2)
   
   # reduces entanglement from 0.248 to 0
   expect_identical(round(corrected_entanglement, 3), 0)
})




test_that("untangle_best_k_to_rotate_by_2side_backNforth works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(30) 
   
   dend1 <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   dend2 <- shuffle(dend1)
   original_entanglement <- entanglement(dend1, dend2)
   expect_identical(round(original_entanglement, 3), 0.251)
   # enable print_times for test case but avoid cluttering output by capturing print statement
   capture.output(
      # resolve entanglement
      dends_corrected <- untangle_best_k_to_rotate_by_2side_backNforth(dend1, dend2, L = 1, print_times = T)  
   )
   corrected_entanglement <- entanglement(dends_corrected[[1]], dends_corrected[[2]])
   
   # reduces entanglement from 0.251 to 0
   expect_identical(round(corrected_entanglement, 3), 0)
})




test_that("collapse_with_pipes works", {
   x <- c("before pipe ", " after pipe")
   collapsed_vector = collapse_with_pipes(x)
   expect_identical(collapsed_vector, "before pipe || after pipe")
})




test_that("untangle.default works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1) 
   
   dend1 <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() %>%
      as.dendrogram()
   dend2 <- shuffle(dend1)
   
   expect_error(untangle.default(dend1, dend2))
})




test_that("flip_1_and_2 works", {
   x <- c(1,2,2,1)
   flipped <- flip_1_and_2(x)
   expect_identical(flipped, c(2,1,1,2))
})




test_that("untangle_labels works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   # Create two example dendrograms with different label orders
   dend1 <- as.dendrogram(hclust(dist(USArrests[1:5, ]), method = "complete"))
   dend2 <- as.dendrogram(hclust(dist(USArrests[5:1, ]), method = "complete"))
   # use untangle_labels to reorder dend2 based on dend1
   result <- untangle.dendrogram(dend1, dend2, "labels")
   expect_identical(labels(result[[1]]), labels(result[[2]]))
})




test_that("untangle_DendSer works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(232)
   
   ss <- sample(1:150, 20)
   dend1 <- iris[ss, -5] %>%
      dist() %>%
      hclust("com") %>%
      as.dendrogram()
   dend2 <- iris[ss, -5] %>%
      dist() %>%
      hclust("sin") %>%
      as.dendrogram()
   dend12 <- dendlist(dend1, dend2)
   
   bad_entanglement =
   dend12 %>%
      untangle("step2") %>%
      entanglement()
   expect_identical(round(bad_entanglement, 3), 0.014)
   
   best_entanglement = 
      untangle.dendrogram(dend1, dend2, "DendSer") %>%
      untangle_DendSer() %>%
      untangle("step2") %>%
      entanglement()
   # reduces entanglement from 0.014 to 0
   expect_identical(best_entanglement, 0)
})




test_that("ladderize works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   dend1 <- as.dendrogram(hclust(dist(mtcars[1:5, ]), method = "average"))
   dend2 <- as.dendrogram(hclust(dist(mtcars[6:10, ]), method = "ward.D2"))
   
   result <- untangle.dendrogram(dend1, dend2, "ladderize")
   
   # check that both dendrograms were ladderized
   expect_false(identical(order.dendrogram(dend1), order.dendrogram(result[[1]])))
   expect_false(identical(order.dendrogram(dend2), order.dendrogram(result[[2]])))
   
   # the first dendrogram should be ladderized, but the second should remain the same
   result_partial <- untangle.dendrogram(dend1, dend2, "ladderize", right = TRUE, which = 1)
   expect_false(identical(order.dendrogram(dend1), order.dendrogram(result_partial[[1]])))
   expect_identical(order.dendrogram(dend2), order.dendrogram(result_partial[[2]]))
   
   # check that the orders should differ after changing the 'right' argument
   result_right_true <- untangle.dendrogram(dend1, dend2, "ladderize", right = TRUE)
   result_right_false <- untangle.dendrogram(dend1, dend2, "ladderize", right = FALSE)
   expect_false(identical(order.dendrogram(result_right_true[[1]]), order.dendrogram(result_right_false[[1]])))
   expect_false(identical(order.dendrogram(result_right_true[[2]]), order.dendrogram(result_right_false[[2]])))
})




test_that("untangle.dendlist works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   dend1 <- as.dendrogram(hclust(dist(mtcars[6:10, ]), method = "complete"))
   dend2 <- as.dendrogram(hclust(dist(mtcars[6:10, ]), method = "single"))
   dend3 <- as.dendrogram(hclust(dist(mtcars[6:10, ]), method = "average"))
   dend_list <- dendlist(dend1 = dend1, dend2 = dend2, dend3 = dend3)

   result <- untangle.dendlist(dend_list, method = "step2side")
   
   # entanglement improved from 0.522 to 0
   initial_entanglement <- entanglement(dend_list[[1]], dend_list[[2]])
   expect_identical(round(initial_entanglement, 3), 0.522)
   
   final_entanglement <- entanglement(result[[1]], result[[2]])
   expect_identical(round(final_entanglement, 3), 0)
})




test_that("untangle_random_search works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   dend1 <- as.dendrogram(hclust(dist(iris[1:50, -5]), method = "average"))
   dend2 <- as.dendrogram(hclust(dist(iris[50:1, -5]), method = "single"))
   
   result <- untangle_random_search(dend1, dend2, R = 10, leaves_matching_method = "order")
   
   # entanglement improved from 0.579 to 0.311
   initial_entanglement <-  entanglement(dend1, dend2)
   expect_identical(round(initial_entanglement, 3), 0.579)
   
   final_entanglement <- entanglement(result[[1]], result[[2]])
   expect_identical(round(final_entanglement, 3), 0.311)
   
   dendextend_options("warn", T)
   expect_warning(
      untangle_random_search(dend1, dend2, R = 10, leaves_matching_method = "order")
   )
   dendextend_options("warn", F)
})




test_that("untangle_intercourse works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   brother_1_dend1 <- as.dendrogram(hclust(dist(iris[1:10, -5]), method = "complete"))
   brother_1_dend2 <- as.dendrogram(hclust(dist(iris[10:1, -5]), method = "single"))
   
   sister_2_dend1 <- as.dendrogram(hclust(dist(iris[11:20, -5]), method = "average"))
   sister_2_dend2 <- as.dendrogram(hclust(dist(iris[20:11, -5]), method = "ward.D2"))
   
   result <- untangle_intercourse(brother_1_dend1, brother_1_dend2, sister_2_dend1, sister_2_dend2, L = 1)
   
   # entanglement reduced from 0.866 to 0.045
   initial_entanglement_dend1 <- entanglement(brother_1_dend1, brother_1_dend2)
   expect_identical(round(initial_entanglement_dend1, 3), 0.866)
   entanglement_child1 <- entanglement(result[[1]], result[[2]])
   expect_identical(round(entanglement_child1, 3), 0.045)
   
   # entanglement reduced from 0.391 to 0
   initial_entanglement_dend2 <- entanglement(sister_2_dend1, sister_2_dend2)
   expect_identical(round(initial_entanglement_dend2, 3), 0.391)
   entanglement_child2 <- entanglement(result[[3]], result[[4]])
   expect_identical(round(entanglement_child2, 3), 0)
})




test_that("entanglement_return_best_brother works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   brother_1_dend1 <- as.dendrogram(hclust(dist(iris[1:10, -5]), method = "complete"))
   brother_1_dend2 <- as.dendrogram(hclust(dist(iris[10:1, -5]), method = "single"))
   
   brother_2_dend1 <- as.dendrogram(hclust(dist(iris[11:20, -5]), method = "average"))
   brother_2_dend2 <- as.dendrogram(hclust(dist(iris[20:11, -5]), method = "ward.D2"))
   
   result <- entanglement_return_best_brother(brother_1_dend1, brother_1_dend2, brother_2_dend1, brother_2_dend2, L = 1)
   # brother_1 is more entangled, therefore we expect to get brother_2 in result 
   expect_true(entanglement(brother_1_dend1, brother_1_dend2) > entanglement(brother_2_dend1, brother_2_dend2))
   expect_identical(result, dendlist(brother_2_dend1, brother_2_dend2))
   
   # if brother_2 is more entangled
   expect_no_error(
      entanglement_return_best_brother(brother_2_dend1, brother_2_dend2, brother_1_dend1, brother_1_dend2, L = 1)
   )
})




test_that("untangle_intercourse_evolution works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   brother_1_dend1 <- as.dendrogram(hclust(dist(iris[1:10, -5]), method = "complete"))
   brother_1_dend2 <- as.dendrogram(hclust(dist(iris[10:1, -5]), method = "single"))
   
   brother_2_dend1 <- as.dendrogram(hclust(dist(iris[11:20, -5]), method = "average"))
   brother_2_dend2 <- as.dendrogram(hclust(dist(iris[20:11, -5]), method = "ward.D2"))
   
   intercourse = dendlist(brother_1_dend1, brother_1_dend2, brother_2_dend1, brother_2_dend2)
   result <- untangle_intercourse_evolution(intercourse, L = 1)
   # brother_1 is more entangled, therefore we expect to get brother_2 in result 
   expect_true(entanglement(brother_1_dend1, brother_1_dend2) > entanglement(brother_2_dend1, brother_2_dend2))
   expect_identical(result, dendlist(brother_2_dend1, brother_2_dend2))
})




test_that("untangle_evolution works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)   
   
   brother_1_dend1 <- as.dendrogram(hclust(dist(iris[1:10, -5]), method = "complete"))
   brother_1_dend2 <- as.dendrogram(hclust(dist(iris[10:1, -5]), method = "single"))
   
   sister_2_dend1 <- as.dendrogram(hclust(dist(iris[11:20, -5]), method = "average"))
   sister_2_dend2 <- as.dendrogram(hclust(dist(iris[20:11, -5]), method = "ward.D2"))
   
   # determine which set of dendrograms can be better untangled
   intercourse_result <- untangle_intercourse(brother_1_dend1, brother_1_dend2, sister_2_dend1, sister_2_dend2, L = 1)
   entanglement_child1 <- entanglement(intercourse_result[[1]], intercourse_result[[2]])
   entanglement_child2 <- entanglement(intercourse_result[[3]], intercourse_result[[4]])
   expect_true(entanglement_child1 > entanglement_child2)
   # the untangled version of the better dendrograms should be the result
   evolution_result <- untangle_evolution(brother_1_dend1, brother_1_dend2, brother_1_dend1, brother_1_dend2, L = 1)
   expect_identical(evolution_result, dendlist(intercourse_result[[1]], intercourse_result[[2]]))
})



