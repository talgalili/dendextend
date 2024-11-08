# library(testthat)
# library(dendextend)


context("sample.dendrogram")


test_that("duplicate_leaf works", {

   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   
   duplicated_dend <- duplicate_leaf(dend, "Florida", 2)
   expect_equal(
      labels(duplicated_dend)[c(2,5)],
      c("Florida", "Florida_2")
   )
   
   # dend is the same if leaf duplicated once
   duplicated_dend <- duplicate_leaf(dend, "Florida", 1)
   expect_identical(
      dend, duplicated_dend
   )
   # if leaf is passed in
   duplicated_leaf <- duplicate_leaf(dend[[1]], "Connecticut", 2)
   expect_equal(
      labels(duplicated_leaf), 
      c(Connecticut_1 = "Connecticut", Connecticut_2 = "Connecticut")
   )
   
   # if multiple labels used
   expect_warning(
      duplicate_leaf(dend, c("Florida", "Arizona"), 2)   
   )
   # if label not in dendrogram
   expect_warning(
      duplicate_leaf(dend, "fake label", 2)
   )
   
})


test_that("sample.dendrogram works", {
   suppressWarnings(RNGversion("3.5.0"))
   set.seed(1)
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   
   sampled_dend <- sample.dendrogram(dend, replace = TRUE)
   expect_equal(
      labels(sampled_dend),
      c("Connecticut", "Arizona", "California", "Arizona_2", "Colorado", 
        "Georgia", "Georgia_2", "Alabama", "Delaware", "Delaware_2")
   )
   
   sampled_dend <- sample.dendrogram(dend, replace = FALSE)
   expect_equal(
      labels(sampled_dend),
      c("Arizona", "Florida", "Colorado", "Delaware", "Arkansas", 
        "Georgia", "Alaska", "California", "Connecticut", "Alabama")
   )
   
})
