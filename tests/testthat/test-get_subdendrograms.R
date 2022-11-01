# library(testthat)
context("get_subdendrograms when labels are integers")

test_that("get_subdendrograms works", {
  dend <- iris[1:10, -5] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram() %>%
    set("labels_to_character") %>%
    color_branches(k = 5)

  # plot(dend)
  
  dend_list <- get_subdendrograms(dend, 5)
  
  expect_length(dend_list, 5L)
  expect_s3_class(dend_list, "dendlist")
  
  cluster_labels <- lapply(dend_list, labels)
  # dput(cluster_labels)
  
  expected_cluster_labels <- list("9", c("2", "10"), 
                                  c("7", "3", "4"), 
                                  "6", 
                                  c("8", "1", "5"))
  
  expect_equal(cluster_labels, expected_cluster_labels)
  
  # Do less-rigorous hash-based test for other k        
  # expect_known_hash(get_subdendrograms(dend, 1), "61885f34b7")
  # expect_known_hash(get_subdendrograms(dend, 2), "8cdbb002df")
  # expect_known_hash(get_subdendrograms(dend, 10), "9340d1e1dc")
  expect_error(get_subdendrograms(dend, 0),
               "elements of 'k' must be between")
  expect_error(get_subdendrograms(dend, 11),
               "elements of 'k' must be between")
})




context("get_subdendrograms when labels aren't integers")

test_that("get_subdendrograms works", {
  dend <- iris[1:10, -5] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram() %>%
    set("labels_to_character") %>%
    color_branches(k = 5) %>% 
    dendextend::set_labels(paste0("a",labels(.) ))
  
  # plot(dend)
  
  dend_list <- get_subdendrograms(dend, 5)
  
  expect_length(dend_list, 5L)
  expect_s3_class(dend_list, "dendlist")
  
  cluster_labels <- lapply(dend_list, labels)
  # dput(cluster_labels)
  expected_cluster_labels <- list("a9", c("a2", "a10"),
                                  c("a7", "a3", "a4"), "a6", 
                                  c("a8", "a1", "a5"))
  
  expect_equal(cluster_labels, expected_cluster_labels)
  
  # Do less-rigorous hash-based test for other k        
  # expect_known_hash(get_subdendrograms(dend, 1), "b45bf4d9f9")
  # expect_known_hash(get_subdendrograms(dend, 2), "362f3a1588")
  # expect_known_hash(get_subdendrograms(dend, 10), "2187dd5232")
  expect_error(get_subdendrograms(dend, 0),
               "elements of 'k' must be between")
  expect_error(get_subdendrograms(dend, 11),
               "elements of 'k' must be between")
})



context("collapse_labels")

test_that("collapse_labels works", {
   set.seed(23235)
   ss <- sample(1:150, 5)
   
   # Getting the dend object
   dend25 <- iris[ss, -5] %>%
      dist() %>%
      hclust() %>%
      as.dendrogram() %>%
      set("labels", letters[1:5])
   
   dend2 <- collapse_labels(dend25, c("d", "e"))
   dend3 <- collapse_labels(dend25, c("c", "d", "e"))

   # dput(labels(dend2))
   # dput(labels(dend3))
   expect_equal(labels(dend2), c("a", "b", "c", "d_e"))
   expect_equal(labels(dend3), c("a", "b", "c_d_e"))
  
})
