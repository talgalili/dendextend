# require(testthat)
suppressPackageStartupMessages(library(dendextend))

test_that("dendlist works",{

   dend <- iris[,-5] %>% dist %>% hclust %>% as.dendrogram
   dend2 <- iris[,-5] %>% dist %>% hclust(method = "single") %>% as.dendrogram
   
   expect_error(dendlist(1:4, 5, a=dend))

   expect_equal(dendlist(dend) %>% class, "dendlist")
   
   # dendlist <- function (...) list(...)
   # check lengths
   expect_equal(dendlist(dend) %>% length, 1L)
   expect_equal(dendlist(dend, dend) %>% length, 2L)
   expect_equal(dendlist(dend, dend, dendlist(dend)) %>% length, 3L)
   
   #  Checking that the piping works
   expect_identical(dendlist(dend, dend2),
                  dendlist(dend) %>% dendlist(dend2))
})

