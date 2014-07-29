# library(testthat)
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

   # the dendlist of a dendlist should be a dendlist!
   expect_identical(
      dendlist(dend), dendlist(dendlist(dend))
   )
   expect_identical(
      dendlist(dend, dend), dendlist(dendlist(dend, dend))
   )

   # dendlist does not handle weird stuff.
   expect_error(
      dendlist(unclass(dendlist(dend, dend)))
   )
   # but it handles empty lists just fine:
   expect_warning(
      dendlist()
   )
   expect_warning(
      dendlist(list())
   )
   # It can merge a dendlist of an empty list with a dend just fine:
   expect_warning(
      dendlist(dendlist(list()), dend)
   )
   # and the result is of length 1! (as we wanted it to)
   expect_equal(
      suppressWarnings(length(dendlist(dendlist(list()), dend))),
      1
   )
   

   # checks is.dendlist
   expect_true(suppressWarnings(is.dendlist(dendlist())))
   expect_true(is.dendlist(dendlist(dend)))
   
   
   
   
})

