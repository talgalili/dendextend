# library(testthat)
# library(dendextend)


context("rect.dendrogram")


test_that("rect.dendrogram works", {
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   
   # standard
   plot(dend)
   rect.dendrogram(dend, k = 4, which = 3)
   rect.dendrogram(dend, k = 4, x = 4)
   rect.dendrogram(dend, h = 70, x = 1)
   
   # horizontal
   plot(dend, horiz = T)
   rect.dendrogram(dend, k = 4, which = 3, horiz = T, text = "test")
   
   # if non-denrogram object passed in
   expect_error(
      rect.dendrogram(hc, k = 4)   
   )
   # if more than one k value passed in
   expect_error(
      rect.dendrogram(dend, k = c(2,3))   
   )
   # if both k and h specified
   expect_error(
      rect.dendrogram(dend, k = 2, h = 2)   
   )
   # if neither k nor h are specified
   expect_error(
      rect.dendrogram(dend)   
   )
   # if k == 1
   expect_error(
      rect.dendrogram(dend, k = 1, stop_if_out = T)   
   )
   expect_warning(
      rect.dendrogram(dend, k = 1)   
   )
   # if both x and which are specified
   expect_error(
      rect.dendrogram(dend, k = 4, x = 2, which = 3)   
   )
   # if which > k
   expect_error(
      rect.dendrogram(dend, k = 4, which = 5)   
   )
   # if k >= number of leaves
   expect_warning(
      rect.dendrogram(dend, k = 10)
   )
   
})


test_that("identify.dendrogram works", {
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   
   # replace locator function with one that automatically clicks to make test non-interactive
   locator_counter <<- 0
   with_mock(
      locator = function(x) {
         locator_counter <<- locator_counter + 1
         if (locator_counter == 1) return(list(x = 172, y = 5))
         if (locator_counter == 2) return(list(x = 172, y = 1))
         return(NULL)
      },
      {
         plot(dend, horiz = TRUE)
         vec <- identify(dend, horiz = TRUE, FUN = function(x) x + 1, DEV.FUN = 2)
      }
   )
   expect_identical(
      vec[[2]], c("Connecticut" = 8)
   )
   expect_true(
      all(vec[[1]] == c(2:7, 9:11))
   )

})