# library(testthat)
# library(dendextend)


context("rect.dendrogram")


testthat("rect.dendrogram works", {
   
   hc <- USArrests[1:10, ] %>%
      dist() %>%
      hclust() 
   dend <- as.dendrogram(hc)
   
   # standard
   plot(dend)
   rect.dendrogram(dend, k = 4, which = 3)
   rect.dendrogram(dend, k = 4, x = 4)
   
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
   
   207 - 209
})