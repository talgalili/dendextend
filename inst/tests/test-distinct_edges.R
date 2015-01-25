# library(testthat)


context("Distinct edges")


test_that("partition_leaves works",{
#    library(magrittr)

   x <- 1:3 %>% dist %>% hclust %>% as.dendrogram
#    partition_leaves(x)
#    plot(x)
   expect_identical(partition_leaves(x), list(c(3L, 1L, 2L), 3L, 1:2, 1L, 2L))
   
})



test_that("distinct_edges works",{

   x <- 1:5 %>% dist %>% hclust %>% as.dendrogram
   y <- set(x, "labels", 5:1)
   expect_identical(distinct_edges(x, y), c(5L,7L))
   expect_identical(distinct_edges(y, x), c(2L,5L))
})



test_that("dist.dendlist work",{
   x <- 1:5 %>% dist %>% hclust %>% as.dendrogram
   y <- set(x, "labels", 5:1)
   
#    dput(dist.dendlist(dendlist(x,x,y)))
#    dend_diff(x,y)
   
   expect_identical(dist.dendlist(dendlist(x,x,y)),                 
                 structure(c(0, 4, 4), Size = 3L, call = as.dist.default(m = the_dist), class = "dist", Diag = FALSE, Upper = FALSE)
            )   
})
