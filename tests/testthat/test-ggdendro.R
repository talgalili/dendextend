# library(testthat)
# library(dendextend)


context("ggdendro")


options("verbose" = T)
test_that("dendrogram_data works", {
   
   dend <- as.dendrogram(hclust(dist(1:5)))
   plot.new()
   
   # if a leaf is passed in
   expect_error(
      dendrogram_data(dend[[1]][[1]])
   )
   
   #
   res <- dendrogram_data(dend, "triangle")
   
   # if dend with edgetext is passed in
   attr(dend, "edgetext") <- "test 1"
   attr(dend[[1]][[1]], "edgetext") <- "test 2"
   res <- dendrogram_data(dend)
   
   # if center = T
   trace("dendrogram_data", quote(center <- TRUE), at = 6)
   res <- dendrogram_data(dend)
   untrace("dendrogram_data")
   
   # if nodePar = (pch = 1)
   trace("dendrogram_data", quote(nodePar <- c(pch = 1)), at = 15)
   res <- dendrogram_data(dend)
   
   # if nodePar = 1
   trace("dendrogram_data", quote(nodePar <- 1), at = 15)
   res <- dendrogram_data(dend)
   
   # if a height is NULL
   attr(dend[[1]][[1]], "height") <- NULL
   attr(dend[[1]][[1]], "pch") <- 1
   res <- dendrogram_data(dend)
   
   # if a leaf has a null label
   attr(dend[[1]][[1]], "label") <- NULL
   res <- dendrogram_data(dend)
   

   # TODO: 115 - 117, 174 - 190 HAVE to figure out nested trace
   expect_true(TRUE)
})
options("verbose" = F)



# trace("gg.plotNode", quote({
#    nodePar <- 1
#    horiz <- TRUE
# }), at = 4, , where = dendrogram_data) 
# res <- dendrogram_data(dend)
# untrace("dendrogram_data")
# 
# 
# 
# 
# # Define an outer function with an inner function
# outer_function <- function(data) {
#    inner_function <- function(x) {
#       # Some code
#       return(x + 1)
#    }
#    a = 1
#    result <- inner_function(data)
#    return(result)
# }
# 
# # Apply a trace to the inner function within the outer function's environment
# trace("outer_function:inner_function", where = outer_function, quote({
#    print("Tracing inner function")
# }))
# 
# 
# 
# test <- get("a", envir = environment(outer_function))
