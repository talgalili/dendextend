# require(testthat)

context("labels assignment")

test_that("labels assginment works for vectors",{
   x <- 1:3   
   expect_that(labels(x), equals(as.character(1:3))) # notice this is 1:3 as character

   labels(x) <- letters[1:3]
   expect_that(labels(x), equals(letters[1:3]))
   # labels(x) # [1] "a" "b" "c"   
   labels(x)[1] <- "one"
   expect_equal(labels(x), c("one", "b", "c")) # checking specific assignment   
})



test_that("labels assginment works for matrix",{
   y <- matrix(1:9, 3,3)
   expect_that(labels(y), equals(NULL))
   
   labels(y) <- letters[1:3] # defaults to assign labels to columns
   expect_that(labels(y), equals(letters[1:3]))
   expect_that(colnames(y), equals(letters[1:3]))
   
   labels(y, which = "rownames") <- letters[24:26]
   expect_that(labels(y, which = "rownames"), equals(letters[24:26]))
   expect_that(rownames(y), equals(letters[24:26]))   

   labels(y)[1] <- "one"
   expect_equal(labels(y), c("one", "b", "c")) # checking specific assignment      
})

test_that("labels (with order=TRUE, by default), before and after assginment, works for hclust",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
    
   #    plot(hc)   
#    expect_that(labels(hc), equals(c("Alabama", "Alaska", "Arizona")))
   expect_that(labels(hc), equals(c("Arizona", "Alabama", "Alaska")))
   
   labels(hc)  <- letters[1:3]
   expect_that(labels(hc), equals(letters[1:3]))   
   
   labels(hc)[1] <- "one"
   expect_equal(labels(hc), c("one", "b", "c")) # checking specific assignment         
})


test_that("labels (without order!) works for hclust",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   #    plot(hc)   
   expect_that(labels(hc, order = FALSE), equals(c("Alabama", "Alaska", "Arizona")))
})


test_that("labels (without order!) works differently than labels assignment (which are WITH order) for hclust",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   #    plot(hc)   
   expect_that(labels(hc, order = FALSE), equals(c("Alabama", "Alaska", "Arizona")))
   
   labels(hc)  <- letters[1:3]
   expect_that(identical(labels(hc, order = FALSE) , letters[1:3]),
               is_false())   
})


test_that("labels assginment works for dendrogram",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   
   expect_that(labels(dend), equals(c("Arizona", "Alabama", "Alaska")))
   
   labels(dend)  <- letters[1:3]
   expect_that(labels(dend), equals(letters[1:3]))   
   
   labels(dend)[1] <- "one"
   expect_equal(labels(dend), c("one", "b", "c")) # checking specific assignment            
})


test_that("labels for hclust and dendrogram are (by default) the same",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)      
   # hc and dend labels should NOT be identical.   
   expect_that(identical(labels(dend), labels(hc)), is_true())
})




test_that("labels assginment recycles properly and consistently",{
   x <- 1:3   
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   y <- matrix(1:9, 3,3)
   
   suppressWarnings({
   labels(x) <- letters[1]
   labels(hc)  <- letters[1]
   labels(dend)  <- letters[1]
   labels(y) <- letters[1] # defaults to assign labels to columns
   labels(y, which = "rownames") <- letters[24]
   })
   
   expect_that(labels(x), equals(rep(letters[1], 3)))
   expect_that(labels(hc), equals(rep(letters[1], 3)))
   expect_that(labels(dend), equals(rep(letters[1], 3)))
   expect_that(labels(y), equals(rep(letters[1], 3)))
   expect_that(labels(y, which = "rownames"), equals(rep(letters[24], 3)))
   # labels(x) # [1] "a" "b" "c"      
})


test_that("labels assginment issues warning when using recycling",{
   x <- 1:3   
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   y <- matrix(1:9, 3,3)   
   
#    expect_that(labels(x) <- letters[1], gives_warning())
   expect_warning(labels(x) <- letters[1])
   expect_warning(labels(hc) <- letters[1])
   expect_warning(labels(dend) <- letters[1])
   expect_warning(labels(y) <- letters[1])
   expect_warning(labels(y, which = "rownames") <- letters[24])
})


test_that("labels assginment to dendrogram keeps the child nodes as NOT of dendrogram class",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)
   labels(dend)  <- letters[1:3]
   
   expect_true(class(unclass(dend)[[2]]) == "list")
})



test_that("order of leaves in sub-dendrogram and as.hclust",{
   # For example:
   hc <- hclust(dist(USArrests[1:6,]), "ave")
   dend <- as.dendrogram(hc)
   sub_dend <- dend[[1]]
   
   # bad order
   expect_equal(order.dendrogram(sub_dend), c(4,6))
   # bad labels:
   expect_equal(labels(as.hclust(sub_dend)), as.character(rep(NA,2)))
   
   # let's fix it:   
   order.dendrogram(sub_dend) <- rank(order.dendrogram(sub_dend), ties.method= "first")
   expect_equal(labels(as.hclust(sub_dend)), c("Arkansas", "Colorado"))
   # We now have labels :)
   
   
})



test_that("labels of hclust on data without rownames is an integer",{
   # this is a problem when some function assumes that labels are "character"
   DATA <- USArrests
   rownames(DATA) <- NULL
   hc <- hclust(dist(DATA))
   dend <- as.dendrogram(hc)
   expect_true(length(labels(hc))==0) # hc has NO labels
   expect_true(is.integer(labels(dend))) # the labels the dend gets are Integer   
})
