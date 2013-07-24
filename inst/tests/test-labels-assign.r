context("labels assignment")

test_that("labels assginment works for vectors",{
   x <- 1:3   
   expect_that(labels(x), equals(as.character(1:3)))

   labels(x) <- letters[1:3]
   expect_that(labels(x), equals(letters[1:3]))
   # labels(x) # [1] "a" "b" "c"   
})



test_that("labels assginment works for matrix",{
   x <- matrix(1:9, 3,3)
   expect_that(labels(x), equals(NULL))
   
   labels(x) <- letters[1:3] # defaults to assign labels to columns
   expect_that(labels(x), equals(letters[1:3]))
   expect_that(colnames(x), equals(letters[1:3]))
   
   labels(x, which = "rownames") <- letters[24:26]
   expect_that(labels(x, which = "rownames"), equals(letters[24:26]))
   expect_that(rownames(x), equals(letters[24:26]))
   
})

test_that("labels (with order=TRUE, by default), before and after assginment, works for hclust",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
    
   #    plot(hc)   
#    expect_that(labels(hc), equals(c("Alabama", "Alaska", "Arizona")))
   expect_that(labels(hc), equals(c("Arizona", "Alabama", "Alaska")))
   
   labels(hc)  <- letters[1:3]
   expect_that(labels(hc), equals(letters[1:3]))   
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
})


test_that("labels for hclust and dendrogram are (by default) the same",{
   hc <- hclust(dist(USArrests[1:3,]), "ave")
   dend <- as.dendrogram(hc)      
   # hc and dend labels should NOT be identical.   
   expect_that(identical(labels(dend), labels(hc)), is_true())
})

