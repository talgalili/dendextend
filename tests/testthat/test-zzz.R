# cat("\n")
# library(testthat)


context("Loading dendextend package")



test_that(".onLoad works", {
   # no dendextend options yet 
   .onLoad("dendextend", "dendextend")  
   expect_null(getOption("dendextend"))  
})



test_that(".onUnload works", {
   
   # dendextend::tanglegram should no longer be available after Namespace is unloaded.
   # HOWEVER, unloadNamespace is unimplemented, so dendextend::tanglegram persists
   loadNamespace("dendextend")
   expect_true(typeof(dendextend::tanglegram) == "closure")
   unloadNamespace("dendextend")
   expect_true(typeof(dendextend::tanglegram) == "closure")
   
   # tanglegram should no longer be available after detaching package
   # HOWEVER, can't detach package in tests
   library(dendextend)
   expect_true(typeof(tanglegram) == "closure")
   detach("package:dendextend", unload = TRUE, character.only = TRUE)
   expect_true(typeof(tanglegram) == "closure")
   
})

