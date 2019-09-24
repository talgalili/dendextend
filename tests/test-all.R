library(testthat)

library(dendextend)
test_check("dendextend")

# library(dendextendRcpp)
# test_check("dendextend")



#
# library(dendextend)
# test_package("dendextend")
#
# library(dendextendRcpp)
# test_package("dendextend")
#
#
# library(dendextend)
# tryCatch(test_package("dendextend"),
#          error = function(e)
#             cat('Please run: testthat::test_dir("tests\\testthat")')
#          )
#
# #
#
# library(dendextendRcpp)
# tryCatch(test_package("dendextend"),
#          error = function(e)
#             cat('Please run: testthat::test_dir("tests\\testthat")')
# )
#
#
# testthat::test_dir("tests\\testthat")

# library(dendextend)
# system.time(test_dir("inst\\tests")) # 12.8  (- 21)
# library(dendextendRcpp)
# system.time(test_dir("inst\\tests")) # 9.36
# search()


# dendextend_options("labels.dendrogram")
# dendextend:::labels.dendrogram

# library(compiler)
# enableJIT(0)
