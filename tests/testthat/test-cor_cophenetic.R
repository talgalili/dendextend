# library(testthat)


context("Cophenetic correlation between two trees")


test_that("cor_cophenetic works", {
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(23235)
  ss <- sample(1:150, 10)
  hc1 <- iris[ss, -5] %>%
    dist() %>%
    hclust("com")
  hc2 <- iris[ss, -5] %>%
    dist() %>%
    hclust("single")
  dend1 <- as.dendrogram(hc1)
  dend2 <- as.dendrogram(hc2)
  #    cutree(dend1)

  #    cophenetic(hc1)
  #    cophenetic(hc2)
  #    # notice how the dist matrix for the dendrograms have different orders:
  #    cophenetic(dend1)
  #    cophenetic(dend2)

  # just taking the cor of two cophenetic treess
  # will NOT give the same result for hclust and dendrogram
  # (due to the different order of matrices)
  expect_false(
    cor(cophenetic(hc1), cophenetic(hc2)) == # 0.874
      cor(cophenetic(dend1), cophenetic(dend2)) # 0.16
  )
  # the difference is becasue the order of the distance table in the case of
  # stats:::cophenetic.dendrogram will change between dendrograms!

  # however, the following numbers are consistant (since I force-sort the rows/columns):
  expect_equal(
    cor_cophenetic(hc1, hc2),
    cor_cophenetic(dend1, dend2)
  )

  # Let's see that dendlist works
  expect_equal(
    cor_cophenetic(dendlist(dend1, dend2)),
    cor_cophenetic(dend1, dend2)
  )
})
