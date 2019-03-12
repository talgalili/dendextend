##### extract au #####
# hclust object descriptions : https://stat.ethz.ch/R-manual/R-patched/library/stats/html/hclust.html

pvclust.edges <- function(pvclust.obj){
  hclust_merge <- pvclust.obj$hclust$merge
  #hclust_merge <- apply(hclust_merge, c(1,2), function(x) if(x < 0) pvclust.obj$hclust$labels[abs(x)] else x)
  hclust_merge[hclust_merge < 0] <- pvclust.obj$hclust$labels[abs(hclust_merge[hclust_merge < 0])] # get sample name
  hclust_merge <- cbind(hclust_merge, pvclust.obj$edges) # combine with edge table
  colnames(hclust_merge)[1:2] <- c("branch_L","branch_R")
  return(hclust_merge)
}