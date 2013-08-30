Creating beautiful trees of clusterings with R - statistically comparing trees
==============================================
author: Tal Galili
date: 2013-09-05
transition: none
transition-speed: fast
autosize: false
width: 1940
height: 1200

Boston-useR










Tanglegram plot - Iris example: first attempt
================================================================

(a sample of 30)


```r
set.seed(85945987)
ss <- sample(1:150, 30)
hc1 <- hclust(dist(iris[ss,-5]), "com")
hc2 <- hclust(dist(iris[ss,-5]), "single")
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)

dend1 <- rotate(dend1, order = c("132", "103", "130", "136", "137", "116", "87", "51", "70", 
"82", "86", "71", "95", "62", "64", "74", "147", "84", "120", 
"122", "37", "21", "18", "50", "48", "43", "9", "17", "33", "16"
))
dend2 <- rotate(dend2, order = c("132", "103", "130", "136", "137", "116", "87", "51", "70", 
"82", "95", "62", "86", "71", "64", "74", "147", "84", "120", 
"122", "37", "21", "18", "50", "48", "43", "9", "17", "33", "16"
))
                   
```








***

<img src="2013-09-05_Boston-useR_04_tree_inference-figure/iris_tanglegram_ordered.png" title="plot of chunk iris_tanglegram_ordered" alt="plot of chunk iris_tanglegram_ordered" style="display: block; margin: auto;" />






