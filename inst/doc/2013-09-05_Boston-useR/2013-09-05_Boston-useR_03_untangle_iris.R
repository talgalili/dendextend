
## ----setup, include=FALSE, echo=FALSE------------------------------------
require(knitr)
opts_chunk$set(cache=TRUE, fig.align='center')
options(digits = 3)
suppressMessages(require(dendextend))
suppressMessages(require(dendextendRcpp))


## ------------------------------------------------------------------------
set.seed(85945987)
ss <- sample(1:150, 30)
hc1 <- hclust(dist(iris[ss,-5]), "com")
hc2 <- hclust(dist(iris[ss,-5]), "single")
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)


## ----first_tanglegram_with_code, echo=TRUE, fig.height = 20, fig.width=18, eval=FALSE----
require(colorspace)
lines_col_true <- rainbow_hcl(3)[sort_levels_values(
      as.numeric(iris[ss,5])[order.dendrogram(dend1)]
   )]
dend12 <- tanglegram(dend1 , dend2, 
           cex_main=3,
           cex_main_left=5,
           cex_main_right=5,
           color_lines=lines_col_true,
           main_left= "hclust complete", main_right="hclust single", main = "(true clusters)",
           lab.cex = .5, edge.lwd = 6, margin_inner= 3, 
           type = "r", center = TRUE, k_branches = 3, axes = FALSE, 
   sub = paste("Entanglement:", 
               round(entanglement(dend1 , dend2),3)),
           )


## ----first_tanglegram_with_code, echo=FALSE, fig.height = 20, fig.width=18----
require(colorspace)
lines_col_true <- rainbow_hcl(3)[sort_levels_values(
      as.numeric(iris[ss,5])[order.dendrogram(dend1)]
   )]
dend12 <- tanglegram(dend1 , dend2, 
           cex_main=3,
           cex_main_left=5,
           cex_main_right=5,
           color_lines=lines_col_true,
           main_left= "hclust complete", main_right="hclust single", main = "(true clusters)",
           lab.cex = .5, edge.lwd = 6, margin_inner= 3, 
           type = "r", center = TRUE, k_branches = 3, axes = FALSE, 
   sub = paste("Entanglement:", 
               round(entanglement(dend1 , dend2),3)),
           )


## ------------------------------------------------------------------------
set.seed(2355)
dend12_rotate <- untangle_random_search(
   dend12[[1]],
   dend12[[2]], 
   R= 100  )
dend12 <- dend12_rotate


## ----first_tanglegram_with_random_untangle, echo=TRUE, fig.height = 20, fig.width=18, eval=FALSE----
lines_col_true <- rev(rainbow_hcl(3))[sort_levels_values(
      as.numeric(iris[ss,5])
      [order.dendrogram(dend12[[1]])]
   )]
dend12 <- tanglegram(dend12[[1]] , dend12[[2]], 
           cex_main=3,
           cex_main_left=5,
           cex_main_right=5,
           color_lines=lines_col_true,
           main_left= "hclust complete", main_right="hclust single", main = "(true clusters)",axes = FALSE,
           lab.cex = .5, edge.lwd = 6, margin_inner= 3, 
           type = "r", center = TRUE,
   sub = paste("Entanglement:", 
               round(entanglement(dend12[[1]] , dend12[[2]]),3)),
           )


## ----first_tanglegram_with_random_untangle, echo=FALSE, fig.height = 20, fig.width=18----
lines_col_true <- rev(rainbow_hcl(3))[sort_levels_values(
      as.numeric(iris[ss,5])
      [order.dendrogram(dend12[[1]])]
   )]
dend12 <- tanglegram(dend12[[1]] , dend12[[2]], 
           cex_main=3,
           cex_main_left=5,
           cex_main_right=5,
           color_lines=lines_col_true,
           main_left= "hclust complete", main_right="hclust single", main = "(true clusters)",axes = FALSE,
           lab.cex = .5, edge.lwd = 6, margin_inner= 3, 
           type = "r", center = TRUE,
   sub = paste("Entanglement:", 
               round(entanglement(dend12[[1]] , dend12[[2]]),3)),
           )


## ------------------------------------------------------------------------
# This can be slow!
dend12_rotate <- untangle_step_rotate_2side(
   dend12[[1]],
   dend12[[2]] )
dend12 <- dend12_rotate


## ----first_tanglegram_with_2side_untangle, echo=TRUE, fig.height = 20, fig.width=18, eval=FALSE----
lines_col_true <- rev(rainbow_hcl(3))[sort_levels_values(
      as.numeric(iris[ss,5])
      [order.dendrogram(dend12[[1]])]
   )]
dend12 <- tanglegram(dend12[[1]] , dend12[[2]], axes = FALSE,
           cex_main=3,
           cex_main_left=5,
           cex_main_right=5,
           color_lines=lines_col_true,
           main_left= "hclust complete", main_right="hclust single", main = "(true clusters)",
           lab.cex = .5, edge.lwd = 6, margin_inner= 3, 
           type = "r", center = TRUE,                     
   sub = paste("Entanglement:", 
               round(entanglement(dend12[[1]] , dend12[[2]]),3)),
           )


## ----first_tanglegram_with_2side_untangle, echo=FALSE, fig.height = 20, fig.width=18----
lines_col_true <- rev(rainbow_hcl(3))[sort_levels_values(
      as.numeric(iris[ss,5])
      [order.dendrogram(dend12[[1]])]
   )]
dend12 <- tanglegram(dend12[[1]] , dend12[[2]], axes = FALSE,
           cex_main=3,
           cex_main_left=5,
           cex_main_right=5,
           color_lines=lines_col_true,
           main_left= "hclust complete", main_right="hclust single", main = "(true clusters)",
           lab.cex = .5, edge.lwd = 6, margin_inner= 3, 
           type = "r", center = TRUE,                     
   sub = paste("Entanglement:", 
               round(entanglement(dend12[[1]] , dend12[[2]]),3)),
           )


## ----first_tanglegram_with_2side_untangle_hang, echo=TRUE, fig.height = 20, fig.width=18, eval=FALSE----
lines_col_true <- rev(rainbow_hcl(3))[sort_levels_values(
      as.numeric(iris[ss,5])
      [order.dendrogram(dend12[[1]])]
   )]
dend12 <- tanglegram(dend12[[1]] , dend12[[2]], 
           cex_main=3,
           cex_main_left=5,
           cex_main_right=5,
           color_lines=lines_col_true,
           main_left= "hclust complete", main_right="hclust single", main = "(true clusters)",axes = FALSE,
           lab.cex = .5, edge.lwd = 6, margin_inner= 3, 
           type = "t", center = TRUE,                     
   sub = paste("Entanglement:", 
               round(entanglement(dend12[[1]] , dend12[[2]]),3)),hang=TRUE,
           )


## ----first_tanglegram_with_2side_untangle_hang, echo=FALSE, fig.height = 20, fig.width=18----
lines_col_true <- rev(rainbow_hcl(3))[sort_levels_values(
      as.numeric(iris[ss,5])
      [order.dendrogram(dend12[[1]])]
   )]
dend12 <- tanglegram(dend12[[1]] , dend12[[2]], 
           cex_main=3,
           cex_main_left=5,
           cex_main_right=5,
           color_lines=lines_col_true,
           main_left= "hclust complete", main_right="hclust single", main = "(true clusters)",axes = FALSE,
           lab.cex = .5, edge.lwd = 6, margin_inner= 3, 
           type = "t", center = TRUE,                     
   sub = paste("Entanglement:", 
               round(entanglement(dend12[[1]] , dend12[[2]]),3)),hang=TRUE,
           )


