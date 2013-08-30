
## ----setup, include=FALSE, echo=FALSE------------------------------------
require(knitr)
opts_chunk$set(cache=TRUE, fig.align='center')
options(digits = 3)
purl("2013-09-05_Boston-useR_02_dendextend.Rpres")


## ----, echo = FALSE------------------------------------------------------
cat(packageDescription("dendextend")$Author)
cat("Yoav Benjamini")


## ----, echo=FALSE, fig.width=15, fig.height=10, results='asis'-----------
library(dendextend)
cat("dendextend has", length(ls("package:dendextend")), "functions\n") # lsf.str

require(mvbutils)
foodweb_dend <- foodweb( where="package:dendextend",plotting=FALSE)
set.seed(24262)
require(igraph)
graph_dend=graph.adjacency(foodweb_dend$funmat)
par(cex = 1.15)
plot(graph_dend, layout=layout.kamada.kawai, vertex.size = 0)


## ------------------------------------------------------------------------
data(precip)
precip_data <- 
   round(precip[c("Boston", "New York", "Nashville", "Miami" , "Washington")])
DIST <- dist(precip_data, diag=TRUE)
# create an heirarchical clustering object
hc <- hclust(DIST, "single") 

dend <- rev(as.dendrogram(hc))
t(t(sort(precip_data)))


## ----, fig.height=10, fig.width=10---------------------------------------
par(cex = 2, lwd = 2) 
# notice how it effects the plot!
plot(dend, las = 2)


## ------------------------------------------------------------------------
require(dendextend)
nleaves(dend)
nnodes(dend)
get_nodes_attr(dend, "height")
get_nodes_attr(dend, "members")
labels_colors(dend)


## ----, fig.height=10, fig.width=10---------------------------------------
par(cex = 2, lwd = 2, las = 2) 
# notice how it effects the plot!
plot(dend)


## ----, warning=FALSE-----------------------------------------------------
require(colorspace)
dend2 <- dend
labels(dend2) <- abbreviate(labels(dend2),5)
labels_colors(dend2) <- rainbow_hcl(nleaves(dend2))


## ----, fig.height=10, fig.width=10---------------------------------------
par(cex = 2, lwd = 2, las = 2) 
plot(dend2)   


## ----, fig.height=10, fig.width=10---------------------------------------
par(cex = 2, lwd = 2, las = 2) 
plot(dend)   


## ----, warning=FALSE-----------------------------------------------------
require(colorspace)
dend2 <- dend
labels(dend2) <- abbreviate(labels(dend2),5)
labels_colors(dend2) <- rainbow_hcl(nleaves(dend2))


## ----, fig.height=10, fig.width=10---------------------------------------
par(cex = 2, lwd = 2, las = 2) 
plot(dend2)   


## ----, fig.height=10, fig.width=10---------------------------------------
dend3 <- color_branches(dend2, k=3)
par(cex = 2, lwd = 2, las = 2) 
plot(dend3)   
abline(h=3, col = 3, lty = 2)


## ----, warning=FALSE-----------------------------------------------------

data.frame(
node_label=get_nodes_attr(dend, "label"),
edgePar_col=unlist(get_nodes_attr(dend, "edgePar")),
label_col=unlist(get_nodes_attr(dend, "nodePar")) )
# NA's are for the root and the left branch


## ----, fig.height=10, fig.width=10---------------------------------------
par(cex = 2, lwd = 2, las = 2) 
plot(dend3)   

abline(h=3, col = 3, lty = 2)


## ----dend_hang_tree_1, fig.height=10, fig.width=10-----------------------
dend4 <- hang.dendrogram(dend3)

par(cex = 2, lwd = 2, las = 2)
plot(dend4)   


## ----, fig.height=10, fig.width=10---------------------------------------
par(cex = 2, lwd = 2, las = 2) 
plot(dend3)   

abline(h=3, col = 3, lty = 2)


## ----dend_hang_tree_1, fig.height=10, fig.width=10-----------------------
dend4 <- hang.dendrogram(dend3)

par(cex = 2, lwd = 2, las = 2)
plot(dend4)   


## ----dend_rotating_example_1, fig.height=10, fig.width=10----------------
dend5 <- rotate(dend4, c(4, 1:3, 5))
par(cex = 2, lwd = 2, las = 2)
plot(dend5)   


## ----dend_unbranching_example_1, fig.height=10, fig.width=10-------------
dend6 <- unbranch(dend5)

par(cex = 2, lwd = 2, las = 2) 
plot(dend6) 


## ------------------------------------------------------------------------
# as.hclust(dend) is not longer possible...


## ----dend_rotating_example_1, fig.height=10, fig.width=10----------------
dend5 <- rotate(dend4, c(4, 1:3, 5))
par(cex = 2, lwd = 2, las = 2)
plot(dend5)   


## ----dend_unbranching_example_1, fig.height=10, fig.width=10-------------
dend6 <- unbranch(dend5)

par(cex = 2, lwd = 2, las = 2) 
plot(dend6) 


## ----dend_pruning_example_1, fig.height=10, fig.width=10----------------
dend7 <- prune(dend6, c("Wshng", "NwYrk")) 

par(cex = 2, lwd = 2, las = 2) 
plot(dend7) 


## ------------------------------------------------------------------------
cutree(dend7 , k=3)
cutree(dend7 , k=3,order_clusters_as_data=FALSE)
args(cutree.dendrogram)


## ----dend_pruning_example_1, fig.height=10, fig.width=10----------------
dend7 <- prune(dend6, c("Wshng", "NwYrk")) 

par(cex = 2, lwd = 2, las = 2) 
plot(dend7) 


## ----,  fig.height = 13, fig.width=13,echo=FALSE-------------------------
data(iris)
require(colorspace)

species_col <- rev(rainbow_hcl(3))[as.numeric(iris[,5])]
# species_col[c(107,71)] <- "red" # highlight the two extreme cases we mentioned
# species_pch <- rep(1, length(species_col))
# species_pch[c(107,71)] <- 19 # highlight the two extreme cases we mentioned
species_col[c(107,71)] <- "black"

pairs(iris[,-5],cex.labels=4,
   col = species_col, pch=19,
      cex = 3.5)



## ----iris_colored_branches, fig.height = 15, fig.width=10, echo=FALSE----
par(cex = 1.5)

require(colorspace)

data(iris) 
d_iris <- dist(iris[,-5]) # method="man" # is a bit better
hc_iris <- hclust(d_iris, method = "complete")
# labels(hc_iris) # no labels, because "iris" has no row names
dend_iris <- as.dendrogram(hc_iris)
# is.integer(labels(dend_iris)) # this could cause problems...

iris_species <- rev(levels(iris[,5]))
dend_iris <- color_branches(dend_iris,k=3, groupLabels=iris_species)
# is.character(labels(dend_iris)) # labels are no longer "integer"

# have the labels match the real classification of the flowers:
labels_colors(dend_iris) <-
   rainbow_hcl(3)[sort_levels_values(
      as.numeric(iris[,5])[order.dendrogram(dend_iris)]
   )]

# We'll add the flower type
labels(dend_iris) <- paste(as.character(iris[,5])[order.dendrogram(dend_iris)],
                           "(",labels(dend_iris),")", 
                           sep = "")

dend_iris <- hang.dendrogram(dend_iris,hang_height=0.1)

# reduce the size of the labels:
dend_iris <- assign_values_to_leaves_nodePar(dend_iris, 0.5, "lab.cex")

dend_pch <- rep(NA, 150)
dend_pch[c(107,71)] <- 19
dend_pch <- dend_pch[order.dendrogram(dend_iris)]
dend_iris <- assign_values_to_leaves_nodePar(dend_iris, dend_pch, "pch")

par(mar = c(3,3,3,7))
plot(dend_iris, 
     main = "Clustered Iris dataset ('complete')
     (the labels give the true flower species)", 
     horiz =  TRUE,  nodePar = list(cex = .007))
legend("topleft", legend = iris_species, fill = rainbow_hcl(3), cex = 2)



## ----first_tanglegram, echo=FALSE, fig.height = 15, fig.width=20---------

hc1 <- hclust(dist(iris[,-5]), "com")
hc2 <- hclust(dist(iris[,-5]), "single")
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)

lines_col_true <- rainbow_hcl(3)[sort_levels_values(
      as.numeric(iris[,5])[order.dendrogram(dend1)]
   )]

tanglegram(dend1 , rev(dend2), 
           cex_main=3,
           cex_main_left=5,
           cex_main_right=5,
           color_lines=lines_col_true,
           main_left= "hclust complete", main_right="hclust single", main = "(true clusters)",
           lab.cex = .5, edge.lwd = 2, margin_inner= 3, 
           type = "r", center = TRUE, k_branches = 3
           )


## ------------------------------------------------------------------------
args(dendextend:::tanglegram.dendrogram)


## ----, warning=FALSE-----------------------------------------------------
DIST <- dist(c(1,2,4,5), diag=TRUE)
# create an heirarchical clustering object
hc1 <- hclust(DIST, "single") 
dend1 <- as.dendrogram(hc1)
dend1 <- color_labels(dend1)
labels(dend1) <- LETTERS[1:4]
dend2 <- rotate(dend1, order=c(1,2,4,3))
dend2_worst <- rev(dend1) # in the code it is a bit different


## ------------------------------------------------------------------------
entanglement(dend1,dend2, L = 1.5)


## ----simple_tanglegram_1, fig.height=7, fig.width = 8--------------------
tanglegram(dend1, dend2, 
           main = "Our data",
           color_lines = c(3,3,2,2), 
           lab.cex = 4, edge.lwd = 7,
           margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)


## ------------------------------------------------------------------------
abs_rank_diff <- abs(c(1:4)-c(1,2,4,3))
abs_rank_diff
L<-1.5
L_norm <- abs_rank_diff^L
sum_L_norm <- sum(L_norm)
worst_case <- sum((abs(c(1:4)-c(4:1)))^L)
sum_L_norm
worst_case
sum_L_norm / worst_case


## ------------------------------------------------------------------------
entanglement(dend1,dend2)


## ----simple_tanglegram_1, fig.height=3, fig.width = 8, echo=FALSE--------
tanglegram(dend1, dend2, 
           main = "Our data",
           color_lines = c(3,3,2,2), 
           lab.cex = 4, edge.lwd = 7,
           margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)


## ----simple_tanglegram_1_worst_case, fig.height=7, fig.width = 8, echo=FALSE----
tanglegram(dend1, dend2_worst, lab.cex = 4, edge.lwd = 7, color_lines = c(2,2,2,2), main = "Worst case",
                      margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)



## ------------------------------------------------------------------------
abs_rank_diff <- abs(c(1:4)-c(1,2,4,3))
abs_rank_diff
L<-0
L_norm <- abs_rank_diff^L
sum_L_norm <- sum(L_norm)
worst_case <- sum((abs(c(1:4)-c(4:1)))^L)
sum_L_norm
worst_case
sum_L_norm / worst_case


## ------------------------------------------------------------------------
entanglement(dend1,dend2, L = 0)


## ----simple_tanglegram_1, fig.height=7, fig.width = 8, echo=FALSE--------
tanglegram(dend1, dend2, 
           main = "Our data",
           color_lines = c(3,3,2,2), 
           lab.cex = 4, edge.lwd = 7,
           margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)


## ----simple_tanglegram_1_worst_case, fig.height=7, fig.width = 8, echo=FALSE----
tanglegram(dend1, dend2_worst, lab.cex = 4, edge.lwd = 7, color_lines = c(2,2,2,2), main = "Worst case",
                      margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)



## ----simple_tanglegram_one_shuffle_1, fig.height=7, fig.width = 8--------

set.seed(1112) # always set this first!
dend2_shuffle <- shuffle(dend2)
tanglegram(dend1, dend2_shuffle, lab.cex = 4, edge.lwd = 7, color_lines = c(2,2,3,3), main = "Different - but NOT better",
                      margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)


## ----simple_tanglegram_1, fig.height=7, fig.width = 8, echo=FALSE--------
tanglegram(dend1, dend2, 
           main = "Our data",
           color_lines = c(3,3,2,2), 
           lab.cex = 4, edge.lwd = 7,
           margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)


## ----simple_tanglegram_1_worst_case, fig.height=7, fig.width = 8, echo=FALSE----
tanglegram(dend1, dend2_worst, lab.cex = 4, edge.lwd = 7, color_lines = c(2,2,2,2), main = "Worst case",
                      margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)



## ----simple_tanglegram_random_search_1, fig.height=7, fig.width = 8------

set.seed(65168)
dend12 <- untangle_random_search(dend1, dend2, R=10)
tanglegram(dend12[[1]] , dend12[[2]], lab.cex = 4, edge.lwd = 7, color_lines = rep(3,4), main = "Problem solved",
                      margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)


## ----simple_tanglegram_1, fig.height=7, fig.width = 8, echo=FALSE--------
tanglegram(dend1, dend2, 
           main = "Our data",
           color_lines = c(3,3,2,2), 
           lab.cex = 4, edge.lwd = 7,
           margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)


## ----simple_tanglegram_1_worst_case, fig.height=7, fig.width = 8, echo=FALSE----
tanglegram(dend1, dend2_worst, lab.cex = 4, edge.lwd = 7, color_lines = c(2,2,2,2), main = "Worst case",
                      margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)



## ----simple_tanglegram_step_rotate_1side_example_1, fig.height=7, fig.width = 8----
# set.seed(65168) # doesn't matter
dend2_corrected <- untangle_step_rotate_1side(dend2, dend1)
tanglegram(dend1 , dend2_corrected, lab.cex = 4, edge.lwd = 7, color_lines = rep(3,4), main = "Problem solved",
                      margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)


## ----simple_tanglegram_1, fig.height=7, fig.width = 8, echo=FALSE--------
tanglegram(dend1, dend2, 
           main = "Our data",
           color_lines = c(3,3,2,2), 
           lab.cex = 4, edge.lwd = 7,
           margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)


## ----simple_tanglegram_1_worst_case, fig.height=7, fig.width = 8, echo=FALSE----
tanglegram(dend1, dend2_worst, lab.cex = 4, edge.lwd = 7, color_lines = c(2,2,2,2), main = "Worst case",
                      margin_inner= 5, margin_bottom = 0, 
           axes = FALSE)



