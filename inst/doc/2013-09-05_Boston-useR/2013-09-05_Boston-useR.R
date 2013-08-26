
## ----setup, include=FALSE, echo=FALSE------------------------------------
require(knitr)
opts_chunk$set(cache=TRUE, fig.align='center')
options(digits = 3)
purl("2013-09-05_Boston-useR.Rpres")


## ----precip_subset_data_prep, echo = TRUE, eval = TRUE-------------------
data(precip)
precip_data <- 
   round(precip[c("Boston", "New York", "Nashville")])
DIST <- dist(precip_data, diag=TRUE)
# create an heirarchical clustering object
hc <- hclust(DIST, method = "single") 


## ----, echo = FALSE------------------------------------------------------
print(precip_data)


## ----, echo = FALSE------------------------------------------------------
print(DIST)


## ----precip_subset__hclust_single_plot, echo = TRUE, fig.height=9, eval = TRUE----
par(cex=1.14, cex.lab = 1.2, lwd = 4,las = 1)
plot(hc, hang = -1) 
abline(h = c(2,4), 
       lty = 2, col = 2, lwd = 2)


## ------------------------------------------------------------------------
cutree(hc, k = 2)


## ------------------------------------------------------------------------
cutree(hc, h = 3.5)


## ----precip_subset__hclust_single_plot_rect,echo = TRUE, fig.height=9, eval = TRUE----
par(cex=1.14, cex.lab = 1.2, lwd = 4,las = 1)
plot(hc, hang = -1) 

abline(h = 3.5, col = 2, lty = 2)
rect.hclust(hc, k = 2, border = 3)
# No - xlim didn't fix this...


## ----,echo = FALSE, fig.height=9, fig.width=25, eval = TRUE--------------
x <- c(1:3, 5)
names(x) <- c(1:3, 5) # We MUST add names...
print(x)
x_dist <- dist(x)
par(mfrow = c(1,3), cex=1.14, cex.lab = 1.2, lwd = 4,cex.main=2)
plot(hclust(x_dist, method = "single"), main = "single", hang = -1) 
plot(hclust(x_dist, method = "complete"), main = "complete", hang = -1) 
plot(hclust(x_dist, method = "average"), main = "average", hang = -1) 


## ----hclust_speed_measurment---------------------------------------------
DIST <- dist(rnorm(4000))
system.time(hc2 <- hclust(DIST))
system.time(cutree(hc2, k = 4))


## ------------------------------------------------------------------------
str(hc)


## ------------------------------------------------------------------------
data(precip)
precip_data <- 
   round(precip[c("Boston", "New York", "Nashville")])
DIST <- dist(precip_data, diag=TRUE)
# create an heirarchical clustering object
hc <- hclust(DIST, "single") 
#########################
##### hclust to dend ####
#########################
dend <- as.dendrogram(hc)

print(dend)
str(dend)


## ----precip_dend_plot_1, echo=FALSE, fig.height=9------------------------
par(cex=2.5, lwd = 4,las = 1)
plot(dend) 


## ------------------------------------------------------------------------
str(unclass(dend))


## ----precip_dend_plot_1, echo=FALSE, fig.height=9------------------------
par(cex=2.5, lwd = 4,las = 1)
plot(dend) 


## ----, cache=FALSE-------------------------------------------------------
as.matrix(methods(class="dendrogram"))


## ----precip_dend_plot_1, echo=FALSE, fig.height=9------------------------
par(cex=2.5, lwd = 4,las = 1)
plot(dend) 


## ------------------------------------------------------------------------
labels(dend)


## ------------------------------------------------------------------------
order.dendrogram(dend)


## ------------------------------------------------------------------------
print(str(dend[[2]]))


## ----precip_dend_plot_1, echo=FALSE, fig.height=9------------------------
par(cex=2.5, lwd = 4,las = 1)
plot(dend) 


## ------------------------------------------------------------------------
cut(dend,h=50)


## ------------------------------------------------------------------------
cut_dend <- cut(dend,h=3)$lower
# now let's get our tree back:
merge(cut_dend[[1]], cut_dend[[2]], height = 4)


## ----precip_dend_plot_2, echo=FALSE, fig.height=9------------------------
par(cex=2.5, lwd = 4,las = 1)
plot(dend) 
abline(h = 3, lty = 2, col = 2)


## ------------------------------------------------------------------------
ord <- order.dendrogram(dend)
DIST <- dist(precip_data[ord], diag=TRUE)
print(DIST, diag = TRUE)
coph_dend <- cophenetic(dend)
as.dist(coph_dend, diag = TRUE)


## ------------------------------------------------------------------------
round(
   cor(DIST, coph_dend),
   2)


## ----precip_dend_plot_1, echo=FALSE, fig.height=9------------------------
par(cex=2.5, lwd = 4,las = 1)
plot(dend) 


## ----, fig.width=20------------------------------------------------------
par(mfrow = c(1,2))
plot(dend) # has various options
plot(rev(dend))


## ------------------------------------------------------------------------
args(stats:::plot.dendrogram)


## ----, fig.width=20, fig.height=10---------------------------------------
par(mfrow = c(1,2), cex = 2)
plot(dend, main = "default") 
plot(dend, main = "modified",
     type = "t", center = TRUE, horiz = TRUE, 
     nodePar = list(lab.col = 4), 
     edgePar = list(col = 3 , lwd = 3)) 


## ------------------------------------------------------------------------
print_height <- function(x) { 
   print(attr(x, "height"))
   }

tmp <- dendrapply(dend,
                  print_height)


## ----ref.label='precip_subset__hclust_single_plot', echo = FALSE, fig.height=9, eval = TRUE----


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


## ----, fig.height=10, fig.width=10---------------------------------------
dend4 <- hang.dendrogram(dend3)

par(cex = 2, lwd = 2, las = 2)
plot(dend4)   


## ----, fig.height=10, fig.width=10---------------------------------------
par(cex = 2, lwd = 2, las = 2) 
plot(dend3)   

abline(h=3, col = 3, lty = 2)


## ----, fig.height=10, fig.width=10---------------------------------------


par(cex = 2, lwd = 2, las = 2)
plot(dend4)   


## ----, fig.height=10, fig.width=10---------------------------------------




dend5 <- rotate(dend4, c(4, 1:3, 5))
par(cex = 2, lwd = 2, las = 2)
plot(dend5)   


## ----, fig.height=10, fig.width=10---------------------------------------
dend6 <- unbranch(dend5)

par(cex = 2, lwd = 2, las = 2) 
plot(dend6) 


## ------------------------------------------------------------------------
# as.hclust(dend) is not longer possible...


## ----, fig.height=10, fig.width=10---------------------------------------




dend5 <- rotate(dend4, c(4, 1:3, 5))
par(cex = 2, lwd = 2, las = 2)
plot(dend5)   


## ----, fig.height=10, fig.width=10---------------------------------------
dend6 <- unbranch(dend5)

par(cex = 2, lwd = 2, las = 2) 
plot(dend6) 


## ----, fig.height=10, fig.width=10---------------------------------------
dend7 <- trim(dend6, c("Wshng", "NwYrk")) 

par(cex = 2, lwd = 2, las = 2) 
plot(dend7) 


## ------------------------------------------------------------------------
cutree(dend7 , k=3)
cutree(dend7 , k=3,order_clusters_as_data=FALSE)
args(cutree.dendrogram)


## ----, fig.height=10, fig.width=10---------------------------------------



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
      as.numeric(iris[,5])[order.dendrogram(dend_iris)]
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


