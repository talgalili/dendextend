
## ----setup, include=FALSE, echo=FALSE------------------------------------
require(knitr)
opts_chunk$set(cache=TRUE, fig.align='center')
options(digits = 3)
purl("2013-09-05_Boston-useR_01_intro.Rpres")


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


