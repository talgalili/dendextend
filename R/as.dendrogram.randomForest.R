# plotting a randomForest tree!
# http://stats.stackexchange.com/questions/2344/best-way-to-present-a-random-forest-in-a-publication
# I will still need to clean this into this:
# as.dendrogram.randomForest




# to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){

  # if(dfrep[rownum,'status'] == -1){
    # rval <- list()

    # attr(rval,"members") <- 1
    # attr(rval,"height") <- 0.0
    # attr(rval,"label") <- dfrep[rownum,'prediction']
    # attr(rval,"leaf") <- TRUE

  # }else{##note the change "to.dendrogram" and not "to.dendogram"
    # left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    # right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    # rval <- list(left,right)

    # attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    # attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    # attr(rval,"leaf") <- FALSE
    # attr(rval,"edgetext") <- dfrep[rownum,'split var']
  # }

  # class(rval) <- "dendrogram"

  # return(rval)
# }


# library(randomForest)
# set.seed(1)
# data(iris, envir = environment())
# iris.rf <- randomForest(Species ~ ., iris, proximity=TRUE,
                        # keep.forest=FALSE)
# MDSplot(iris.rf, iris$Species)


# set.seed(1)
# mod <- randomForest(Species ~ .,data=iris)
# tree <- getTree(mod,1,labelVar=TRUE)




# d <- to.dendrogram(tree)
# str(d)
# plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=1,p.col=NA,p.lty=0))

