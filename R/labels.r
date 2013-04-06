####################
## Adding labels assignment as an S3 method (and extending it to include hclust)
####################





# turning label insertion function into a method:

#' @title "label" assignment operator
#' @description "label" assignment operator for vectors, dendrogram, and hclust classes.
#' @aliases 
#' labels<-.default 
#' labels<-.dendrogram 
#' labels.hclust labels<-.hclust
#' labels.matrix labels<-.matrix
#' @usage
#' labels(object, ...) <- value
#' 
#' \method{labels}{matrix}(object, which = c("colnames", "rownames"), ...)
#' 
#' \method{labels}{matrix}(object, which = c("colnames", "rownames"), ...) <- value
#' 
#' \method{labels}{dendrogram}(object, ...) <- value
#' 
#' \method{labels}{hclust}(object, ...)
#' 
#' \method{labels}{hclust}(object, ...) <- value
#' @param object a variable name (possibly quoted) who's label are to be updated
#' @param which "colnames" or "rownames", to which of the two should labels refer to.
#' @param ... parameters passed (not currently in use)
#' @param value a value to be assigned to object's label
#' @return The updated object
#' @author Gavin Simpson, Tal Galili
#' @export
#' @source The functions here are based on code by Gavin and kohske from (adopted to dendrogram by Tal Galili): \url{http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question}
#' @seealso \code{\link{labels}}, \code{\link{labels.matrix}}
#' @examples
#' x <- 1:3 
#' labels(x)
#' labels(x) <- letters[1:3]
#' labels(x) # [1] "a" "b" "c"
#' x
#' # a b c 
#' # 1 2 3 
#' 
#' 
#' # get("labels<-")
#' 
#' ################
#' # Example for matrix objects
#' 
#' x <- matrix(1:9, 3,3)
#' labels(x)
#' x
#' labels(x) <- letters[1:3]
#' x
#' labels(x, which = "rownames") <- LETTERS[24:26]
#' x
#' #  a b c
#' #X 1 4 7
#' #Y 2 5 8
#' #Z 3 6 9
#' 
#' ################
#' # Example for using the assignment with dendrogram and hclust objects:
#' hc <- hclust(dist(USArrests[1:3,]), "ave")
#' dend <- as.dendrogram(hc)
#' 
#' labels(hc) # "Arizona" "Alabama" "Alaska" 
#' labels(hc)  <- letters[1:3]
#' labels(hc)# "a" "b" "c"
#' labels(dend) # "Arizona" "Alabama" "Alaska" 
#' labels(dend) <- letters[1:3]
#' labels(dend) # "a" "b" "c"
#' labels(dend) <- LETTERS[1:2] # will produce a warning
#' labels(dend) # "A" "B" "A"
#' labels(dend) <- LETTERS[4:6] # will replace the labels correctly (the fact the tree had duplicate labels will not cause a problem)
#' labels(dend) # "D" "E" "F"
"labels<-" <- function(object,..., value) UseMethod("labels<-")

# example("labels<-")
# ?"labels<-"


# ' @title "label" assignment operator - default
#' @export
"labels<-.default" <- function(object,..., value) {
   names(object) <- value # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
   object
}



# ' @title "label" assignment operator - dendrogram
# ' @export
#' @S3method labels<- dendrogram
"labels<-.dendrogram" <- function(object,..., value) {
   # credit for the help on how to write this type of function goes to:
   # Gavin Simpson and also kohske, see here:
   # http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question

   # deals with wrong length of new labels VS tree size
   new_labels <- as.character(value)
   new_labels_length <- length(new_labels)
   leaves_length <- length(order.dendrogram(object)) # labels(object) # it will be faster to use order.dendrogram than labels...   
   if(new_labels_length < leaves_length) {
      warning("The lengths of the new labels is shorter than the number of leaves in the dendrogram - labels are recycled.")
      new_labels <- rep(new_labels, length.out = leaves_length)
   }
   
   .change.label.LTR <- function(dend_node)
   {
      if(is.leaf(dend_node))
      {			
         i_leaf_number <<- i_leaf_number + 1 # this saves us from cases of duplicate enteries...
         attr(dend_node, "label") <- new_labels[i_leaf_number]
      }
      return(dend_node)
   }
   
   i_leaf_number <- 0
   new_dend_object <- dendrapply(object, .change.label.LTR)

   return(new_dend_object)
}


# ' @title Find Labels from hclust Object
# ' @export
#' @S3method labels hclust
labels.hclust <- function(object, ...)  as.character(object$labels)

# ' @title "label" assignment operator - hclust
# ' @export
#' @S3method labels<- hclust
"labels<-.hclust" <- function(object,..., value) {
   object$labels <- value  # I assume here that if ever labels will be used in the naive sense, it would be as a synonym to "names"
   return(object)
}






# ' @title "label" assignment operator for matrix class
# ' @export
#' @S3method labels matrix
labels.matrix <- function(object, which = c("colnames","rownames"), ...) {
   if(missing(which))
      which <- "colnames"
   which <- match.arg(which)
   if(which == "colnames") {
      out <- colnames(object)
   } else {
      out <- rownames(object)
   }
   out
}
# example("labels.matrix")
# ?"labels.matrix"

# ' @title "label" assignment operator - matrix
# ' @export
#' @S3method labels<- matrix
'labels<-.matrix' <- function(object, which = c("colnames","rownames"), ..., value) {
   if(missing(which))
      which <- "colnames"
   which <- match.arg(which)
   if(which == "colnames") {
      colnames(object) <- value
   } else {
      rownames(object) <- value
   }
   object
}


# methods("labels")
# methods("labels<-")
# example("labels.matrix")
