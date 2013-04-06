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
"labels<-" <- function(object,..., value) UseMethod("labels<-")

# example("labels<-")
# ?"labels<-"


# ' @title "label" assignment operator - default
#' @export
"labels<-.default" <- function(object,..., value) {
   # object <- value		
   # since labels first goes to object names, then a change in objects labels (in a default setting), should result in changing it's "names"
   # x <- 1:3
   # labels(x)
   # names(x) <- letters[1:3]
   # labels(x)
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
   # this function gets:
   #	object - a dendrogram object
   # 	value - the values of the new labels to insert instead of the old labels
   
   new_labels <- as.character(value)
   old_labels <- labels(object)
   if(length(old_labels) != length(new_labels)) stop("The lengths of old_labels and new_labels differ - you'll need to fix this in order for the function to proporly change the dendrogram labels")
   
   labels_mat <- cbind(old_labels, new_labels)
   .change.label.by.mat <- function(dend_node, new_labels_mat = labels_mat)
   {
      if(is.leaf(dend_node))
      {			
         node_label <- as.character(attr(dend_node, "label"))
         optional_labels <- as.character(new_labels_mat[,1])
         i_leaf_number <<- i_leaf_number + 1
         ss <- i_leaf_number # this saves us from cases of duplicate enteries...
         # ss <- which(optional_labels %in% node_label)[1] # the "which" makes sure that if there are multiple labels of the same name, nothing "bad" will happen here
         attr(dend_node, "label") <- new_labels_mat[,2][ss]
      }
      return(dend_node)
   }
   # mtrace(".change.label.by.mat")
   i_leaf_number <- 0
   new_dend_object <- dendrapply(object, .change.label.by.mat)
   # This function could also be written using a running index of "i", with using "i <<- i + 1" to move in each step
   # But I felt this way of writing the code was more clearly read (although, for production, the i<<-i+1 might be a better alternative)
   # update - this function should be rewritten with the i<<-i+1 method - so to enable cases of duplicate labels... 
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
   # this function gets:
   #	object - a hclust object
   # 	value - the values of the new labels to insert instead of the old labels	
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
