<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Doing more with dendrogram objects using dendextend}
-->






# Doing more with dendrogram objects
# A tutorial on using the {**_dendextend_**} package
================================================


Introduction
---------------


### The "dendrogram" object

The *"dendrogram"* class provides general functions for handling tree-like structures in R. It is intended as a replacement for similar functions in hierarchical clustering and classification/regression trees, such that all of these can use the same engine for plotting or cutting trees.

A dendrogram object represents a tree as a list object, with various attributes.

For example, let's create a dendrogram object based on an heirarchical clustering of 4 states in the U.S.:


```r
# our data:
data(USArrests)
US_data <- USArrests[c(2, 5, 32, 35), ]
print(US_data)
```

```
##            Murder Assault UrbanPop Rape
## Alaska       10.0     263       48 44.5
## California    9.0     276       91 40.6
## New York     11.1     254       86 26.1
## Ohio          7.3     120       75 21.4
```

```r

hc <- hclust(dist(US_data), "ave")  # create an heirarchical clustering object
dend <- as.dendrogram(hc)
```


Dendrogram has several useful methods bundled with R:


```r
methods(class = "dendrogram")
```

```
##  [1] [[.dendrogram*         as.hclust.dendrogram*  cophenetic.dendrogram*
##  [4] cut.dendrogram*        labels.dendrogram*     merge.dendrogram*     
##  [7] plot.dendrogram*       print.dendrogram*      reorder.dendrogram*   
## [10] rev.dendrogram*        str.dendrogram*       
## 
##    Non-visible functions are asterisked
```


Here are some examples for their use:


```r
print(dend)
```

```
## 'dendrogram' with 2 branches and 4 members total, at height 146.7
```

```r
labels(dend)
```

```
## [1] "Ohio"       "Alaska"     "California" "New York"
```

```r
str(dend)
```

```
## --[dendrogram w/ 2 branches and 4 members at h = 147]
##   |--leaf "Ohio" 
##   `--[dendrogram w/ 2 branches and 3 members at h = 44.1]
##      |--leaf "Alaska" 
##      `--[dendrogram w/ 2 branches and 2 members at h = 26.9]
##         |--leaf "California" 
##         `--leaf "New York"
```

```r
str(dend[[2]])  # looking at one branch of the dendrogram
```

```
## --[dendrogram w/ 2 branches and 3 members at h = 44.1]
##   |--leaf "Alaska" 
##   `--[dendrogram w/ 2 branches and 2 members at h = 26.9]
##      |--leaf "California" 
##      `--leaf "New York"
```

```r
plot(dend)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


You might notice how the order of the items (leaves/terminal nodes) of the dendrogram is different than their order in the table. In order to re-order the rows in the data-table to have the same order as the items in the dendrogram, we can use the *order.dendrogram* function:


```r
(new_order <- order.dendrogram(dend))  # the order of the original items to have them be at the same order as they assume in the dendrogram
```

```
## [1] 4 1 2 3
```

```r
print(US_data[new_order, ])
```

```
##            Murder Assault UrbanPop Rape
## Ohio          7.3     120       75 21.4
## Alaska       10.0     263       48 44.5
## California    9.0     276       91 40.6
## New York     11.1     254       86 26.1
```





In order to see what our dendrogram (list) object includes, we need to use the **unclass** function, which will allow us to print the list as is, without going through the print.dendrorgam method:


```r
unclass(dend)
```

```
## [[1]]
## [1] 4
## attr(,"members")
## [1] 1
## attr(,"height")
## [1] 0
## attr(,"label")
## [1] "Ohio"
## attr(,"leaf")
## [1] TRUE
## 
## [[2]]
## [[2]][[1]]
## [1] 1
## attr(,"members")
## [1] 1
## attr(,"height")
## [1] 0
## attr(,"label")
## [1] "Alaska"
## attr(,"leaf")
## [1] TRUE
## 
## [[2]][[2]]
## [[2]][[2]][[1]]
## [1] 2
## attr(,"label")
## [1] "California"
## attr(,"members")
## [1] 1
## attr(,"height")
## [1] 0
## attr(,"leaf")
## [1] TRUE
## 
## [[2]][[2]][[2]]
## [1] 3
## attr(,"label")
## [1] "New York"
## attr(,"members")
## [1] 1
## attr(,"height")
## [1] 0
## attr(,"leaf")
## [1] TRUE
## 
## attr(,"members")
## [1] 2
## attr(,"midpoint")
## [1] 0.5
## attr(,"height")
## [1] 26.9
## 
## attr(,"members")
## [1] 3
## attr(,"midpoint")
## [1] 0.75
## attr(,"height")
## [1] 44.14
## 
## attr(,"members")
## [1] 4
## attr(,"midpoint")
## [1] 0.875
## attr(,"height")
## [1] 146.7
```


We can see how each node in the dendrogram/list object has the following (self explaining) attributes:

```r
names(attributes(dend)[-4])
```

```
## [1] "members"  "midpoint" "height"
```

Also, terminal nodes also has the "leaf" attribute (set to TRUE).


### Motivation for creating {**_dendextend_**}

The *dendrogram* object has several **advantages**:

1. *dendrogram* objects are simply list R objects. This makes their structure  very simple to understand by R users.
2. *dendrogram* objects has various methods and functions for using them in R. 
3. *dendrogram* objects are relatively simple to manipulte and extend.
4. Other tree objects (such as *hclust*, and objects from the *{ape}* package) include an *as.dendrogram* method for converting their objects into a dendrogram.

However, even with all of its advantages, the *dendrogram* class in R still lacks various basic features.

The {**_dendextend_**} package aims at filling some gaps in base R, by extending the available functions for dendrogram manipulation, statistical analysis, and visualization.

This vignettes Provides a step-by-step description of the functionality provided by the {**_dendextend_**} package.


### Installing {**_dendextend_**}

To install the stable version from CRAN use:


```r
install.packages("dendextend")  # not yet available from CRAN
```


To install the [GitHub version](https://github.com/talgalili/dendextend) use:


```r
if (!require("devtools")) install.packages("devtools")
require("devtools")
install_github("dendextend", "talgalili")
```



Labels extraction and assignment 
--------------------------------

### labels in base R

In base R, the *labels* function is intended to find/extract a suitable set of labels from an object for use in printing or plotting, for example. By default, it uses the *names* and *dimnames* functions.

What base R *labels* function is mising is assignment. In the next few examples we will go through different examples of what the {**_dendextend_**} package offers for various objects.

These assignment functions were originally written by Gavin Simpson (in a post on [stackoverflow](http://stackoverflow.com/questions/4614223/how-to-have-the-following-work-labelsx-some-value-r-question))


### labels for vectors and matrixes

In base R, for vectors, labels gives the *names* of the object. And if these are missing, then *labels* will give the vector itself as a character vector:


```r
x <- 1:3
names(x)  # this vector has no names
```

```
## NULL
```

```r
labels(x)  # this vector has no labels
```

```
## [1] "1" "2" "3"
```


Assignment to names is available in base R and works as follows:


```r
x <- 1:3
names(x) <- letters[1:3]  # assignment for names is in base R
# both names and labels will give the same result:
names(x)
```

```
## [1] "a" "b" "c"
```

```r
labels(x)
```

```
## [1] "a" "b" "c"
```



The new labels assignment function will allow a user to change the labels of the vector just as if it was "names":


```r
x <- 1:3
labels(x) <- letters[1:3]
```

```
## Error: could not find function "labels<-"
```

```r
names(x)
```

```
## NULL
```

```r
labels(x)
```

```
## [1] "1" "2" "3"
```


Let's see how labels assignment will work for 



   
   labels(x) <- letters[1:3]



### labels for dendrogram objects


### labels for hclust objects








