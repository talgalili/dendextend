# dendextend

## Introduction

Class "dendrogram" provides general functions for handling tree-like structures. It is intended as a replacement for similar functions in hierarchical clustering and classification/regression trees, such that all of these can use the same engine for plotting or cutting trees.

However, many basic features are still missing from the dendrogram class.  This package aims at filling in some gaps.


## Motivation

Extending R core dendrogram functions.

## Installation

To install the stable version on CRAN:

```r
# install.packages('dendextend')
```

To install the GitHub version:

```r
if (!require('installr')) install.packages('installr'); require('installr')
# the installr includes the "require2" function - making it faster to add/load new packages.
## install.Rtools() # run this if you are using Windows and don't have Rtools installed
require2(devtools)
install_github('talgalili/dendextend')
require2(Rcpp)
install_github('talgalili/dendextendRcpp')

# Having colorspace is also useful, since it is used
# In various examples in the vignettes
require2(colorspace)
```

And then you may load the package using:
```r
library(dendextend)
# library(dendextendRcpp) # loads by itself, if installed.
# And you might also wish to load:
library(colorspace)
```

## Usage

Please see:
- Vignette: https://github.com/talgalili/dendextend/blob/master/vignettes/dendextend-tutorial.pdf
- Presentations: 
   - http://htmlpreview.github.com/?https://raw.github.com/talgalili/dendextend/master/inst/doc/2013-09-05_Boston-useR/2013-09-05_Boston-useR_01_intro.html
   - http://htmlpreview.github.com/?https://raw.github.com/talgalili/dendextend/master/inst/doc/2013-09-05_Boston-useR/2013-09-05_Boston-useR_02_dendextend.html
   - http://htmlpreview.github.com/?https://raw.github.com/talgalili/dendextend/master/inst/doc/2013-09-05_Boston-useR/2013-09-05_Boston-useR_03_untangle_iris.html
   - http://htmlpreview.github.com/?https://raw.github.com/talgalili/dendextend/master/inst/doc/2013-09-05_Boston-useR/2013-09-05_Boston-useR_04_tree_inference.html

## Share your dendrograms!

If you have made interesting work using the dendextend package, I would LOVE to know about it. It can be a blog post, an academic paper, or just some plots you made for your work in the industry. Please contact me with what you have done, and I would also be happy to promote it in this page.

## Contact

You are welcome to:
* submit suggestions and bug-reports at: <https://github.com/talgalili/dendextend/issues>
* send a pull request on: <https://github.com/talgalili/dendextend/>
* compose a friendly e-mail to: <tal.galili@gmail.com>


## Available functions

You can see the most recent changes to the package in the NEWS.md file:

- https://github.com/talgalili/dendextend/blob/master/NEWS.md



* labels assignment operators for vector, matrix, dendrogram, hclust.
* count_terminal_nodes
* unclass_dend
* labels_colors (retrieving and assignment)
* head.dendrogram (S3 method for dendrogram)
* nleaves (with S3 methods for dendrogram and hclust)
* rotate (with S3 methods for dendrogram, hclust, and phylo)
* sort (with S3 methods for dendrogram and hclust)
* flip (works for both dendrogram and hclust)
* prune - prunes leaves off a dendrogram/hclust/phylo trees. (based on the prune_leaf function)
* as_hclust_fixed
* get_branches_attr
* unroot (dendrogram/hclust/phylo)
* raise.dendrogram
* flatten.dendrogram
* order.dendrogram<-
* intersect_trees