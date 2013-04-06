# dendextend

## Introduction

Class "dendrogram" provides general functions for handling tree-like structures. It is intended as a replacement for similar functions in hierarchical clustering and classification/regression trees, such that all of these can use the same engine for plotting or cutting trees.

However, many basic features are still missing from the dendrogram class.  This package aims at filling in some gaps.


## Motivation

Extending R core dendrogram functions.

## Installation

To install the stable version on CRAN:

```r
# install.packages('dendextend') # not yet available from CRAN
```

To install the GitHub version:

```r
if (!require('devtools')) install.packages('devtools'); require('devtools')
install_github('dendextend', 'talgalili')
```

## Usage

If you are using the Rgui, you will see a new menu added on your top right (just by "help"), giving you the option to update R, or install new software.

For command line use you can **update R** by running:

```r
require(installr)
updateR() # this will open dialog boxes to take you through the steps.
```

Or **install a new software** simply by running:

```r
require(installr)
installr() #  user can easily select (via a GUI interface) a software to install.
```


## Contact

You are welcome to:
* submit suggestions and bug-reports at: <https://github.com/talgalili/dendextend/issues>
* send pull requests are welcome on: <https://github.com/talgalili/dendextend/>
* compose friendly e-mail to: <tal.galili@gmail.com>


## Available functions

* labels assignment operators for vector, matrix, dendrogram, hclust.
* count_terminal_nodes
* unclass_dend
* labels_colors (retrieving and assignment)
* head.dendrogram (S3 method for dendrogram)
* nleaves (with S3 methods for dendrogram and hclust)
* rotate (with S3 methods for dendrogram and hclust)
* sort (with S3 methods for dendrogram and hclust)
* flip (works for both dendrogram and hclust)
