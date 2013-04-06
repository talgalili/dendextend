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

TO BE UPDATED...

## Contact

You are welcome to:
* submit suggestions and bug-reports at: <https://github.com/talgalili/dendextend/issues>
* send a pull request on: <https://github.com/talgalili/dendextend/>
* compose a friendly e-mail to: <tal.galili@gmail.com>


## Available functions

* labels assignment operators for vector, matrix, dendrogram, hclust.
* count_terminal_nodes
* unclass_dend
* labels_colors (retrieving and assignment)
* head.dendrogram (S3 method for dendrogram)
* nleaves (with S3 methods for dendrogram and hclust)
* rotate (with S3 methods for dendrogram, hclust, and phylo)
* sort (with S3 methods for dendrogram and hclust)
* flip (works for both dendrogram and hclust)
* trim - trims leaves off a dendrogram/hclust/phylo trees. (based on the trim_leaf function)
* as_hclust_fixed
* get_branches_attr
* unroot (dendrogram/hclust/phylo)
* raise.dendrogram
* flatten.dendrogram
