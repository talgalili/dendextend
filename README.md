<!-- badges: start -->
[![Build Status](https://travis-ci.org/talgalili/dendextend.png?branch=master)](https://travis-ci.org/talgalili/dendextend)
[![codecov.io](https://codecov.io/github/talgalili/dendextend/coverage.svg?branch=master)](https://codecov.io/github/talgalili/dendextend?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dendextend)](https://cran.r-project.org/package=dendextend)
![](https://cranlogs.r-pkg.org/badges/dendextend?color=yellow)
![](https://cranlogs.r-pkg.org/badges/grand-total/dendextend?color=yellowgreen)
[![R-CMD-check](https://github.com/talgalili/dendextend/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/talgalili/dendextend/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# dendextend

**[dendextend website](https://talgalili.github.io/dendextend/) (built using [pkgdown](https://pkgdown.r-lib.org/))**


**Table of contents:**

* [Introduction](#introduction)
* [Installation](#installation)
* [Usage](#usage)
* [Getting help](#getting-help)
* [How to cite the dendextend package](#how-to-cite-the-dendextend-package)
* [Submitting bug reports and patches](#submitting-bug-reports-and-patches)


## Introduction

Class "dendrogram" provides general functions for handling tree-like structures in R. It is intended as a replacement for similar functions in hierarchical clustering and classification/regression trees, such that all of these can use the same engine for plotting or cutting trees.

However, many basic features are still missing from the dendrogram class.  This package aims at filling in some gaps.

**The dendextend package extending R core dendrogram functions.**

## Installation

To install and load dendextend, simply use:

```r
install.packages('dendextend') # stable CRAN version
library("dendextend") # load the package
```

## Usage

Vignettes: 

* [Introduction to dendextend](https://talgalili.github.io/dendextend/articles/dendextend.html)
* [Frequently asked questions](https://talgalili.github.io/dendextend/articles/FAQ.html)
* [Hierarchical cluster analysis on famous data sets - enhanced with the dendextend package](https://talgalili.github.io/dendextend/articles/Cluster_Analysis.html)

Also ceck out the [dendextend tag in stackoverflow](https://stackoverflow.com/questions/tagged/dendextend) for more examples.

A notable sister package for dendextend is [heatmaply](https://talgalili.github.io/heatmaply/articles/heatmaply.html) for creating interactive cluster heatmaps using R (combining dendextend and plotly).

## Getting help

Please post your question to stackoverflow using the tags: [dendextend](https://stackoverflow.com/questions/tagged/dendextend) and [r](https://stackoverflow.com/questions/tagged/dendextend).


## How to cite the dendextend package

The methods within the code package can be cited as:

     Tal Galili (2015). dendextend: an R package for visualizing, adjusting, and comparing trees of
     hierarchical clustering. Bioinformatics. DOI: 10.1093/bioinformatics/btv428

A BibTeX entry for LaTeX users is

     @Article{,
       author = {Tal Galili},
       title = {dendextend: an R package for visualizing, adjusting, and comparing trees of hierarchical clustering},
       journal = {Bioinformatics},
       year = {2015},
       doi = {10.1093/bioinformatics/btv428},
       url = {https://academic.oup.com/bioinformatics/article/31/22/3718/240978/dendextend-an-R-package-for-visualizing-adjusting},
       eprint = {https://academic.oup.com/bioinformatics/article-pdf/31/22/3718/17122682/btv428.pdf},
     }

This free open-source software implements academic research by the authors and co-workers. If you use
it, please support the project by citing the appropriate journal articles.



## Submitting bug reports and patches

You are welcome to:

* submit bug-reports and features' suggestions at: <https://github.com/talgalili/dendextend/issues>
* send a pull request on: <https://github.com/talgalili/dendextend/>
* compose a friendly e-mail to: <tal.galili@gmail.com>

Before reporting bugs, please make sure you're using the latest version from github:

```R
install.packages.2 <- function (pkg) {
   if (!require(pkg, character.only = TRUE)) 
   install.packages(pkg, character.only = TRUE)
   }
install.packages('remotes')
remotes::install_github('talgalili/dendextend')

# Having colorspace is also useful, since it is used
# In various examples in the vignettes
install.packages.2('colorspace')
```

## Latest news

You can see the most recent changes to the package in the [NEWS.md file](https://talgalili.github.io/dendextend/news/index.html)



## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/talgalili/dendextend/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.

