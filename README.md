[![Build Status](https://travis-ci.org/talgalili/dendextend.png?branch=master)](https://travis-ci.org/talgalili/dendextend)
[![codecov.io](https://codecov.io/github/talgalili/dendextend/coverage.svg?branch=master)](https://codecov.io/github/talgalili/dendextend?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dendextend)](https://cran.r-project.org/package=dendextend)
![](http://cranlogs.r-pkg.org/badges/dendextend?color=yellow)
![](http://cranlogs.r-pkg.org/badges/grand-total/dendextend?color=yellowgreen)

# dendextend

**Table of contents:**

* [Introduction](#introduction)
* [Motivation](#motivation)
* [Installation](#installation)
* [Usage](#usage)
* [Share your dendrograms!](#share-your-dendrograms)
* [How to cite the dendextend package](#how-to-cite-the-dendextend-package)
* [Contact](#contact)


## Introduction

Class "dendrogram" provides general functions for handling tree-like structures in R. It is intended as a replacement for similar functions in hierarchical clustering and classification/regression trees, such that all of these can use the same engine for plotting or cutting trees.

However, many basic features are still missing from the dendrogram class.  This package aims at filling in some gaps.


## Motivation

Extending R core dendrogram functions.

## Installation

To install the stable version on CRAN:

```r
install.packages('dendextend')
# install.packages('dendextendRcpp')
```

To install the GitHub version:

```R
install.packages.2 <- function (pkg) if (!require(pkg)) install.packages(pkg);
install.packages.2('devtools')
# install.packages.2('Rcpp')
# make sure you have Rtools installed first! if not, then run:
#install.packages('installr'); install.Rtools()
devtools::install_github('talgalili/dendextend')
# devtools::install_github('talgalili/dendextendRcpp')

# Having colorspace is also useful, since it is used
# In various examples in the vignettes
install.packages.2('colorspace')
```

And then you may load the package using:

```R
library("dendextend")
# library("dendextendRcpp") # dendextendRcpp is no longer supported (or available on CRAN). Still, dendextend is faster these days, in recent versions of R, thanks to some updates to labels.dendrogram and other modifications to R.
```

## Usage

Vignettes: 

* [Introduction to dendextend](https://htmlpreview.github.io/?https://github.com/talgalili/dendextend/blob/master/inst/ignored/Introduction%20to%20dendextend.html)
* [Frequently asked questions](https://htmlpreview.github.io/?https://github.com/talgalili/dendextend/blob/master/inst/ignored/Frequently%20asked%20questions.html)
* [Hierarchical cluster analysis on famous data sets - enhanced with the dendextend package](https://htmlpreview.github.io/?https://github.com/talgalili/dendextend/blob/master/inst/ignored/Hierarchical%20cluster%20analysis%20on%20famous%20data%20sets%20-%20enhanced%20with%20the%20dendextend%20package.html)
* [Introduction vignette (older)](https://github.com/talgalili/dendextend/blob/master/inst/doc/dendextend-tutorial.pdf)  (older)

## Share your dendrograms!

If you have made interesting work using the dendextend package, I would LOVE to know about it. It can be a blog post, an academic paper, or just some plots you made for your work in the industry. Please contact me (see below) with what you have done, and I would also be happy to promote it in this page.

### Usages of dendextend

Packages:
* [heatmaply](https://github.com/talgalili/heatmaply)
* [ComplexHeatmap](https://www.bioconductor.org/packages/3.3/bioc/html/ComplexHeatmap.html)
* [d3heatmap](https://CRAN.R-project.org/package=d3heatmap/vignettes/Introduction.html)


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



## Contact

You are welcome to:

* submit suggestions and bug-reports at: <https://github.com/talgalili/dendextend/issues>
* send a pull request on: <https://github.com/talgalili/dendextend/>
* compose a friendly e-mail to: <tal.galili@gmail.com>


## Latest news

You can see the most recent changes to the package in the [NEWS.md file](https://github.com/talgalili/dendextend/blob/master/NEWS.md)



## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

