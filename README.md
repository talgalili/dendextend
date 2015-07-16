[![Build Status](https://travis-ci.org/talgalili/dendextend.png?branch=master)](https://travis-ci.org/talgalili/dendextend)
[![codecov.io](http://codecov.io/github/talgalili/dendextend/coverage.svg?branch=master)](http://codecov.io/github/talgalili/dendextend?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dendextend)](http://cran.r-project.org/package=dendextend)

# dendextend

**Table of contents:**

* [Introduction](#introduction)
* [Motivation](#motivation)
* [Installation](#installation)
* [Usage](#usage)
* [Share your dendrograms!](#share-your-dendrograms)
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
install.packages('dendextendRcpp')
```

To install the GitHub version:

```R
install.packages.2 <- function (pkg) if (!require(pkg)) install.packages(pkg);
install.packages.2('devtools')
install.packages.2('Rcpp')
# make sure you have Rtools installed first! if not, then run:
#install.packages('installr'); install.Rtools()
devtools::install_github('talgalili/dendextend')
devtools::install_github('talgalili/dendextendRcpp')

# Having colorspace is also useful, since it is used
# In various examples in the vignettes
install.packages.2('colorspace')
```

And then you may load the package using:

```R
library("dendextend")
library("dendextendRcpp")
```

## Usage

Vignettes: 

* [Introduction to dendextend](http://htmlpreview.github.io/?https://github.com/talgalili/dendextend/blob/master/inst/ignored/Introduction%20to%20dendextend.html)
* [Frequently asked questions](http://htmlpreview.github.io/?https://github.com/talgalili/dendextend/blob/master/inst/ignored/Frequently%20asked%20questions.html)
* [Hierarchical cluster analysis on famous data sets - enhanced with the dendextend package](http://htmlpreview.github.io/?https://github.com/talgalili/dendextend/blob/master/inst/ignored/Hierarchical%20cluster%20analysis%20on%20famous%20data%20sets%20-%20enhanced%20with%20the%20dendextend%20package.html)
* [Introduction vignette (older)](https://github.com/talgalili/dendextend/blob/master/inst/doc/dendextend-tutorial.pdf)  (older)

## Share your dendrograms!

If you have made interesting work using the dendextend package, I would LOVE to know about it. It can be a blog post, an academic paper, or just some plots you made for your work in the industry. Please contact me (see below) with what you have done, and I would also be happy to promote it in this page.

### Usages of dendextend

Packages:
* [ComplexHeatmap](http://www.bioconductor.org/packages/release/bioc/vignettes/ComplexHeatmap/inst/doc/ComplexHeatmap.html)
* [d3heatmap](http://cran.r-project.org/web/packages/d3heatmap/vignettes/Introduction.html)




## Contact

You are welcome to:

* submit suggestions and bug-reports at: <https://github.com/talgalili/dendextend/issues>
* send a pull request on: <https://github.com/talgalili/dendextend/>
* compose a friendly e-mail to: <tal.galili@gmail.com>


## Latest news

You can see the most recent changes to the package in the [NEWS.md file](https://github.com/talgalili/dendextend/blob/master/NEWS.md)



## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

