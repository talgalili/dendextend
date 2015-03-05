[![Build Status](https://travis-ci.org/talgalili/dendextend.png?branch=master)](https://travis-ci.org/talgalili/dendextend)


# dendextend

**Table of content:**

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
require2 <- function (package, ...) {
	if (!require(package)) install.packages(package); library(package)
}

## require2('installr')
## install.Rtools() # run this if you are using Windows and don't have Rtools installed

# Load devtools:
require2("devtools")
devtools::install_github('talgalili/dendextend')
require2("Rcpp")
devtools::install_github('talgalili/dendextendRcpp')

# Having colorspace is also useful, since it is used
# In various examples in the vignettes
require2("colorspace")
```

And then you may load the package using:
```R
library(dendextend)
library(dendextendRcpp)
```

## Usage

Vignettes: 

* http://htmlpreview.github.io/?https://github.com/talgalili/dendextend/blob/master/inst/ignored/introduction.html
* https://github.com/talgalili/dendextend/blob/master/inst/doc/dendextend-tutorial.pdf  (older)

## Share your dendrograms!

If you have made interesting work using the dendextend package, I would LOVE to know about it. It can be a blog post, an academic paper, or just some plots you made for your work in the industry. Please contact me with what you have done, and I would also be happy to promote it in this page.

## Contact

You are welcome to:

* submit suggestions and bug-reports at: <https://github.com/talgalili/dendextend/issues>
* send a pull request on: <https://github.com/talgalili/dendextend/>
* compose a friendly e-mail to: <tal.galili@gmail.com>


## Latest news

You can see the most recent changes to the package in the NEWS.md file:

- https://github.com/talgalili/dendextend/blob/master/NEWS.md

