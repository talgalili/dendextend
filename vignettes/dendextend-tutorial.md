<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Doing more with dendrogram objects using dendextend}
-->

Doing more with dendrogram objects
==================================
A tutorial on using the **dendextend** package
================================================


Introduction
-------------

The "dendrogram" class provides general functions for handling tree-like structures in R. It is intended as a replacement for similar functions in hierarchical clustering and classification/regression trees, such that all of these can use the same engine for plotting or cutting trees.

However, many basic features are still missing from the dendrogram class.

### Motivation
The **dendextend** package aims at filling some gaps in base R, by extending the available functions for dendrogram manipulation, statistical analysis, and visualization.

This vignettes Provides a step-by-step description of the functionality provided by the **dendextend** package.


### Installation

To install the stable version from CRAN use:

```r
# install.packages('dendextend') # not yet available from CRAN
```

To install the GitHub version use:

```r
if (!require('devtools')) install.packages('devtools'); require('devtools')
install_github('dendextend', 'talgalili')
```











This is an example of Markdown vignettes in R. Before R 3.0.0, only Sweave/PDF vignettes were supported in R. Markdown is gaining popularity over the years due to its simplicity, and R 3.0.0 starts to support package vignettes written in [R Markdown](http://www.rstudio.com/ide/docs/authoring/using_markdown).

## Package vignettes

To enable Markdown vignettes in an R package, you need to

- add `*.Rmd` files under the `vignettes` directory
- add `VignetteBuilder: knitr` to the `DESCRIPTION` file
- specify the vignette engine `\VignetteEngine{knitr::knitr}` in the `Rmd` files (inside HTML comments)

## View vignettes

And R will load the **knitr** package to build these vignettes to HTML files, and you can see them when you open the HTML help:


```r
help(package = "YourPackage", help_type = "html")
```


## Examples

Below are some code chunks as examples.


```r
cat("_hello_ **markdown**!", "\n")
```

_hello_ **markdown**! 


Normally you do not need any chunk options.


```r
1 + 1
```

```
## [1] 2
```

```r
10:1
```

```
##  [1] 10  9  8  7  6  5  4  3  2  1
```

```r
rnorm(5)^2
```

```
## [1] 3.6492 0.9615 1.9829 0.4458 1.0560
```

```r
strsplit("hello, markdown vignettes", "")
```

```
## [[1]]
##  [1] "h" "e" "l" "l" "o" "," " " "m" "a" "r" "k" "d" "o" "w" "n" " " "v"
## [18] "i" "g" "n" "e" "t" "t" "e" "s"
```


Feel free to draw beautiful plots and write math $P(X>x)=\alpha/2$.


```r
n = 300
set.seed(123)
par(mar = c(4, 4, 0.1, 0.1))
plot(rnorm(n), rnorm(n), pch = 21, cex = 5 * runif(n), col = "white", bg = "gray")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


You can use your own CSS file instead of the built-in style in the **markdown** package -- just set the option `markdown.HTML.stylesheet`, e.g.


```r
options(markdown.HTML.stylesheet = "path/to/a/custom/style.css")
```


For this vignette, I use the `markdown.HTML.header` option to modify a few CSS rules in the default CSS style:


```r
options(markdown.HTML.header = system.file("misc", "vignette.css", package = "knitr"))
```

