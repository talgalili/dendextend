\name{dendextend_options}
\alias{dendextend_options}
\title{Access to dendextend_options}
\usage{
dendextend_options(option, value)
}
\value{
a list with functions
}
\description{
This is a function inside its own environment. This enables
a bunch of functions to be manipulated outside the package,
even when they are called from function within the
dendextend package.
}
\examples{
dendextend_options("a")
dendextend_options("a", 1)
dendextend_options("a")
dendextend_options("a", NULL)
dendextend_options("a")
dendextend_options()
}
\author{
Kurt Hornik
}

