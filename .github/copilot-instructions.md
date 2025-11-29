# Copilot Code Review Instructions

This file provides instructions for GitHub Copilot when reviewing code in the dendextend R package.

## About This Package

dendextend is an R package that extends the functionality of dendrogram objects in R. It provides functions for visualizing and comparing trees of hierarchical clusterings, including adjusting graphical parameters and statistically comparing different dendrograms.

## Code Review Guidelines

When reviewing code in this repository, please consider:

### R Code Standards
- Follow tidyverse style guide conventions
- Ensure functions are properly documented with roxygen2 comments
- Check for proper use of R's dendrogram and hclust objects
- Verify that exported functions have complete documentation including `@param`, `@return`, and `@examples` sections

### Package Development
- Changes to R code should maintain backward compatibility
- New functions should include appropriate unit tests in the `tests/` directory
- Updates to exported functions require corresponding updates to NAMESPACE via roxygen2

### Dependencies
- Minimize adding new package dependencies
- Prefer packages listed in Suggests over Imports when possible
- Ensure any new dependencies are compatible with R >= 3.0.0

### Testing
- Tests should use the testthat framework
- Ensure tests cover edge cases for dendrogram manipulation functions
- Test functions with both small and moderately sized dendrograms

### Documentation
- Vignettes should be kept up-to-date with new features
- NEWS.md should be updated for user-facing changes
- README.md should reflect current package capabilities
