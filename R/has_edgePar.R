

#' Does a dendrogram has an edgePar/nodePar component?
#' @export
#' @aliases
#' has_edgePar
#' has_nodePar
#'
#' @param dend a \link{dendrogram} object.
#' @param component a character value to be checked if exists in the tree. For edgePar the list: "col", "lty" and "lwd" (for the segments),
#' "p.col", "p.lwd", and "p.lty" (for the polygon around the text) and "t.col" for the text color.
#' For edgePar "pch", "cex", "col", "xpd", and/or "bg".
#' @param the_attrib A character of the attribute for which to check the existence of the component.
#' Often either "edgePar" or "nodePar".
#' @param ... ignored
#'
#' @seealso \link{get_nodes_attr}, \link{set}
#' @return
#' Logical. TRUE if such a component is defined somewhere in the tree, FALSE otherwise.
#' If dend is not a dendrogram, the function will return FALSE.
#'
#' @examples
#'
#' dat <- iris[1:20, -5]
#' hca <- hclust(dist(dat))
#' hca2 <- hclust(dist(dat), method = "single")
#' dend <- as.dendrogram(hca)
#' dend2 <- as.dendrogram(hca2)
#'
#' dend %>%
#'   set("branches_lwd", 2) %>%
#'   set("branches_lty", 2) %>%
#'   plot()
#' dend %>%
#'   set("branches_lwd", 2) %>%
#'   set("branches_lty", 2) %>%
#'   has_edgePar("lty")
#' dend %>%
#'   set("branches_lwd", 2) %>%
#'   has_edgePar("lty")
#' dend %>%
#'   set("branches_lwd", 2) %>%
#'   has_edgePar("lwd")
#'
#' dend %>%
#'   set("branches_lwd", 2) %>%
#'   set("clear_branches") %>%
#'   has_edgePar("lwd")
has_component_in_attribute <- function(dend, component, the_attrib = "edgePar", ...) {
  if (!is.dendrogram(dend)) {
    return(FALSE)
  }

  list_of_names <- dend %>%
    get_nodes_attr(the_attrib, simplify = FALSE) %>%
    lapply(names)

  has_component_fun <- function(node_names) any(node_names %in% component)
  has_component <- list_of_names %>%
    lapply(has_component_fun) %>%
    unlist() %>%
    any()
  has_component
}

#' @export
has_edgePar <- function(dend, component, the_attrib = "edgePar", ...) {
  has_component_in_attribute(dend, component, the_attrib, ...)
}

#' @export
has_nodePar <- function(dend, component, the_attrib = "nodePar", ...) {
  has_component_in_attribute(dend, component, the_attrib, ...)
}
