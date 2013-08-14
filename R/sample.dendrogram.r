



#' @export
sample.dendrogram <- function(dend, replace = FALSE, dend_labels, fix_members=TRUE, fix_order=TRUE, ...) {
   if(missing(dend_labels)) dend_labels <- labels(dend)
   
   if(replace) {
      s_dend_labels <- sample(dend_labels, replace=TRUE)
      
      # 1) trim redundent leaves
      ss_kept_labels <- dend_labels %in% s_dend_labels
      ss_removed_labels <- !ss_kept_labels
      removed_labels <- dend_labels[ss_removed_labels]
      dend <- trim(dend, leaves=removed_labels)
      
      # 2) add new leaves
      
      # 3) Fix members
      if(fix_members) dend <- fix_members_attr.dendrogram(dend)
      
      # 4) Fix leaves order values
      if(fix_order) dend <- rank_order.dendrogram(dend)
      
      
      
   } else { # don't replace
      n_dend <- nleaves(dend)
      new_order <- sample(n_dend)
      labels(dend) <- dend_labels[new_order]      
      order.dendrogram(dend) <- order.dendrogram(dend)[new_order]      
   }
   
   return(dend)
}


