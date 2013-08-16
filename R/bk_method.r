###
#
# Bk-method.r

# require(R2PPT)



##example:
# mydata <- USArrests
##Ward Hierarchical Clustering
# d <- dist(mydata, method = "euclidean") # distance matrix
# fit1 <- hclust(d, method="ward") 
# fit2 <- hclust(d, method="single") 
##fit <- fit1
# par(mfrow = c(1,2))
# plot(fit1) # display dendogram
# plot(fit2) # display dendogram

if(F) {
   system.time(Bk(dendo1, dendo2)) 
   # Bk = 0.7832726 0.4284998 0.0007704066
   # times in sec after modifications:
   # 0.51
   # 0.68 
}





Bk <- function(A1, A2, k = 3, print.output = F, sanity_checks = FALSE, ...)
{
   if(sanity_checks) {	# the sanity checks are turned off by default since the "labels" function for dendrogram is one which takes some time to run...
      # notice that we must have labels.hclust and labels.dendrogram defined!
      A1_labels <- labels(A1)
      A2_labels <- labels(A2)
      length_A1_labels <- length(A1_labels)
      length_A2_labels <- length(A2_labels)	
      
      # Checking for common error options:
      if(length_A1_labels != length_A2_labels) stop("The two clusters don't have the same number of items!")	# If cluster sized are different - stop
      if(!all(sort(A1_labels) == sort(A2_labels))) stop("Your trees are having leaves with different names - please correct it inorder to use this function")
   }
   
   # calculate n
   if(is.hclust(A1)) n <- length(A1$labels)  # this may be done faster if we know this is a dendrogram
   if(is.dendrogram(A1)) n <- length(labels(A1))  # this may be done faster if we know this is a dendrogram
   
   # creating matrix M
   # ----------
   # a not-so-smart way
   # M <- matrix(0, nrow = k, ncol = k)
   # for(i in 1:k)
   # {
   # for(j in 1:k)
   # {
   # M[i,j] <- sum(rect.hclust(A1, k = k)[[i]] %in% rect.hclust(A2, k = k)[[j]])
   # how many of the objects in cluser i in tree A1, exist in cluster j in tree A2
   # }
   # }
   # ----------
   # a much better way!
   
   A1.clusters <- cutree(A1, k = k)
   A2.clusters <- cutree(A2, k = k)
   
   # If one of the trees can not be cut, we should return the data.frame with missing values (so to not stop the entire function...)
   if(is.null(A1.clusters) || is.null(A2.clusters)) return(data.frame(Bk = NA, E.Bk= NA, Var.Bk= NA) )
   
   # sort them to look the same (we assume the *same naming* for the leaves !
   A1.clusters <- A1.clusters[order(names(A1.clusters))]	# order the vec accourding to the names, so to allow a comparison
   A2.clusters <- A2.clusters[order(names(A2.clusters))]	# order the vec accourding to the names, so to allow a comparison
   if(print.output) print(list(A1.clusters, A2.clusters )	)
   M <- table(A1.clusters, A2.clusters)
   if(print.output) print(M)
   
   
   Tk <- sum(M^2) - n
   m_i. <- apply(M, 1, sum)
   m_.j <- apply(M, 2, sum)
   m_.. <- n # sum(M)
   if(sum(M) != n ) stop("Why does M matrix doesn't sum up to n ??")
   Pk <- sum(m_i.^2) - n
   Qk <- sum(m_.j^2) - n
   
   Bk <- Tk / sqrt(Pk*Qk)
   
   E.Bk <- sqrt(Pk*Qk)/ (n*(n-1))
   
   
   Pk2 <- sum( m_i. * (m_i. -1) * (m_i. -2) )
   Qk2 <- sum( m_.j * (m_.j -1) * (m_.j -2) )
   Var.Bk <- 2/ (n*(n-1))  +  
      4*Pk2 * Qk2 / ( (n*(n-1)*(n-2)) * Pk*Qk ) + 
      (Pk -2 - 4*Pk2/Pk) * (Qk -2 - 4*Qk2/Qk) / ( (n*(n-1)*(n-2)*(n-3))) -
      Pk * Qk / (n^2*(n-1)^2)
   
   return(data.frame(Bk, E.Bk, Var.Bk) )
}

# Bk(fit1, fit2, k = 2, T)
# Bk(as.dendrogram(fit1), as.dendrogram(fit2), k = 2, T)

# Bk(fit1, fit2, k = 2, F)
# Bk(as.dendrogram(fit1), as.dendrogram(fit2), k = 5, F)


if(F) {
   # The Bk function was previously also implemented by Matt in:
   #		 http://cran.r-project.org/web/packages/profdpm/index.html
   # See pages 9 and 10 here: http://cran.r-project.org/web/packages/profdpm/vignettes/profdpm.pdf
   # I came by this package thanks to chl: http://stats.stackexchange.com/questions/3672/a-measure-to-describe-the-distribution-of-a-dendrogram	
   # Also, there is a great overview of similarity measures on this here:
   # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.164.6189&rep=rep1&type=pdf
   
   our_dist <- dist(USArrests[1:10,])
   dend1 <- hclust(our_dist , "complete")
   dend2 <- hclust(our_dist , "single")
   plot(dend1)
   plot(as.dendrogram(dend1))
   our_dist
   
   
   # install.packages("profdpm")
   require("profdpm")
   # see page 9 and 10:
   pci(cutree(dend1 , k= 4),cutree(dend2 , k= 4))[2]
   Bk(dend1 , dend2, k = 4)
   # I get the same results - yay!
   
   system.time(pci(cutree(dend1 , k= 4),cutree(dend2 , k= 4)))
   system.time(Bk(dend1 , dend2, k = 4))
   
   
   
}

Bk_range <- function(A1, A2, ks = NULL, to.print = F)
{
   #ks is the k range
   
   # n <- length(A1$order)	# this would ONLY work for hclust...
   n <- length(leaves.values(A1))	# this will also work for both hclust and denderogram (assuming the proper methods were defined...)
   if(is.null(ks)) ks <- 2:(n-1)
   Bks <- NULL # rep(NA, length(ks))
   
   # counter <- 1
   
   for(the.k in ks)
   {
      #		Bks[[counter]] <- Bk(A1, A2, k = the.k)
      Bks <- rbind(Bks, Bk(A1, A2, k = the.k))
      # counter <- 	counter + 1
      if(to.print) print(paste("We just cut using k:",the.k))
   }	
   return(data.frame(ks ,	Bks))
}

# Bk_range(fit1, fit2, c(2:6))

plot_Bks_with_E_Bk <- function(Bk_data_frame, CI_sd_times = 2, plot_mean_Bk_under_H0 = TRUE, ...)	
{
   # CI_sd_times == how many times to multiply the Sd(Bk) - to get the cI
   # the three dots ... are for the plot
   if(sum(is.na(Bk_data_frame))>1) warning("NA's exists in the Bk table - rows with NA were ommited")
   
   aa <- na.omit(Bk_data_frame)	# the na.omit fixes issues with missing values (it deletes the entire row)
   with(aa, plot(Bk ~ ks, ylim = c(0,1), type = "b" ,...))
   if(plot_mean_Bk_under_H0 )
   {
      with(aa, points(E.Bk ~ ks, type = "l" , lwd = 2))
      with(aa, points((E.Bk +  CI_sd_times*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2))
      with(aa, points((E.Bk -  CI_sd_times*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2))
   }
}


plot_Bks_with_E_Bk.and.bakers.gamma <- function(the_bk_object ,the_bakers_gamma, main = "")
{
   plot_Bks_with_E_Bk(the_bk_object)
   
   title(main = main)
   legend("topright", legend = paste("Baker's gamma (P_value):  ",
                                     round(the_bakers_gamma$estimate, 3)," (",
                                     round(the_bakers_gamma$p.value, 5),")", sep = "")
   )
}


# plot_Bks_with_E_Bk(Bk_range(fit1, fit2))	


plot_Bks_with_E_Bk.BH.adjusted <- function(Bk_data_frame, CI_sd_times = 2, ...)	
{
   # CI_sd_times == how many times to multiply the Sd(Bk) - to get the cI
   # the three dots ... are for the plot
   aa <- Bk_data_frame
   
   Bk.Ps <- with(aa, 1-pnorm(((Bk - E.Bk)/sqrt(Var.Bk)) ))
   with(aa, 1-pnorm(((Bk - E.Bk)/sqrt(Var.Bk)) ))
   with(aa, 1-pnorm(Bk , E.Bk,sqrt(Var.Bk)))
   aa$Bk
   adjusted.Bk.Ps <- p.adjust(Bk.Ps, method = "BH")
   adjusted.Bk <- with(aa, (qnorm(1-adjusted.Bk.Ps) + E.Bk)*sqrt(Var.Bk))	
   ss <- adjusted.Bk == Inf
   adjusted.Bk[ss] <- aa$Bk[ss] # fill in Inf values with original Bk 
   
   
   with(aa, plot(Bk ~ ks, ylim = c(0,1), type = "b" ,...))
   with(aa, points(adjusted.Bk ~ ks, col = "orange", pch = 19, type = "b"))
   with(aa, points(E.Bk ~ ks, type = "l" , lwd = 2))
   with(aa, points((E.Bk +  CI_sd_times*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2))
   with(aa, points((E.Bk -  CI_sd_times*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2))
}


plot_Bks_with_E_Bk.holm.adjusted.CI <- function(Bk_data_frame, CI_sd_times = 2, ...)	
{
   # CI_sd_times == how many times to multiply the Sd(Bk) - to get the cI
   # the three dots ... are for the plot
   aa <- Bk_data_frame
   
   # let's assume we wish to keep alfa = .05/2
   CI_sd_times 
   adj.Ps <- (.05/2) / length(aa$Bk):1
   ordered.adj.Ps <- adj.Ps[order(aa$Bk)]
   CI_sd_times <- qnorm(1-ordered.adj.Ps)	# the new  CI_sd_times - adjusted	
   
   with(aa, plot(Bk ~ ks, ylim = c(0,1), type = "b" ,...))	
   with(aa, points(E.Bk ~ ks, type = "l" , lwd = 2))
   with(aa, points((E.Bk +  CI_sd_times*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2, col = "green"))
   with(aa, points((E.Bk -  CI_sd_times*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2, col = "green"))
   
   with(aa, points((E.Bk +  qnorm(.975)*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2, col = 1))
   with(aa, points((E.Bk -  qnorm(.975)*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2, col = 1))	
   
   bonf.CI_sd_times <- qnorm(1-((.05/2) / length(aa$Bk)))	
   with(aa, points((E.Bk +  bonf.CI_sd_times*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2, col = "red"))
   with(aa, points((E.Bk -  bonf.CI_sd_times*sqrt(Var.Bk))  ~ ks, type = "l" , lwd = 1, lty = 2, col = "red"))
   
   legend("topright", fill = c("black", "red","green"), legend = c("no correction - +-1.96", "Bonferroni correction CI", "Holm's adjusted CI"))
}





# calculate
# aa <- Bk_range(fit1, fit2)
# plot_Bks_with_E_Bk(aa)

# str(as.dendrogram(fit1))
# aa <- Bk_range(as.dendrogram(fit1), as.dendrogram(fit2))
# Bk(as.dendrogram(fit1), as.dendrogram(fit2), k = 2,T)

# plot_Bks_with_E_Bk(aa)

# unlist(as.dendrogram(fit1))
# labels(as.dendrogram(fit1))
