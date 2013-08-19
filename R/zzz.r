# Copyright (C) Tal Galili
#
# This file is part of dendextend.
#
# dendextend is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# dendextend is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#




assign_dendextendRcpp_to_dendextend <- function() {
   # assigns the FASTER dendextendRcpp functions to override
   # the dendextend functions....
   
   if(suppressWarnings(require(dendextendRcpp))) {
      # This wouldn't work since it will only assign
      # the faster function in the current env      
#       get_branches_heights <- dendextendRcpp:::get_branches_heights
#       heights_per_k.dendrogram <- dendextendRcpp:::heights_per_k.dendrogram
      # for getting the functions "into" dendextend, we need to run this:
      
      # create a backup of these functions in order to later
      # compare them using benchmark (their kept invisible - but can be accessed)
      assign("old_heights_per_k.dendrogram", dendextend:::heights_per_k.dendrogram,
             envir=as.environment("package:dendextend"))
      assign("old_get_branches_heights", dendextend:::get_branches_heights,
             envir=as.environment("package:dendextend"))
      
      assignInNamespace(
         x= "heights_per_k.dendrogram",
         value = dendextendRcpp:::heights_per_k.dendrogram,
         ns = "dendextend"
      )
      assignInNamespace(
         x= "get_branches_heights",
         value = dendextendRcpp:::get_branches_heights,
         ns = "dendextend"
      )   
      
      ## p.s:
      # doing the following is a BAD IDEA!
      # This will not allow us to use labels.dendrogram when our Rcpp version fails...
      # assignInNamespace(
      #    x= "labels.dendrogram",
      #    value = dendextendRcpp:::labels.dendrogram,
      #    ns = "stats"
      #    )
      
      
      
   } else {
      warning("
         The 'dendextend' package runs 
         MUCH faster when you also have the dendextendRcpp package installed.
         Please consider running:
         install.packages('dendextendRcpp')
         and then re-load dendextend.
           ")
   }
   
}








.onLoad <- function(libname, pkgname){
   # Thanks for Romain: http://stackoverflow.com/questions/4369334/first-lib-idiom-in-r-packages
   
   # adding and removing menus from the Rgui when loading and detaching the library
   # setHook(packageEvent("installr", "attach"), {function(pkgname, libpath) {add.installr.GUI()}  } )
   # setHook(packageEvent("installr", "detach"), {function(pkgname, libpath) {remove.installr.GUI()}  } )
   
}

# menus are added and removed as needed: !!


.onAttach <- function(lib, pkg,...){
   ####
   ### I decided to not use the following code since it will 
   ###   always give a "masked" warning when loading 
   ###   dendextend. But it IS an issue...
   ####
   # if ape is installed on this computer, it will be loaded FIRST!   
   # This way I make sure to not have "unbranch" or "rotate" masked by {ape}
   #     (they would still work though)
   if("ape" %in% .packages(all.available = TRUE)) {       
      library("ape", pos = which(search() %in% "package:dendextend")+1, 
              warn.conflicts = FALSE,
              quietly = TRUE)
      #       search()
      #       unloadNamespace("ape")
      #       unloadNamespace("dendextend")
      #       require("dendextend", warn.conflicts = TRUE)                 
      #       require("dendextend", warn.conflicts = FALSE)                 
   }   
   packageStartupMessage(installrWelcomeMessage())  
   
   assign_dendextendRcpp_to_dendextend()
   
   
}




installrWelcomeMessage <- function(){
   
   paste("\n",     
         "Welcome to dendextend version ", utils:::packageDescription("dendextend")$Version, "\n",
         "\n",
         "Type ?dendextend to access the overall documentation and\n",
         "vignette('dendextend') for the package vignette.\n",
         "You can execute a demo of the package via: demo(dendextend)\n",
         "\n",  
         "More information is available on the dendextend project web-site:\n",
         "https://github.com/talgalili/dendextend/\n",
         "\n",               
         "Contact: <tal.galili@gmail.com>\n",
         "Suggestions and bug-reports can be submitted at: https://github.com/talgalili/dendextend/issues\n",
         "\n",
         "\t\t\tTo suppress the this message use:\n",
         "\t\t\tsuppressPackageStartupMessages(library(dendextend))\n",  
         sep="")
}





# using "zzz.r" like in devtools...

# When adding new files, make sure they are listed in DESCRIPTION:
# Collate:
#    'create.global.library.r'
# 'install.r'
# 'updateR.r'


# IMPORTANT NOTICE: this will add Collate to the DESCRIPTION file, and if any new r file is added - it will need to be updated.
# Collate:
# +    'create.global.library.r'
# +    'install.r'
# +    'updateR.r'
# +    'zzz.r'

# when a function is renamed, its document in man must be removed - otherwise it may cause problems with the built check (it will try to run the code in the example, and will fail.)
# When all is done, run:
# require(devtools)
# check()
# check(args="--as-cran")
#                 Thanks to: http://stackoverflow.com/questions/10017702/r-cmd-check-options-for-more-rigorous-testing-2-15-0
# build_win()
# release()
