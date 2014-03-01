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

# 
# 
# 
# assign_dendextendRcpp_to_dendextend <- function() {
#    # assigns the FASTER dendextendRcpp functions to override
#    # the dendextend functions....
#    
#    if(suppressWarnings(require(dendextendRcpp))) {
#       # This wouldn't work since it will only assign
#       # the faster function in the current env      
# #       get_branches_heights <- dendextendRcpp::get_branches_heights
# #       heights_per_k.dendrogram <- dendextendRcpp::heights_per_k.dendrogram
#       # for getting the functions "into" dendextend, we need to run this:
#       
#       # create a backup of these functions in order to later
#       # compare them using benchmark (their kept invisible - but can be accessed)
#       assign("old_get_branches_heights", dendextend:::get_branches_heights,
#              envir=as.environment("package:dendextend"))
#       assign("old_heights_per_k.dendrogram", dendextend:::heights_per_k.dendrogram,
#              envir=as.environment("package:dendextend"))
#       assign("old_cut_lower_fun", dendextend:::cut_lower_fun,
#              envir=as.environment("package:dendextend"))
#       
# 
# 	  # require(utils) # doesn't help really...
# 	  # but this does: (!)
# 		# http://stackoverflow.com/questions/13595145/overriding-a-package-function-inherited-by-another-package
# # 	  get("assignInNamespace", envir=asNamespace("utils"))
# 	  # Using only "::" instead of ":::" will crash many tests...
# 	  
#       assignInNamespace(
#          x= "get_branches_heights",
#          value = dendextendRcpp:::get_branches_heights,
#          ns = "dendextend"
#       )   
#       assignInNamespace(
#          x= "heights_per_k.dendrogram",
#          value = dendextendRcpp:::heights_per_k.dendrogram,
#          ns = "dendextend"
#       )
#       assignInNamespace(
#          x= "cut_lower_fun",
#          value = dendextendRcpp:::cut_lower_fun,
#          ns = "dendextend"
#       )
#       
#       ## p.s:
#       # doing the following is a BAD IDEA!
#       # This will not allow us to use labels.dendrogram when our Rcpp version fails...
#       # assignInNamespace(
#       #    x= "labels.dendrogram",
#       #    value = dendextendRcpp:::labels.dendrogram,
#       #    ns = "stats"
#       #    )
#       
#       
#       
#    } else {
#       warning("
#          The 'dendextend' package runs 
#          MUCH faster when you also have the dendextendRcpp package installed.
#          Please consider running:
#          install.packages('dendextendRcpp')
#          and then re-load dendextend.
#            ")
#    }
#    
# }
# 




assign_dendextend_options <- function() { 
   # assigns the functions which could later be replaced by the FASTER dendextendRcpp functions 
   options(dendextend_get_branches_heights = dendextend::dendextend_get_branches_heights)
   options(dendextend_heights_per_k.dendrogram = dendextend::dendextend_heights_per_k.dendrogram)
   options(dendextend_cut_lower_fun = dendextend::dendextend_cut_lower_fun)
}

remove_dendextend_options <- function() { 
   # assigns the functions which could later be replaced by the FASTER dendextendRcpp functions 
   options(dendextend_get_branches_heights = NULL)
   options(dendextend_heights_per_k.dendrogram = NULL)
   options(dendextend_cut_lower_fun = NULL)
}





.onLoad <- function(libname, pkgname){
   # Thanks for Romain: http://stackoverflow.com/questions/4369334/first-lib-idiom-in-r-packages
   
   # adding and removing menus from the Rgui when loading and detaching the library
   # setHook(packageEvent("installr", "attach"), {function(pkgname, libpath) {add.installr.GUI()}  } )
   setHook(packageEvent("dendextend", "detach"), {function(pkgname, libpath) {remove_dendextend_options()}  } )
}

# menus are added and removed as needed: !!


.onAttach <- function(lib, pkg,...){
   ####
   ### I decided to not use the following code since it will 
   ###   always give a "masked" warning when loading 
   ###   dendextend. But it IS an issue...
   ####
   # if ape is installed on this computer, it will be loaded FIRST!   
   # This way I make sure to not have "rotate" masked by {ape}
   #     (they would still work though)

   # this would solve it, but it is not a "nice" way to do it...
#    if("ape" %in% .packages(all.available = TRUE)) {       
#       library("ape", pos = which(search() %in% "package:dendextend")+1, 
#               warn.conflicts = FALSE,
#               quietly = TRUE)
#    }      
   
   # The above line causes problems such as: 'library' or 'require' call not declared from: 'ape'
   
      #       search()
      #       unloadNamespace(ape")
      #       unloadNamespace("dendextend")
      #       require("dendextend", warn.conflicts = TRUE)                 
      #       require("dendextend", warn.conflicts = FALSE)                 
   # but it makes sure that "dendextend" does not "Depends" on ape"...
   
   # move some functions to the "options" so that they would later be overridden.
   assign_dendextend_options()
   
   packageStartupMessage(dendextendWelcomeMessage())  
   
#    assign_dendextendRcpp_to_dendextend()
   
   
}




dendextendWelcomeMessage <- function(){
   require(utils)   
   
   paste("\n",     
         "Welcome to dendextend version ", utils::packageDescription("dendextend")$Version, "\n",
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




# require(dendextend)
# environmentIsLocked(as.environment("package:dendextend"))
# lockEnvironment(env=as.environment("package:dendextend"), bindings = FALSE)
# lockEnvironment(env=as.environment("package:dendextend"), bindings = TRUE)


# IMPORTANT NOTICE: this will add Collate to the DESCRIPTION file, and if any new r file is added - it will need to be updated.
# Collate:
# +    'create.global.library.r'
# +    'install.r'
# +    'updateR.r'
# +    'zzz.r'


# Steps:
# http://r.789695.n4.nabble.com/vignettes-problems-with-PDF-compaction-td4664909.html
# 1) install gs - http://www.ghostscript.com/download/gsdnld.html
# 2) find where it is, and update R_GSCMD:
# Sys.setenv(R_GSCMD="C:\\Program Files\\gs\\gs9.10\\bin\\gswin64c.exe")
# Sys.getenv("R_GSCMD")
# 3) Check that it works: 
# system2(Sys.getenv("R_GSCMD"), args="--version") 
# 4) use:
#### tools::compactPDF("inst\\doc\\dendextend-tutorial.pdf", gs_quality="ebook") 
# tools::compactPDF("inst\\doc\\dendextend-tutorial.pdf") 
###   compacted ‘dendextend-tutorial.pdf’ from 961Kb to 737Kb

# For checking:
# 1) get qpdf
#     http://sourceforge.net/projects/qpdf/files/
# 2) put it somewhere
# 3) set R_QPDF
#  Sys.setenv(R_QPDF="C:\\Rtools\\qpdf-5.1.1\\bin\\qpdf.exe")
#  Sys.which(Sys.getenv("R_QPDF", "qpdf"))
   
# Also, make sure to add:
# options(repos=c("http://cran.rstudio.com", "http://www.stats.ox.ac.uk/pub/RWin" ))
# to D:\R\R-devel\etc\Rprofile.site

# when a function is renamed, its document in man must be removed - otherwise it may cause problems with the built check (it will try to run the code in the example, and will fail.)
# When all is done, run:
# require(devtools)
# check()
# browseURL(tempdir())
### http://www.rstudio.com/ide/docs/packages/build_options
# check(build_args="--no-build-vignettes --no-manual", args = "--no-examples --no-build-vignettes --no-manual",  cran = FALSE, cleanup = FALSE)
# check(build_args="--no-build-vignettes ", args = "--no-build-vignettes",  cran = FALSE, cleanup = FALSE)
# check(args="--as-cran")
# check("D:/Dropbox/aaaa good R code/AA - My packages/dendextend", args="--as-cran")
#                 Thanks to: http://stackoverflow.com/questions/10017702/r-cmd-check-options-for-more-rigorous-testing-2-15-0
# file.copy("NEWS", "NEWS.md")
# build_win(version="R-devel")
# release()
