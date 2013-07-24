
.onLoad <- function(libname, pkgname){
   # Thanks for Romain: http://stackoverflow.com/questions/4369334/first-lib-idiom-in-r-packages
   
   # adding and removing menus from the Rgui when loading and detaching the library
   # setHook(packageEvent("installr", "attach"), {function(pkgname, libpath) {add.installr.GUI()}  } )
   # setHook(packageEvent("installr", "detach"), {function(pkgname, libpath) {remove.installr.GUI()}  } )
   
}

# menus are added and removed as needed: !!


.onAttach <- function(lib, pkg,...){
   packageStartupMessage(installrWelcomeMessage())
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
