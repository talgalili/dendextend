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

# An internal function to create rainbows:

rainbow_fun <- function(n, c = 90, l = 50, ...) {
  if (requireNamespace("colorspace")) {
    colorspace::rainbow_hcl(n, c = c, l = l, ...)
  } else {
    rainbow(n, ...)
  }
}

# n = 10
# barplot(rep(10, n), col = rainbow_hcl(n) )
# barplot(rep(10, n), col = rainbow_hcl(n, c=90, l=50) )
