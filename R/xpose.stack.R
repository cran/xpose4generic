# Xpose 4
# An R-based population pharmacokinetic/
# pharmacodynamic model building aid for NONMEM.
# Copyright (C) 1998-2004 E. Niclas Jonsson and Mats Karlsson.
# Copyright (C) 2005-2008 Andrew C. Hooker, Justin J. Wilkins, 
# Mats O. Karlsson and E. Niclas Jonsson.
# Copyright (C) 2009-2010 Andrew C. Hooker, Mats O. Karlsson and 
# E. Niclas Jonsson.

# This file is a part of Xpose 4.
# Xpose 4 is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with this program.  A copy can be cound in the R installation
# directory under \share\licenses. If not, see http://www.gnu.org/licenses/.

##stack.xpose <- function (object, select,rep,...)  {
xpose.stack <- function (data, object, select,rep,...)  {

  x  <- data
  xx <- data
  
  nl        <- as.list(1:ncol(x))
  names(nl) <- names(x)
  vars      <- eval(substitute(select), nl, parent.frame())
  x         <- x[, vars, drop = FALSE]
  facnams   <- xlabel(names(x),object)
  names(x)  <- facnams

  tmp <- data.frame(values = unlist(unname(x)),
                    ind = factor(rep.int(names(x),lapply(x, length)),
                      levels=names(x))
                    )
  labs <- c()
  labs["values"] <- paste(select,sep="/",collapse="/")
  labs["ind"]    <- "ind"


  if(!missing(rep)) {
    for(yy in rep) {
 #     labs[yy] <- xlabel(yy,object)
      tmp[,yy] <- rep(xx[,yy],length(select))
    }
  }
  ## xpobj       <- new("xpose")
##   xpobj@Data  <- tmp
##   xpobj@Prefs@Labels <- as.list(labs)
##   xpobj@Runno  <- object@Runno
##   return(xpobj)
  return(tmp)
}
