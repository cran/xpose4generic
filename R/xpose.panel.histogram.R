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

xpose.panel.histogram <- function(x,
                                  object,
                                  ##data,
                                  ##subscripts,
                                  ##inclZeroWRES = FALSE,
                                  ##onlyfirst = FALSE,
                                  ##samp = NULL,
                                  ##xvarnam=NULL,
                                  breaks=NULL,
                                  hidlty = object@Prefs@Graph.prefs$hidlty, 
                                  hidcol = object@Prefs@Graph.prefs$hidcol, 
                                  hidlwd = object@Prefs@Graph.prefs$hidlwd, 
                                  hiborder = object@Prefs@Graph.prefs$hiborder, 
                                  hilty = object@Prefs@Graph.prefs$hilty, 
                                  hicol = object@Prefs@Graph.prefs$hicol, 
                                  hilwd = object@Prefs@Graph.prefs$hilwd,
                                  math.dens=NULL,
                                  
                                  ## vline settings
                                  vline= NULL,#object@Prefs@Graph.prefs$abline,
                                  vllwd= 3,#object@Prefs@Graph.prefs$abllwd,
                                  vllty= 2,#object@Prefs@Graph.prefs$abllty,
                                  vlcol= "grey",#object@Prefs@Graph.prefs$ablcol,

                                  ## hline settings
                                  hline= NULL,#object@Prefs@Graph.prefs$abline,
                                  hllwd= 3,#object@Prefs@Graph.prefs$abllwd,
                                  hllty= 1,#object@Prefs@Graph.prefs$abllty,
                                  hlcol= "grey",#object@Prefs@Graph.prefs$ablcol,
                                  ...) {
  
  ## if(!is.null(samp)) {
##     data <- SData(object,inclZeroWRES,onlyfirst=onlyfirst,samp=samp)
##   } else {
##     data <- Data(object,inclZeroWRES,onlyfirst=onlyfirst)
##   }
  if(length(unique(x)) <= object@Prefs@Cat.levels){
    x <- as.factor(x)
  }

  if(is.factor(x)) {
    nint   <- length(levels(x))
    breaks <- seq(0.5, length = length(levels(x))+1)
  } else {
    nint      <- round(log2(length(x))+1)
    endpoints <- range(x[!is.na(x)])
    #breaks <- do.breaks(endpoints, nint)
  }

  panel.histogram(x,
                  breaks=breaks,
                  lty = hilty,
                  lwd = hilwd,
                  col = hicol,
                  border = hiborder,
                  #type = "density",
                  ...
                  )

  if (is.numeric(x)) {## this should be a choice to plot this not required
    panel.densityplot(x,
                      #breaks=breaks,
                      lty=hidlty,
                      col=hidcol,
                      lwd=hidlwd,
                      ...)
    
    ## dens <- density(x)
##     panel.xyplot(dens$x, dens$y,
##                  type="l",
##                  lty=hidlty,
##                  col=hidcol,
##                  lwd=hidlwd,
##                  ...
##                  )
  }

  if (!is.null(math.dens)){
    panel.mathdensity(dmath = dnorm,
                      args = list(mean=math.dens$mean,sd=math.dens$sd),
                      col.line="black",lwd=3,
                      ...)
  }

  ## vertical Line in histogram
  if(!is.null(vline)) {
    panel.abline(v=vline,col=vlcol,lwd=vllwd,lty=vllty)
  }

  ## Horizontal Line in histogram
  if(!is.null(hline)) {
    panel.abline(h=hline,col=hlcol,lwd=hllwd,lty=hllty)
  }
  
}

  
