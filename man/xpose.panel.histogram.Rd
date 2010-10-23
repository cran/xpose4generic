% Xpose 4
% An R-based population pharmacokinetic/
% pharmacodynamic model building aid for NONMEM.
% Copyright (C) 1998-2004 E. Niclas Jonsson and Mats Karlsson.
% Copyright (C) 2005-2008 Andrew C. Hooker, Justin J. Wilkins, 
% Mats O. Karlsson and E. Niclas Jonsson.
% Copyright (C) 2009-2010 Andrew C. Hooker, Mats O. Karlsson and 
% E. Niclas Jonsson.

% This file is a part of Xpose 4.
% Xpose 4 is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public License
% as published by the Free Software Foundation, either version 3
% of the License, or (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public License
% along with this program.  A copy can be cound in the R installation
% directory under \share\licenses. If not, see http://www.gnu.org/licenses/.

\name{xpose.panel.histogram}
\alias{xpose.panel.histogram}
\title{Default histogram panel function for Xpose 4}
\description{
  This is the histogram panel function for Xpose 4. This is not intended to be
  ised outside the \code{xpose.plot.histogram} function. Most of the
  arguments take their default values from xpose.data object but this
  can be overridden by supplying them as argument to
  \code{xpose.plot.histogram}. 
}
\usage{
xpose.panel.histogram(x, object,%data,
                      %subscripts,
                      %inclZeroWRES = FALSE,
                      %onlyfirst = FALSE,
                      %samp = NULL,
                      %xvarnam = NULL,
                      breaks = NULL,
                      hidlty = object@Prefs@Graph.prefs$hidlty, 
                      hidcol = object@Prefs@Graph.prefs$hidcol, 
                      hidlwd = object@Prefs@Graph.prefs$hidlwd, 
                      hiborder = object@Prefs@Graph.prefs$hiborder, 
                      hilty = object@Prefs@Graph.prefs$hilty, 
                      hicol = object@Prefs@Graph.prefs$hicol,
                      hilwd = object@Prefs@Graph.prefs$hilwd,
                      math.dens = NULL,
                      vline= NULL,
                      vllwd= 3,
                      vllty= 2,
                      vlcol= "grey",
                      hline= NULL,
                      hllwd= 3,
                      hllty= 1,
                      hlcol= "grey",
                      ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Name(s) of the x-variable.}
  \item{object}{An xpose.data object. }
%  \item{data}{Data to use in plot.}
%  \item{subscripts}{The standard Trellis subscripts argument (see
%    \code{\link[lattice]{xyplot}})} 
%  \item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
%    is included in the plot.}
%  \item{onlyfirst}{Logical value indicating whether only the first row
%    per individual is included in the plot. }
%  \item{samp}{An integer between 1 and object@Nsim
%    (see\code{\link[xpose4data]{xpose.data-class}}) specifying which of the
%    simulated data sets to extract from SData.}
%  \item{xvarnam}{Character string with the name of the x-variable. }
  \item{breaks}{The breakpoints for the histogram.}
  \item{hidlty}{Density line type.}
  \item{hidcol}{Color of density line.}
  \item{hidlwd}{Width of density line.}
  \item{hiborder}{Colour of the bar borders.}
  \item{hilty}{Line type for the bar borders.}
  \item{hicol}{Fill colour for the bars.}
  \item{hilwd}{Width for the bar borders.}
  \item{math.dens}{Should a density line be drawn.  Values are
    \code{NULL} or \code{TRUE}.}
  \item{vline}{\code{NULL} or a vector of locations for the vertical
    lines to be drawn.  For example, \code{vline=c(50,60)} will draw two
    vertical lines. The function
    \code{\link[lattice:panel.functions]{panel.abline}} is used.}   
  \item{vllwd}{Line width of the vertical lines defined with
    \code{vline}. Can be a vector or a single value, for example \code{vllwd=2}
    or \code{vllwd=c(2,3)}.}
  \item{vllty}{Line type of the vertical lines defined with
    \code{vline}. Can be a vector or a single value, for example \code{vllty=1}
    or \code{vllty=c(1,2)}.}
  \item{vlcol}{Line color of the vertical lines defined with
    \code{vline}. Can be a vector or a single value, for example \code{vlcol="red"}
    or \code{vllty=c("red","blue")}.}
  \item{hline}{\code{NULL} or a vector of locations for the horizontal
    lines to be drawn.  For example, \code{hline=c(50,60)} will draw two horizontal
    lines. The function
    \code{\link[lattice:panel.functions]{panel.abline}} is used.} 
  \item{hllwd}{Line width of the horizontal lines defined with
    \code{hline}. Can be a vector or a single value, for example \code{hllwd=2}
    or \code{hllwd=c(2,3)}.}
  \item{hllty}{Line type of the horizontal lines defined with
    \code{hline}. Can be a vector or a single value, for example \code{hllty=1}
    or \code{hllty=c(1,2)}.}
  \item{hlcol}{Line color of the horizontal lines defined with
    \code{hline}. Can be a vector or a single value, for example \code{hlcol="red"}
    or \code{hllty=c("red","blue")}.}  
  \item{\dots}{Other arguments that may be needed in the function. }
}
\details{
}
\value{
}
\author{Andrew Hooker, Mats Karlsson, Justin Wilkins & E. Niclas Jonsson}
\seealso{\code{\link[xpose4data]{xpose.data-class}}, Cross-references above.}
\examples{

}
\keyword{methods}
