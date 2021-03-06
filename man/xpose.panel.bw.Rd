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

\name{xpose.panel.bw}
\alias{xpose.panel.bw}
\title{Default box-and-whisker panel function for Xpose 4}
\description{
  This is the box-and-whisker panel function for Xpose 4. This is not 
  intended to be used outside the \code{xpose.plot.bw} function. 
  Most of the arguments take their default values from xpose.data object but this
  can be overridden by supplying them as arguments to \code{xpose.plot.bw}.
}
\usage{
xpose.panel.bw(x, y, object,
           subscripts,
           groups = NULL,
           inclZeroWRES = FALSE,
           onlyfirst = FALSE,
           samp = NULL,
           xvarnam = NULL,
           yvarnam = NULL,
           type = object@Prefs@Graph.prefs$type,
           col = object@Prefs@Graph.prefs$col,
           pch = object@Prefs@Graph.prefs$pch,
           cex = object@Prefs@Graph.prefs$cex,
           lty = object@Prefs@Graph.prefs$lty,
           fill = object@Prefs@Graph.prefs$col,
           ids = NULL,
           idsmode = object@Prefs@Graph.prefs$idsmode,
           idsext = object@Prefs@Graph.prefs$idsext,
           idscex = object@Prefs@Graph.prefs$idscex,
           idsdir = object@Prefs@Graph.prefs$idsdir,
           bwhoriz = object@Prefs@Graph.prefs$bwhoriz,
           bwratio = object@Prefs@Graph.prefs$bwratio,
           bwvarwid = object@Prefs@Graph.prefs$bwvarwid,
           bwdotpch = object@Prefs@Graph.prefs$bwdotpch,
           bwdotcol = object@Prefs@Graph.prefs$bwdotcol,
           bwdotcex =object@Prefs@Graph.prefs$bwdotcex,
           bwreccol = object@Prefs@Graph.prefs$bwreccol,
           bwrecfill = object@Prefs@Graph.prefs$bwrecfill,
           bwreclty = object@Prefs@Graph.prefs$bwreclty,
           bwreclwd = object@Prefs@Graph.prefs$bwreclwd,
           bwumbcol = object@Prefs@Graph.prefs$bwumbcol,
           bwumblty = object@Prefs@Graph.prefs$bwumblty,
           bwumblwd = object@Prefs@Graph.prefs$bwumblwd,
           bwoutcol = object@Prefs@Graph.prefs$bwoutcol,
           bwoutcex = object@Prefs@Graph.prefs$bwoutcex,
           bwoutpch = object@Prefs@Graph.prefs$bwoutpch,
           grid = object@Prefs@Graph.prefs$grid,
           logy = FALSE,
           logx = FALSE,
           force.x.continuous = TRUE,
           binvar = NULL,
           bins = 10,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Name(s) of the x-variable.}
  \item{y}{Name(s) of the y-variable. }
  \item{object}{An xpose.data object. }
  \item{subscripts}{The standard Trellis subscripts argument (see
    \code{\link[lattice]{xyplot}}).} 
  \item{groups}{Name of the variable used for superpose plots.}
  \item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
    is included in the plot.}
  \item{onlyfirst}{Logical value indicating whether only the first row
    per individual is included in the plot. }
  \item{samp}{An integer between 1 and object@Nsim
    (see\code{\link[xpose4data]{xpose.data-class}}) specifying which of the
    simulated data sets to extract from SData.}
  \item{xvarnam}{Character string with the name of the x-variable. }
  \item{yvarnam}{Character string with the name of the y-variable.}
  \item{type}{Character value indicating the type of display to use:
    "l"=lines, "p"=points, "b"=both points and lines.}
  \item{col}{Colour of lines and plot symbols.}
  \item{pch}{Plot character to use.}
  \item{cex}{Size of the plot characters.}
  \item{lty}{Line type.}
  \item{fill}{Fill colour.}
  \item{ids}{Character value with the name of the variable to label data
    points with.}
  \item{idsmode}{Determines the way text labels are added to
    plots. \code{NULL} means that only extreme points are
    labelled. Non-\code{NULL} means all data points are labelled. (See
    \code{link[xpose4generic]{xpose.plot.default}})}
  \item{idsext}{See \code{link[xpose4generic]{xpose.plot.bw}}}
  \item{idscex}{Size of text labels.}
  \item{idsdir}{A value of "both" (the default) means that
    both high and low extreme points are labelled while "up" and "down"
    labels the high and low extreme points respectively. See \code{\link[xpose4generic]{xpose.plot.bw}}}
  \item{bwhoriz}{logical value indicating whether box and whiskers should be 
    horizontal or not. The default is FALSE.}
  \item{bwratio}{Ratio of box height to inter-box space. The default is 1.5.
    An argument for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwvarwid}{Logical. If TRUE, widths of boxplots are proportional to the
    number of points used in creating it. The default is FALSE. An argument 
    for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwdotpch}{Graphical parameter controlling the dot plotting character
    'bwdotpch="|"' is treated specially, by replacing 
    the dot with a line. The default is 16. An argument for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwdotcol}{Graphical parameter controlling the dot colour - an integer or 
    string. See 'col'. The default is black. An argument for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwdotcex}{The amount by which plotting text and symbols should be scaled 
    relative to the default. 'NULL' and 'NA' are equivalent to '1.0'. An argument 
    for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwreccol}{The colour to use for the box rectangle - an integer or string. 
    The default is blue. See \code{\link[lattice]{trellis.par.get}} and "box.rectangle".}
  \item{bwrecfill}{The colour to use for filling the box rectangle - an integer 
    or string. The default is transparent (none). See \code{\link[lattice]{trellis.par.get}} 
    and "box.rectangle".}
  \item{bwreclty}{The line type for the box rectangle - an integer or string. 
    The default is solid. See \code{\link[lattice]{trellis.par.get}} and "box.rectangle".}
  \item{bwreclwd}{The width of the lines for the box rectangle - an integer. The 
    default is 1. See \code{\link[lattice]{trellis.par.get}} and "box.rectangle".}
  \item{bwumbcol}{The colour to use for the umbrellas - an integer or string. 
    The default is blue. See \code{\link[lattice]{trellis.par.get}} and "box.umbrella".}
  \item{bwumblty}{The line type for the umbrellas - an integer or string. The 
    default is solid.See \code{\link[lattice]{trellis.par.get}} and "box.umbrella".}
  \item{bwumblwd}{the width of the lines for the umbrellas - an integer. The 
    default is 1. See \code{\link[lattice]{trellis.par.get}} and "box.umbrella".}
  \item{bwoutcol}{The colour to use for the outliers - an integer or string. 
    The default is blue. See \code{\link[lattice]{trellis.par.get}} and "box.symbol".}
  \item{bwoutcex}{The amount by which outlier points should be scaled relative 
    to the default. 'NULL' and 'NA' are equivalent to '1.0'. The default
    is 0.8. See \code{\link[lattice]{trellis.par.get}} and "box.symbol".}
  \item{bwoutpch}{The plotting character, or symbol, to use for outlier points. 
    Specified as an integer. See R help on 'points'. The default is an open 
    circle. See \code{\link[lattice]{trellis.par.get}} and "box.symbol".}
  \item{grid}{logical value indicating whether a visual reference grid
    should be added to the graph. (Could use arguments for line type,
    color etc).}
  \item{logy}{Logical value indicating whether the y-axis should be logarithmic.}
  \item{logx}{Logical value indicating whether the x-axis should be logarithmic.}
  \item{force.x.continuous}{Logical value indicating whether x-values should be
    taken as continuous, even if categorical.}
  \item{binvar}{Variable to be used for binning.}
  \item{bins}{The number of bins to be used. The default is 10.}
  \item{\dots}{Other arguments that may be needed in the function. }
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{\code{\link[xpose4data]{xpose.data-class}}, Cross-references above.}
\keyword{methods}
