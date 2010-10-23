\name{xpose.multiple.plot}
\alias{xpose.multiple.plot}

\title{
Create and object with class "xpose.multiple.plot".
}
\description{
Create and object with class "xpose.multiple.plot".
}
\usage{
xpose.multiple.plot(plotList,
           plotTitle=NULL,
           nm7 = TRUE,
           prompt=FALSE,
           new.first.window=FALSE,
           max.plots.per.page=4,                   
           title    = list(
             title.x = unit(0.5, "npc"),
             title.y = unit(0.5, "npc"),
             title.gp= gpar(cex=1.2,fontface="bold"),#,font=2),
             title.just = c("center","center")
             ),
           mirror=FALSE,
           bql.layout=FALSE,
           ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plotList}{
A list of lattice plots.
}
  \item{plotTitle}{
Main title for plots.
}
  \item{nm7}{
\code{TRUE} if we are using NONMEM 7
}
  \item{prompt}{
When printing should we prompt for each new page in plot?
}
  \item{new.first.window}{
\code{TRUE} or \code{FALSE}.
}
  \item{max.plots.per.page}{
A number.  Max value is 9.
}
  \item{title}{
Title properties.
}
  \item{mirror}{
Are there mirror plots in plot list?
}
  \item{bql.layout}{
Should we use layout optimized for plots with BQL (below limit of quantification) measurements?
}
  \item{\dots}{
Additional options passed to function.
}
}
\details{
}
\value{
An object of class "xpose.multiple.plot".
}
\references{
}
\author{
Niclas Jonsson and Andrew C. Hooker
}
\note{
}

\seealso{
  \code{\link{print.xpose.multiple.plot}},
  \code{\link{xpose.multiple.plot.default}} 
}
\examples{
\dontrun{
## See code for cwres.vs.cov() by typing

library(xpose4specific)
cwres.vs.cov

}
}

\keyword{ methods }
