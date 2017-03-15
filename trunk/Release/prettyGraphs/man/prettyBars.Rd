\name{prettyBars}
\alias{prettyBars}
\title{
prettyBars
}
\description{
prettyBars creates simple, crisp, publication-style quality bar graphs. 
}
\usage{
prettyBars(data, axis = 1, cex.names = 0.5, fg.col = NULL, axis.lims = NULL,
	show.bg.bars = FALSE, threshold.line = FALSE, main = "", bg.border = "white", 
	bg.col = NULL, bg.lims = NULL, sort.data = TRUE, horiz = TRUE, dev.new = TRUE)
}
\arguments{
  \item{data}{A set of data you would like to plot with bars for 1 dimension. Can be original data or factor scores or anything with at least 1 column.}
  \item{axis}{which axis (column of \code{data}) should be plotted?}
  \item{cex.names}{size of the text}
  \item{fg.col}{a matrix (single column) of colors for bars corresponding to rows of \code{data}.}
  \item{axis.lims}{two values in the form of \code{c(min,max)} for plot limits. If NULL, the min and max are computed.}
  \item{show.bg.bars}{a set of bars to be plotted under the bars in \code{data}. Used for a fill effect.}
  \item{threshold.line}{boolean. If TRUE, a line perpendicular to the bars is plotted. The lines appear at \code{bg.lims}.}
  \item{main}{A title to be placed at the top of the graph.}
  \item{bg.border}{color for \code{show.bg.bars} borders (see \code{border} in \code{\link{barplot}})}
  \item{bg.col}{a matrix (single column) of colors for background bars to be plotted under \code{fg.col}.}
  \item{bg.lims}{two values in the form of \code{c(min,max)} for where to plot background bars or threshold line.}
  \item{sort.data}{a boolean. Will sort the bars by descending values.}
  \item{horiz}{see \code{horiz} in \code{\link{barplot}}.}
  \item{dev.new}{boolean. If TRUE, \code{\link{dev.new}} is called internally to create new device. If FALSE, a device must already be open.}
}
\value{
	\item{bp.cols}{locations of bars (as would be returned by \code{\link{barplot}}).}
}
\author{
Derek Beaton
}
\seealso{
	\code{\link{barplot}}, \code{\link{prettyPlot}}
}
\examples{
	##stolen from ?barplot
	#tN <- table(Ni <- stats::rpois(100, lambda = 5))
	#the.colors <- rainbow(length(tN))
	#dev.new()
	#r <- barplot(tN, col = the.colors)
	#prettyBars(as.matrix(tN),axis=1,fg.col=as.matrix(the.colors),horiz=FALSE,dev.new=TRUE)
}
\keyword{ graphs }
\keyword{ multivariate }
\keyword{ bootstrap }