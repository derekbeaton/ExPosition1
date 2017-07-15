\name{prettyPlot}
\alias{prettyPlot}
\title{
prettyPlot
}
\description{
prettyPlot creates simple, crisp, publication-style quality graphics for multivariate analyses. 
}
\usage{
prettyPlot(data_matrix,x_axis=1,y_axis=2,col=NULL,pch=NULL,cex=NULL,text.cex=NULL,
	pos=3,xlab="",ylab="",main="",display_names=TRUE,display_points=TRUE,
	constraints=NULL,contributionCircles=FALSE,contributions=NULL,axes=TRUE,
	fg.line.width=3,fg.type="l",fg.col="black",
	bg.line.width=1.5,bg.lty=3,bg.col="black",
	flip=FALSE,asp=1,findBounds=TRUE,dev.new=TRUE,new.plot=TRUE)
}
\arguments{
  \item{data_matrix}{
	A set of data you would like to plot on 2 dimensions (e.g., a scatter plot). Can be original data or factor scores or anything with at least 2 columns.
}
  \item{x_axis}{
Which axis is the x-axis? Default is 1.
}
  \item{y_axis}{
Which axis is the y-axis? Default is 2.
}
  \item{col}{any acceptable color format.
A single-column matrix of colors for each data point. A single value can be used. 
}
  \item{pch}{
A single-column matrix of pch for each data point. A single value can be used. Indicates which point style to use for each item. See \code{\link{par}}.
}
  \item{cex}{a single column of continuous values. A single value can be used. Indicates the size of the points. See \code{\link{par}}. Used multiplicatively for points with \code{contributions}.
}
  \item{text.cex}{A replacement for the cex parameter in text() (to avoid collisions with cex). Used additively for points with \code{contributions}. See \code{\link{par}} and \code{\link{text}}.
}
  \item{pos}{
integer between 1-4. Determines position of text for points. See \code{\link{par}}.
} 
  \item{xlab}{
A label to be placed along the x-axis.
}
  \item{ylab}{
A label to be placed along the y-axis.
}
  \item{main}{
A title to be placed at the top of the graph.
}
  \item{display_names}{
boolean. If TRUE, the names of the points will be displayed on the plot.
}
  \item{display_points}{
boolean. If TRUE, the points will be displayed on the plot.
}
  \item{constraints}{
A list that contains the plot constraints. Default is NULL (constraints are computed each time).
}
  \item{contributionCircles}{
boolean. If TRUE, items plotted will vary in size, dependent on amount of contribution to the variance.
}
  \item{contributions}{
A matrix of contribution values for data_matrix (should also be the same size & dimensionality as data_matrix)
}

\item{axes}{boolean. If TRUE, a new set of axes are plotted.}

\item{fg.line.width}{integer. Determines thickness of foreground (default: solid) axis lines, see \code{\link{points}} and \code{lwd} under \code{\link{par}}.}

\item{fg.type}{character or string Determines type of points for foreground (default: solid) axis lines, see \code{\link{points}} and \code{type} under \code{\link{par}}.}

\item{fg.col}{any acceptable color format. Determines color for foreground (default: solid) axis lines, see \code{\link{points}} and \code{col} under \code{\link{par}}.}

\item{bg.line.width}{integer. Determines thickness of background (default: dashed) axis lines, see \code{\link{abline}} and \code{lwd} under \code{\link{par}}.}

\item{bg.lty}{integer. Determines type of background (default: dashed) axis lines, see \code{\link{abline}} and \code{lty} under \code{\link{par}}.}

\item{bg.col}{any acceptable color format. Determines color of background (default: dashed) axis lines, see \code{\link{abline}} and \code{col} under \code{\link{par}}.}

  \item{flip}{
boolean. If TRUE, exchanges bg and col for all applicable pch values.
}
  \item{asp}{
numeric. Aspect ratio, see \code{\link{par}}.
}
  \item{findBounds}{
boolean. If TRUE, finds the min and max of each plotted axes. If FALSE, the largest value is used for all constraints.
}
  \item{dev.new}{
boolean. If TRUE, \code{\link{dev.new}} is called internally to create new device. If FALSE, a device must already be open.
}
  \item{new.plot}{
boolean. If TRUE, \code{\link{plot}} is called for a new plot on the current device. If FALSE, items will be overlayed much like \code{\link{points}}.
}

}
\details{
	All items after contributions (i.e., axes onward) are parameters for very fine-grained detail. They are quite powerful but in most cases not required.
}
\value{
Returns a three item list:

	\item{col}{A matrix of colors where each element is the color for each item plotted.}
	\item{pch}{A matrix of pch values where each element is the pch number for each item plotted.}	
	\item{constraints}{A list (from \code{\link{minmaxHelper}}) of the plot constraints (i.e., min and max for axes)}

}
\author{
Derek Beaton
}
\keyword{ graphs }
\keyword{ multivariate }
