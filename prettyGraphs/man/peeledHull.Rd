\name{peeledHull}
\alias{peeledHull}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
peeledHull
}
\description{
Computes and plots a peeled hull around a set of points onto a current graphics device.
}
\usage{
peeledHull(data_matrix, x_axis = 1, y_axis = 2, percentage = 1, 
	col = "black", lwd = 3, lty = 1)
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
  \item{percentage}{
The percentage of points that should be enveloped by the hull.
}
  \item{col}{
The color of the hull (see col in plot()).
}
  \item{lwd}{
The thickness of the hull line (see lwd in plot())
}
  \item{lty}{
The line type (see lty in plot()).
}
}
\references{
Greenacre, M. J. (2007). Correspondence Analysis in Practice. \emph{Chapman and Hall}.
}
\author{
Derek Beaton turned Michael Greenacre's code into a function.
}
\note{
The code for this function was (barely) adapted from Michael Greenacre's book on correspondence analysis. All credit for this code should go to Michael Greenacre; I only turned it into a function. The original code can be found at http://carme-n.org/?sec=code2
}

\keyword{ graphs }
\keyword{ multivariate }
