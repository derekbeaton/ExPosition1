\name{correlationPlotter}
\alias{correlationPlotter}

\title{
correlationPlotter
}
\description{
Computes and plots a correlation circle (used in multivariate analyses). Correlation is computed between measured items and components (factors, dimensions, principal axes, etc...). 
}
\usage{
correlationPlotter(data_matrix, factor_scores, x_axis = 1, y_axis = 2, 
  col = NULL, pch = NULL, xlab = "", ylab = "", main = "", 
	axis.lwd = 3, circle.lwd = 3, circle.col = "#00000040", 
	asp = 1, dev.new = TRUE)
}
\arguments{
  \item{data_matrix}{
	A set of data (i.e., original measures and observations)
}
  \item{factor_scores}{
	One set of factor scores that were computed from the original data matrix.
}
  \item{x_axis}{
Which axis is the x-axis? Default is 1.
}
  \item{y_axis}{
Which axis is the y-axis? Default is 2.
}
  \item{col}{
A single-column matrix of colors for each data point.
}
  \item{pch}{
A single-column matrix of pch for each data point. Indicates which point style to use for each item. See \code{\link{par}}.
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
  \item{axis.lwd}{
numeric. Line width for the axes.
  } 
  \item{circle.lwd}{
  numeric. Line width for the circle.
  }
  \item{circle.col}{
  color for the circle
  }
  \item{asp}{
numeric. Aspect ratio (see \code{asp} in \code{\link{par}}).
}
  \item{dev.new}{
boolean. If TRUE, \code{\link{dev.new}} is called internally to create new device. If FALSE, a device must already be open.
}
 
}

\author{
Derek Beaton
}
\keyword{ graphs }
\keyword{ multivariate }