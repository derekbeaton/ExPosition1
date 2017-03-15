\name{createColorVectorsByDesign}
\alias{createColorVectorsByDesign}
\title{
createColorVectorsByDesign
}
\description{
Produces a color vector for items by using a design matrix.
}
\usage{
createColorVectorsByDesign(design_matrix,hsv = TRUE, offset = NULL)
}
\arguments{
  \item{design_matrix}{
A dummy coded design matrix.
}
  \item{hsv}{
a boolean. If TRUE, use \code{\link{prettyGraphsHSVColorSelection}}. If FALSE, use \code{\link{prettyGraphsColorSelection}}.
}
  \item{offset}{
numeric. An offset value to be passed to \code{\link{prettyGraphsHSVColorSelection}} or \code{\link{prettyGraphsColorSelection}}.
}
}
\value{
	Produces a list with the following items\cr
	\item{oc}{The colors of the observations (based on group)}
	\item{gc}{The colors of the groups}	
}

\author{
Derek Beaton
}
\keyword{ misc }
