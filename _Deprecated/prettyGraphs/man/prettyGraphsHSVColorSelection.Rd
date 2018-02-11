\name{prettyGraphsHSVColorSelection}
\alias{prettyGraphsHSVColorSelection}
\title{
Algorithmically select HSV colors.
}
\description{
This function uses an HSV color selection algorithm to create HSV color palletes. 
}
\usage{
prettyGraphsHSVColorSelection(n.colors = 1, offset = NULL, h = 13, s = 0.75, v = 0.75)
}
\arguments{
  \item{n.colors}{
number of colors needed
}
  \item{offset}{
numeric (decimal/percentage). This number decides the distance between indices for color selection. If NULL, the golden ratio is selected.
}
  \item{h}{
numeric. The initial hue (see \code{\link{hsv}}).
}
  \item{s}{
numeric. The initial saturation (see \code{\link{hsv}}).
}
  \item{v}{
numeric. The initial value (see \code{\link{hsv}}).
}
}
\value{
	\item{a matrix}{a matrix of colors are returned.}
}
\author{
Derek Beaton\cr\cr
HSV selection from here:\cr
http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
}
\seealso{
	\code{\link{prettyGraphsColors}}, 	\code{\link{prettyGraphsColorSelection}}
}
\keyword{ graphs }
