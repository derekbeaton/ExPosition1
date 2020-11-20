\name{supplementaryCols}
\alias{supplementaryCols}
\title{
Supplementary columns
}
\description{
Computes factor scores for supplementary measures (columns).
}
\usage{
supplementaryCols(SUP.DATA, res, center = TRUE, scale = TRUE)
}
\arguments{
\item{SUP.DATA}{a data matrix of supplementary measures (must have the same observations [rows] as active data)}
\item{res}{ExPosition or TExPosition results}
  \item{center}{a boolean, string, or numeric. See \code{\link{expo.scale}}}
  \item{scale}{a boolean, string, or numeric. See \code{\link{expo.scale}}}
}
\details{
This function recognizes the class types of: \code{\link{epPCA}}, \code{\link{epMDS}}, \code{\link{epCA}}, \code{\link{epMCA}}, and \code{TExPosition} methods. Further, the function recognizes if Hellinger (as opposed to row profiles; in CA, MCA and DICA) were used. 
}
\value{
A list of values containing:\cr
\item{fjj}{factor scores computed for supplemental columns}
\item{djj}{squared distances for supplemental columns}
\item{rjj}{cosines for supplemental columns}
}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }
