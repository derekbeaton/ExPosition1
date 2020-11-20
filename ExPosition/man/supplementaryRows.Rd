\name{supplementaryRows}
\alias{supplementaryRows}
\title{
Supplementary rows
}
\description{
Computes factor scores for supplementary observations (rows).
}
\usage{
supplementaryRows(SUP.DATA, res)
}
\arguments{
\item{SUP.DATA}{a data matrix of supplementary observations (must have the same measures [columns] as active data)}
\item{res}{ExPosition or TExPosition results}
}
\details{
This function recognizes the class types of: \code{\link{epPCA}}, \code{\link{epMDS}}, \code{\link{epCA}}, \code{\link{epMCA}} and \code{TExPosition} methods. Further, the function recognizes if Hellinger (as opposed to row profiles; in CA, MCA and DICA) were used. 
}
\value{
A list of values containing:\cr
\item{fii}{factor scores computed for supplemental observations}
\item{dii}{squared distances for supplemental observations}
\item{rii}{cosines for supplemental observations}
}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }
