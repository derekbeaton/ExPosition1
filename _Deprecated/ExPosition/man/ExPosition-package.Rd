\name{ExPosition-package}
\alias{ExPosition-package}
\alias{ExPosition}
\encoding{UTF-8}
\docType{package}
\title{
ExPosition: \emph{Ex}ploratory Analysis with the Singular Value Decom\emph{Position}
}
\description{
Exposition is defined as \emph{a comprehensive explanation of an idea}. With ExPosition for R, a comprehensive explanation of your data will be provided with minimal effort.\cr\cr
The core of ExPosition is the singular value decomposition (SVD; see: \code{\link{svd}}). The point of ExPosition is simple: to provide the user with an overview of their data that only the SVD can provide. ExPosition includes several techniques that depend on the SVD (see below for examples and functions).
}
\details{
\tabular{ll}{
Package: \tab ExPosition\cr
Type: \tab Package\cr
Version: \tab 2.8.20\cr
Date: \tab 2013-12-09\cr
Depends: \tab R (>=2.15.0), prettyGraphs (>= 2.1.4)\cr
License: \tab GPL-2\cr
URL: \tab http://www.utdallas.edu/~derekbeaton/software/exposition\cr
}
}
\author{
Questions, comments, compliments, and complaints go to Derek Beaton \email{exposition.software@gmail.com}.\cr\cr

The following people are authors or contributors to ExPosition code, data, or examples:\cr
Derek Beaton, Hervé Abdi, Cherise Chin-Fatt, Joseph Dunlop, Jenny Rieck, Rachel Williams,  Anjali Krishnan, and Francesca Filbey.
}
\references{
Abdi, H., and Williams, L.J. (2010). Principal component analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 2, 433-459.\cr
Abdi, H. and Williams, L.J. (2010). Correspondence analysis. In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr
Abdi, H. (2007). Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr
Abdi, H. (2007). Metric multidimensional scaling. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics.} Thousand Oaks (CA): Sage. pp. 598-605.\cr
Greenacre, M. J. (2007). Correspondence Analysis in Practice. \emph{Chapman and Hall}.\cr
Benzécri, J. P. (1979). Sur le calcul des taux d'inertie dans l'analyse d'un questionnaire. \emph{Cahiers de l'Analyse des Données}, \bold{4}, 377-378.\cr
}
\keyword{ package }
\keyword{ multivariate }
\seealso{
\code{\link{epPCA}}, \code{\link{epGPCA}}, \code{\link{epMDS}}, \code{\link{epCA}}, \code{\link{epMCA}}
}
\examples{
#For more examples, see each individual function (as noted above).
}
