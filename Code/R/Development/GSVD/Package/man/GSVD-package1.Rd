\name{GSVD-package}
\alias{GSVD-package}
\alias{GSVD}
\encoding{UTF-8}
\docType{package}
\title{
  Generalized Singular Value Decomposition
}
\description{
 The generalized singular value decomposition (GSVD) generalizes the standard SVD (see \code{\link{svd}}) procedure through addition of (optional) constraints applied to the rows and/or columns of a matrix.
}
\details{
While dedicated to the GSVD, this package also includes some nice features that are helpful for matrix analyses. For example there are tests to check if matrices are empty (\code{\link{is.empty.matrix}}) or identity (\code{\link{is.identity.matrix}}); also included are operations such as generalized inverse (\code{\link{matrix.generalized.inverse}}) and matrix exponents (\code{\link{matrix.exponent}} or \code{\link{\%^\%}}).
}
\author{
  Questions, comments, compliments, and complaints go to Derek Beaton \email{exposition.software@gmail.com}.\cr\cr
}
\references{

}
\keyword{ package }
\keyword{ multivariate }
\seealso{
\code{\link{gsvd}}, \code{\link{tolerance.svd}}, \code{\link{\%^\%}}
}
\examples{

}
