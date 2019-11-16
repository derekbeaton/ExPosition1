\name{mpSTATIS.rowPreproc}
\alias{mpSTATIS.rowPreproc}
\encoding{UTF-8}
\title{mpSTATIS.rowPreproc: Row Preprocessing for STATIS}
\description{Preprocessesing of the rows of the matrix for STATIS.}
\usage{mpSTATIS.rowPreproc(data, row.preprocess = 'None')}
\arguments{
  \item{data}{Data Matrix}
  \item{row.preprocess}{String option with the following options: 'None'(Default), 'Profile', 'Hellinger','Center', and 'Center_Hellinger'}
}
\details{
Row Preprocessing is the first preprocessing step in STATIS. The only combination of row preprocessing that is allowed is Centering and Hellinger. The other preprocessing options cannot be combined. 

If you need to create the Group Matrix into a design matrix, you can use \code{\link{makeNominalData}} which was developed by Derek Beaton.
}
\value{
A matrix of the same dimensions as the data matrix, which is the result of the row preprocessing step chosen.
}
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167. \cr
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpSTATIS.rowPreproc}}, \code{\link{mpSTATIS.columnPreproc}}, \code{\link{mpSTATIS.tablePreproc}}
}
\examples{
# Center - type of row preprocessing choosen
row.preprocess ='Center'
X <- matrix(1:10,2)
preproc <- mpSTATIS.rowPreproc(X, row.preprocess)
}
\keyword{ multivariate}