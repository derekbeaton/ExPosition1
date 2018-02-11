\name{mpSTATIS.tablePreproc}
\alias{mpSTATIS.tablePreproc}
\encoding{UTF-8}
\title{mpSTATIS.tablePreproc: Table Preprocessing for STATIS}
\description{Preprocessing of the tables for STATIS.}
\usage{mpSTATIS.tablePreproc(data, column.design, table.preprocess = 'None')}
\arguments{
  \item{data}{Data Matrix}
  \item{column.design}{Matrix which identifies the tables.}
  \item{table.preprocess}{String option with the following options: 'None' (Default), 'Num_Columns', 'Tucker','Sum_PCA', 'RV_Normalization' and 'MFA_Normalization'}
}
\details{
Table Preprocessing is the last preprocessing step in STATIS. Only one type of table preprocessing is suggested. 

If you need to create the Group Matrix into a design matrix, you can use \code{\link{makeNominalData}} which was developed by Derek Beaton.
}
\value{
The output of STATIS.tablePreproc is a matrix of the same dimensions as the data matrix, which is the result of the table preprocessing step chosen.
}
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpSTATIS.rowPreproc}}, \code{\link{mpSTATIS.columnPreproc}}, \code{\link{mpSTATIS.preprocess}}
}
\examples{
# Sum PCA - type of table preprocessing choosen
table.preprocess='Sum_PCA'
X <- matrix(1:10,2)
Y<- c('g1','g1','g1','g2','g2')
groupMatrix <- t(makeNominalData(as.matrix(Y)))
preproc <- mpSTATIS.tablePreproc(X,groupMatrix, table.preprocess)
}
\keyword{ multivariate}
