\name{mpSTATIS.columnPreproc}
\alias{mpSTATIS.columnPreproc}
\encoding{UTF-8}
\title{mpSTATIS.columnPreproc: Column Preprocessing for STATIS}
\description{Preprocessing of the columns of the table for STATIS.}
\usage{mpSTATIS.columnPreproc(data, column.preprocess = 'None')}
\arguments{
  \item{data}{Data Matrix}
  \item{column.preprocess}{String option with the following options: 'None' (default),'Center','1Norm', 'Center_1Norm' and 'Z_Score'}
}
\details{
Column Preprocessing is the second preprocessing step in STATIS. The only combination of Column Preprocessing allowed is Column Center plus 1 Norm.Besides this combination, all other column preprocessing options are done independently. 

If you need to create the Group Matrix into a design matrix, you can use \code{\link{makeNominalData}} which was developed by Derek Beaton.
}
\value{
A matrix of the same dimensions as X, which is the result of the column preprocessing step chosen.
}
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpSTATIS.rowPreproc}}, \code{\link{mpSTATIS.tablePreproc}}, \code{\link{mpSTATIS.preprocess}}
}
\examples{
# Center- type of column preprocessing chosen
column.preprocess = 'Center'
X <- matrix(1:10,2)
preproc <- mpSTATIS.columnPreproc(X, column.preprocess)
}
\keyword{ multivariate }