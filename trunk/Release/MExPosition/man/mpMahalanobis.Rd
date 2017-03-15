\name{mpMahalanobis}
\alias{mpMahalanobis}
\encoding{UTF-8}
\title{mpMahalanobis: Mahalanobis Distance}
\description{Computation of Mahalanobis Distance}
\usage{mpMahalanobis(data, row.design)}
\arguments{
  \item{data}{Data Matrix}
  \item{row.design}{Design Matrix which identifies the groups of the data matrix}
 
}
\details{
Computation of Mahalanobis Distance which is used in \code{\link{mpCANOSTATIS}}.
}

\value{
  \item{D}{Matrix of Mahalanobis Distances}
  }
  
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpCANOSTATIS}}
}
\examples{
   #Mahalanobis Example
   data('wines2012')
   data <- as.matrix(wines2012$data[,1:6])
   design <- makeNominalData(as.matrix(c('NZ','NZ','NZ','NZ',
    'FR','FR','FR','FR','CA','CA','CA','CA')))
   demo <- mpMahalanobis(data,design)
}
\keyword{ multivariate}
