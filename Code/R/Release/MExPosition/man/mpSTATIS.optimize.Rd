\name{mpSTATIS.optimize}
\alias{mpSTATIS.optimize}
\title{mpSTATIS.optimize: STATIS Optimization Options}
\description{Provides various optimization options for STATIS.}
\usage{mpSTATIS.optimize(data, num.obs, column.design = NULL, 
	num.groups, optimization.option = 'STATIS')}
\arguments{
  \item{data}{Data Matrix}
  \item{num.obs}{Number of Observation}
  \item{column.design}{Table which identifies the tables.}
  \item{num.groups}{Number of Tables}
  \item{optimization.option}{String option with the following options: 'None', 'STATIS' (default), 'RV_Matrix', 'STATIS_Power1', 'ANISOSTATIS_Type1', 'ANISOSTATIS_Type2'}
}
\details{
After the optimization option is passed through this function, the core of the STATIS processing is performed by calling either \code{\link{mpSTATIS.core}} or \code{\link{mpANISOSTATIS.core}}.
}
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt \email{cherise.chinfatt@utdallas.edu}
}
\seealso{
  \code{\link{mpDISTATIS}}, \code{\link{mpSTATIS}}, \code{\link{mpANISOSTATIS.core}}
}
\keyword{ multivariate }
