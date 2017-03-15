\name{mpDISTATIS.core}
\alias{mpDISTATIS.core}
\encoding{UTF-8}
\title{mpDISTATIS.core}
\description{mpDISTATIS.core performs the core functions of DISTATIS.}
\usage{mpDISTATIS.core(data, table, sorting = 'No', normalization = 'None', 
masses = NULL, make.table.nominal=TRUE)}
\arguments{
  \item{data}{Matrix of preprocessed data}
  \item{table}{Table which identifies the different tables.}
  \item{sorting}{a boolean. If YES, DISTATIS will by processed as a sorting task. Default is NO}
  \item{normalization}{Normaliztion string option: 'None' (default), 'Sum_PCA', or 'MFA'}
  \item{masses}{Masses: if NULL, 1/num.obs would be set by default. For customized masses, enter the vector of customized masses}
  \item{make.table.nominal}{a boolean. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
}
\details{This function should not be used directly. Please use \code{\link{mpDISTATIS}}
}
\value{
Returns a large list of items which are also returned in \code{\link{mpDISTATIS}}.
  \item{data}{Data Matrix}
  \item{table}{Design Matrix}
  \item{normalization}{Type of Normalization used.}
  \item{sorting}{Indicates if the task is a sorting task}
  \item{S}{Inner Product: Scalar Product Matrices}
  \item{C}{Inner Product: C Matrix}
  \item{ci}{Inner Product: Contribution of the rows of C}
  \item{cj}{Inner Product: Contribuition of the columns of C}
  \item{eigs.vector}{Inner Product: Eigen Vectors}
  \item{eigs}{Inner Product: Eigen Values}
  \item{fi}{Inner Product: Factor Scores}
  \item{tau}{Inner Product: Percent Variance Explained}
  \item{alphaWeights}{Alpha Weights}
  
   \item{compromise}{Compromise Matrix}
  \item{compromise.eigs}{Compromise: Eigen Values}
  \item{compromise.eigs.vector}{Compromise: Eigen Vector}
  \item{compromise.fi}{Compromise: Factor Scores}
  \item{Compromise.tau}{Compromise: Percent Variance Explained}
  \item{compromise.ci}{Compromise: Contributions of the rows}
  \item{compromise.cj}{Compromise: Contributions of the Columns}
  
  \item{masses}{Table: masses}
  \item{table.eigs}{Table: Eigen Values}
  \item{table.eigs.vector}{Table: Eigen Vectors}
  \item{table.Q}{Table: Loadings}
  \item{table.fi}{Table: Factor Scores}
  \item{table.partial.fi}{Table: Partial Factor Scores}
  \item{table.partial.fi.array}{Table: Array of Partial Factor Scores}
  \item{table.tau}{Table: Percent Variance Explained}
  }
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167 \cr \cr
Abdi, H., Valentin, D., Chollet, S., & Chrea, C. (2007). Analyzing assessors and products in sorting tasks: DISTATIS, theory and applications. \emph{Food Quality and Preference}, 18, 627-640.\cr \cr
Abdi, H., & Valentin, D. (2005). DISTATIS: the analysis of multiple distance matrices. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}. Thousand Oaks (CA): Sage. pp. 284-290. \cr
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpSTATIS}}, \code{\link{mpSTATIS.core}}, \code{\link{mpDISTATIS}}
}
\keyword{ multivariate }

