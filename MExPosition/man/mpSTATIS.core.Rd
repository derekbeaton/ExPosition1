\name{mpSTATIS.core}
\alias{mpSTATIS.core}
\encoding{UTF-8}
\title{mpSTATIS.core}
\description{mpSTATIS.core performed the core of STATIS.}
\usage{mpSTATIS.core(data, num.obs, column.design, num.groups, optimization.option = 'STATIS')}
\arguments{
  \item{data}{Matrix of preprocessed data}
  \item{num.obs}{Number of Observations}
  \item{column.design}{Matrix which identifies the tables.}
  \item{num.groups}{Number of Groups/Tables}
  \item{optimization.option}{String option with the following options: 'None', 'STATIS', 'RV Matrix' and 'STATIS Power 1'}
}

\value{
   \item{S}{Scalar Product Matrices}
  \item{RVMatrix}{RV Matrix}
  \item{C}{C Matrix}
  \item{eigs.vector}{Inner Product: Eigen Vectors of S}
  \item{eigs}{Inner Product: Eigen Value}
  \item{fi}{Inner Product: Factor Scores}
  \item{tau}{Inner Product: Percent Variance Explained}
  \item{alphaWeights}{Alpha Weights}
  \item{compromise}{Compromise Matrix}
  \item{compromise.eigs.vector}{Compromise: Eigen Vectors}
  \item{compromise.eigs}{Compromise: Eigen Values}
  \item{compromise.fi}{Compromise: Factor Scores}
  \item{compromise.ci}{Compromise: contribution of the rows}
  \item{compromise.cj}{Compromise: contribution of the colunbs}
  \item{compromise.tau}{Compromise: Percent Variance Explained}
  \item{table.eigs.value}{Table: Eigen Values}
  \item{table.eigs}{Table: Eigen Vectors}
  \item{table.loadings}{Table: Loadings}
  \item{table.fi}{Table: Factor Scores}
  \item{table.partial.fi}{Table: Partial Factor Scores}
  \item{table.partial.fi.array}{Table: Array of Partial Factor Scores}
  \item{table.ci}{Table: contribution of the rows}
  \item{table.cj}{Table: contribution of the columns}
  \item{table.tau}{Table: Percent Variance Explained}
}
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4. \cr \cr
Abdi, H., & Valentin, D. (2007). STATIS. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}. Sage. pp. 955-962. \cr
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpSTATIS}}, \code{\link{mpDISTATIS.core}}, \code{\link{mpDISTATIS}}
}
\keyword{ multivariate}
