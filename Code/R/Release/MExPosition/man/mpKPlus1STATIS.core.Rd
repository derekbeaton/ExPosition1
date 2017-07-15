\name{mpKPlus1STATIS.core}
\alias{mpKPlus1STATIS.core}
\encoding{UTF-8}
\title{mpKPlus1STATIS.core: Core Function for (K+1) STATIS via MExPosition}
\description{Performs the core of (K+1) STATIS}
\usage{mpKPlus1STATIS.core(data, plus1data, num.obs, column.design, num.groups, 
  optimization.option = 'STATIS')}
\arguments{
  \item{data}{Matrix of preprocessed data}
  \item{plus1data}{Matrix of preprocessed external table}
  \item{num.obs}{Number of observations}
  \item{column.design}{Column Design for data - used to identifty the tables of the data matrix}
  \item{num.groups}{Number of groups}
  \item{optimization.option}{String option of either 'None', 'Multiable', 'RV_Matrix', 'STATIS' (DEFAULT), \emph{or} 'STATIS_Power1'}
}
\details{
Computation of (K+1) STATIS. This function should not be used independently. It should be used with \code{\link{mpKPlus1STATIS}}
}

\value{
  \item{S}{Inner Product: Scalar Product Matrices of dataset}
  \item{S.star}{Inner Product: Scalar Product Matrices * of dataset}
  \item{rvMatrix.star}{Inner Product: RV Matrix *}
  \item{C}{Inner Product: C Matrix of S*}
  \item{ci}{Inner Product: Contribution of the rows of C*}
  \item{cj}{Inner Product: Contribuition of the columns of C*}
  \item{eigs}{Inner Product: Eigen Values of C*}
  \item{eigs.vector}{Inner Product: Eigen Vectors of C*}
  \item{eigenValue}{Inner Product: Eigen Value of C*}
  \item{fi}{Inner Product: Factor Scores of C*}
  \item{tau}{Inner Product: Percent Variance Explained of C*}
  \item{alphaWeights}{Inner Product: Alpha Weights *}

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
  \item{table.loadings}{Table: Loadings}
  \item{table.fi}{Table: Factor Scores}
  \item{table.partial.fi}{Table: Partial Factor Scores}
  \item{table.partial.fi.array}{Table: Array of Partial Factor Scores}
  \item{table.tau}{Table: Percent Variance Explained}
  }
  
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpKPlus1STATIS}}, \code{\link{mpSTATIS}}
}
\keyword{ multivariate}
