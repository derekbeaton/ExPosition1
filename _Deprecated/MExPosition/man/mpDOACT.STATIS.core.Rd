\name{mpDOACT.STATIS.core}
\alias{mpDOACT.STATIS.core}
\encoding{UTF-8}
\title{mpDOACT.STATIS.core: Core Function for Dual STATIS (DO-ACT) via MExPosition}
\description{Performs the core of Dual STATIS on two given dataset}
\usage{mpDOACT.STATIS.core(dataset1, column.design.1, dataset2, column.design.2)}
\arguments{
  \item{dataset1}{Matrix of dataset 1}
  \item{column.design.1}{Column Design for dataset 1 - used to identifty the tables of the data matrix}
  \item{dataset2}{Matrix of dataset 2}
  \item{column.design.2}{Column Design for dataset 2 - used to identifty the tables of the data matrix}
}
\details{
Computation of DualSTATIS (DOSTATIS). This function should not be used independently. It should be used with \code{\link{mpDOACT.STATIS}}
}

\value{
  \item{S.1}{Inner Product: Scalar Product Matrices of dataset1}
  \item{S.2}{Inner Product: Scalar Product Matrices of dataset2}
  \item{C}{Inner Product: C Matrix}
  \item{ci}{Inner Product: Contribution of the rows of C}
  \item{cj}{Inner Product: Contribuition of the columns of C}
  \item{eigs}{Inner Product: Eigen Values of C}
  \item{eigs.vector}{Inner Product: Eigen Vectors of S}
  \item{eigenValue}{Inner Product: Eigen Value}
  \item{fi}{Inner Product: Factor Scores}
  \item{tau}{Inner Product: Percent Variance Explained}
  \item{alphaWeights}{Inner Product: Alpha Weights}
  \item{betaWeights}{Inner Product: Beta Weights}

  \item{compromiseMatrix.1}{Compromise Matrix for dataset 1}
  \item{compromise.eigs.1}{Compromise: Eigen Values for dataset 1}
  \item{compromise.eigs.vector.1}{Compromise: Eigen Vector for dataset 1}
  \item{compromise.fi.1}{Compromise: Factor Scores for dataset 1}
  \item{Compromise.tau.1}{Compromise: Percent Variance Explained for dataset 1}
  \item{compromise.ci.1}{Compromise: Contributions of the rows for dataset 1}
  \item{compromise.cj.1}{Compromise: Contributions of the Columns for dataset 1}
  
  \item{compromiseMatrix.2}{Compromise Matrix for dataset 2}  
  \item{compromise.eigs.2}{Compromise: Eigen Values for dataset 2}
  \item{compromise.eigs.vector.2}{Compromise: Eigen Vector for dataset 2}
  \item{compromise.fi.2}{Compromise: Factor Scores for dataset 2}
  \item{Compromise.tau.2}{Compromise: Percent Variance Explained for dataset 2}
  \item{compromise.ci.2}{Compromise: Contributions of the rows for dataset 2}
  \item{compromise.cj.2}{Compromise: Contributions of the Columns for dataset 2}

  \item{masses.1}{Table: masses for dataset 1}
  \item{table.eigs.1}{Table: Eigen Values for dataset 1}
  \item{table.eigs.vector.1}{Table: Eigen Vectors for dataset 1}
  \item{table.loadings.1}{Table: Loadings for dataset 1}
  \item{table.fi.1}{Table: Factor Scores for dataset 1}
  \item{table.partial.fi.1}{Table: Partial Factor Scores for dataset 1}
  \item{table.partial.fi.array.1}{Table: Array of Partial Factor Scores for dataset 1}
  \item{table.tau.1}{Table: Percent Variance Explained for dataset 1}
  
  \item{masses.2}{Table: masses for dataset 2}
  \item{table.eigs.2}{Table: Eigen Values for dataset 2}
  \item{table.eigs.vector.2}{Table: Eigen Vectors for dataset 2}
  \item{table.loadings.2}{Table: Loadings for dataset 2}
  \item{table.fi.2}{Table: Factor Scores for dataset 2}
  \item{table.partial.fi.2}{Table: Partial Factor Scores for dataset 2}
  \item{table.partial.fi.array.2}{Table: Array of Partial Factor Scores for dataset 2}
  \item{table.tau.2}{Table: Percent Variance Explained for dataset 2}
  }
  
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpSTATIS}}, \code{\link{mpDOACT.STATIS}}
}
\keyword{ multivariate}
