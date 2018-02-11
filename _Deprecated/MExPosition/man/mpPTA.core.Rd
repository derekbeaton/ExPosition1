\name{mpPTA.core}
\alias{mpPTA.core}
\encoding{UTF-8}
\title{mpPTA.core: Core Function for Partial Triadic Analysis (PTA) via MExPosition}
\description{Performs the core of PTA}
\usage{mpPTA.core(data, num.obs, column.design, num.groups, optimization.option = 'STATIS')}
\arguments{
  \item{data}{Matrix of dataset}
  \item{num.obs}{Number of observations in dataset}
  \item{column.design}{Column Design for dataset}
  \item{num.groups}{Number of groups in dataset}
  \item{optimization.option}{String option of either 'None', 'Multiable', 'RV_Matrix', 'STATIS' (DEFAULT), \emph{or} 'STATIS_Power1'}
}
\details{
Computation of Partial Triadic Analyis (PTA). This function should not be used independently. It should be used with \code{\link{mpPTA}}.
}

\value{
  \item{S}{Inner Product: Scalar Product Matrices}
  \item{RVMatrix}{Inner Product:RV Matrix}
  \item{C}{Inner Product: C Matrix}
  \item{ci}{Inner Product: Contribution of the rows of C}
  \item{cj}{Inner Product: Contribuition of the columns of C}
  \item{eigs}{Inner Product: Eigen Values of C}
  \item{eigs.vector}{Inner Product: Eigen Vectors of S}
  \item{eigenValue}{Inner Product: Eigen Value}
  \item{fi}{Inner Product: Factor Scores}
  \item{tau}{Inner Product: Percent Variance Explained}
  \item{alphaWeights}{Inner Product: Alpha Weights}

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
  \code{\link{mpPTA}}
}
\keyword{ multivariate}
