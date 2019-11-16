\name{mpCOVSTATIS}
\alias{mpCOVSTATIS}
\encoding{UTF-8}
\title{mpCOVSTATIS: Core Function for COVSTATIS via MExPosition}
\description{All COVSTATIS steps are combined in this function. It enables preparation of the data, processing and graphing.}
\usage{mpCOVSTATIS(data, normalization = 'None', masses = NULL, table = NULL, 
make.table.nominal = TRUE, DESIGN = NULL, make.design.nominal = TRUE, graphs = TRUE)}
\arguments{
  \item{data}{Matrix of preprocessed data}
  \item{normalization}{String option of either 'None', 'MFA' (DEFAULT), \emph{or} 'Sum_PCA'}
  \item{masses}{Masses}
  \item{table}{Design Matrix - used to identifty the tables of the data matrix}
  \item{make.table.nominal}{a boolean. If TRUE (default), table is a vector that indicates tables (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
    \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make.design.nominal}{Boolean option. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
  \item{graphs}{Boolean option. If TRUE (default), graphs are displayed}
}
\details{
COVSTATIS is used to analysis covariance matrices. It is an extension of three-way multidimensional scaling.
}

\value{
Returns a large list of items which are divided into four categories: 
  \item{$Overview}{Overview of Results}
  \item{$InnerProduct}{Results for the Inner Product}
  \item{$Compromise}{Results for the Compromise}
  \item{$Table}{Results for the Tables}
The results for Overview are bundled inside of $Overview.
  \item{$Overview$data}{Data Matrix}
  \item{$Overview$normalization}{Type of normalization used}
  \item{$Overview$table}{Matrix used to identify the different tables of the data matrix}
  \item{$Overview$num.groups}{Number of Tables}
The results for InnerProduct are bundled inside of $InnerProduct
  \item{$InnerProduct$S}{Inner Product: Scalar Product Matrices}
  \item{$InnerProduct$C}{Inner Product: C Matrix}
  \item{$InnerProduct$rvMatrix}{Inner Product: RV Matrix}
  \item{$InnerProduct$eigs.vector}{Inner Product: Eigen Vectors}
  \item{$InnerProduct$eigs}{Inner Product: Eigen Values}
  \item{$InnerProduct$fi}{Inner Product: Factor Scores}
  \item{$InnerProduct$t}{Inner Product: Percent Variance Explained}
  \item{$InnerProduct$ci}{Inner Product: Contribution of the Rows}
  \item{$InnerProduct$cj}{Inner Product: Contribution of the Columns}
  \item{$InnerProduct$alphaWeights}{Alpha Weights}  
The results for the Compromise are bundled inside of $Compromise
  \item{compromise}{Compromise Matrix}
  \item{compromise.eigs}{Compromise: Eigen Values}
  \item{compromise.eigs.vector}{Compromise: Eigen Vector}
  \item{compromise.fi}{Compromise: Factor Scores}
  \item{Compromise.t}{Compromise: Percent Variance Explained}
  \item{compromise.ci}{Compromise: Contributions of the rows}
  \item{compromise.cj}{Compromise: Contributions of the Columns}  
The results for the Tables are bundled inside of $Table.
  \item{$m}{Table: masses}
  \item{$Table$eigs}{Table: Eigen Values}
  \item{$Table$eigs.vector}{Table: Eigen Vectors}
  \item{$Table$Q}{Table: Loadings}
  \item{$Table$fi}{Table: Factor Scores}
  \item{$Table$partial.fi}{Table: Partial Factor Scores}
  \item{$Table$partial.fi.array}{Table: Arrray of Partial Factor Scores}
  \item{Table$ci}{Table: Contribition of the Rows}
  \item{$Table$cj}{Table: Contribution of the Columns}
  \item{$Table$t}{Table: Percent Variance Explained} 
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
  #COVSTATIS
   data('faces2005')
   table = c('pixel','pixel','pixel','pixel','pixel','pixel', 
   'distance','distance','distance','distance','distance','distance', 
   'ratings','ratings','ratings','ratings','ratings','ratings',
   'similarity','similarity','similarity','similarity','similarity','similarity')
   demo.covstatis.2005 <- mpCOVSTATIS(faces2005$data, table = table)
}
\keyword{ multivariate}
