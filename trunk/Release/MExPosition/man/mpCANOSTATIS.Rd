\name{mpCANOSTATIS}
\alias{mpCANOSTATIS}
\encoding{UTF-8}
\title{mpCANOSTATIS: Canonical STATIS (CANOSTATIS) via MExPosition}
\description{All CANOSTATIS steps are combined in this function. It enables preparation of the data, processing and graphing.}
\usage{mpCANOSTATIS(data, column.design, row.design, normalization = 'MFA', 
row.preprocess = 'None', column.preprocess = 'Center_1Norm', table.preprocess ='Sum_PCA', 
make.columndesign.nominal = TRUE, make.rowdesign.nominal = TRUE, DESIGN = NULL, 
make.design.nominal = TRUE, graphs = TRUE)}
\arguments{
  \item{data}{Matrix of data}
  \item{column.design}{Column Design- used to identifty the tables of the data matrix}
  \item{row.design}{Row Design - used to identify the groups of the data matrix}
  \item{normalization}{String option: 'None', 'MFA' (default), \emph{or} 'Sum_PCA'}
  \item{row.preprocess}{String option: 'None' (default), 'Profile', 'Hellinger', 'Center' \emph{or} 'Center_Hellinger'}
  \item{column.preprocess}{String option: 'None', 'Center', '1Norm', 'Center_1Norm' (default) \emph{or} 'Z_Score'}
  \item{table.preprocess}{String option: 'None','Num_Columns','Tucker','Sum_PCA' (default), 'RV_Normalization' \emph{or} 'MFA_Normalization'}
  \item{make.columndesign.nominal}{Boolean option. If TRUE (default), the matrix will be nominalized}
  \item{make.rowdesign.nominal}{Boolean option. If TRUE (default), the matrix will be nominalized}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make.design.nominal}{Boolean option. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
  \item{graphs}{Boolean option. If TRUE (default), graphs are displayed}

}
\details{
Computation of Canonical STATIS (CANOOSTATIS), where the observations come from predefined groups and tables.
}
\value{
Returns a large list of items which are divided into four categories: 
  \item{$Overview}{Overview of Results}
  \item{$InnerProduct}{Results for the Inner Product}
  \item{$Compromise}{Results for the Compromise}
  \item{$Table}{Results for the Tables}
The results for Overview are bundled inside of $Overview.
  \item{$Overview$data}{Data Matrix}
  \item{$Overview$groupmatrix}{Matrix used to identify the different tables of the data matrix}
  \item{$Overview$row.design}{Matrix used to identify the groups of the data matrix}
  \item{$Overview$preprocess.data}{Preprocessed data matrix}
  \item{$Overview$num.groups}{Number of Tables}
  \item{$Overview$num.obs}{Number of Observations}
  \item{$Overview$row.preprocess}{Row Preprocess Option used}
  \item{$Overview$column.preprocess}{Column Preprocess Option used}
  \item{$Overview$Table.preprocess}{Table Preprocess Option used}
The results for InnerProduct are bundled inside of $InnerProduct
  \item{mahalanobis}{Mahalanobis distance matrices}
  \item{$InnerProduct$S}{Inner Product: Scalar Product Matrices}
  \item{$InnerProduct$C}{Inner Product: C Matrix}
  \item{$InnerProduct$RVMatrix}{Inner Product: RV Matrix}
  \item{$InnerProduct$eigs.vector}{Inner Product: Eigen Vectors}
  \item{$InnerProduct$eigs}{Inner Product: Eigen Values}
  \item{$InnerProduct$fi}{Inner Product: Factor Scores}
  \item{$InnerProduct$t}{Inner Product: Percent Variance Explained}
  \item{$InnerProduct$ci}{Inner Product: Contribution of the Rows}
  \item{$InnerProduct$cj}{Inner Product: Contribution of the Columns}
  \item{$InnerProduct$alphaWeights}{Alpha Weights}
The results for the Compromise are bundled inside of $Compromise
  \item{$Compromise$compromise}{Compromise Matrix}
  \item{$Compromise$compromise.eigs}{Compromise: Eigen Values}
  \item{$Compromise$compromise.eigs.vector}{Compromise: Eigen Vector}
  \item{$Compromise$compromise.fi}{Compromise: Factor Scores}
  \item{$Compromise$compromise.t}{Compromise: Percent Variance Explained}
  \item{$Compromise$compromise.ci}{Compromise: Contributions of the rows}
  \item{$Compromise$compromise.cj}{Compromise: Contributions of the Columns}
The results for the Tables are bundled inside of $Table.
  \item{$Table$m}{Table: masses}
  \item{$Table$eigs}{Table: Eigen Values}
  \item{$Table$eigs.vector}{Table: Eigen Vectors}
  \item{$Table$Q}{Table: Loadings}
  \item{$Table$fi}{Table: Factor Scores}
  \item{$Table$partial.fi}{Table: Partial Factor Scores}
  \item{$Table$partial.fi.array}{Table: Arrray of Partial Factor Scores}
  \item{$Table$ci}{Table: Contribition of the Rows}
  \item{$Table$cj}{Table: Contribution of the Columns}
  \item{$Table$t}{Table: Percent Variance Explained}
}
  
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpCANOSTATIS.core}}, \code{\link{mpCANOSTATIS}}
}
\examples{
# CANOSTATIS
  data('wines2012')
  row.design = c('NZ','NZ','NZ','NZ','FR','FR','FR','FR','CA','CA','CA','CA')
	column.design = wines2012$table
	demo.canostatis.2012 <- mpCANOSTATIS(wines2012$data,column.design, row.design, 
  DESIGN = row.design)
}
\keyword{ multivariate }