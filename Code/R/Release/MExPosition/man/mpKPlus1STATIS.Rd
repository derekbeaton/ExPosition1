\name{mpKPlus1STATIS}
\alias{mpKPlus1STATIS}
\encoding{UTF-8}
\title{mpKPlus1STATIS: Function for (K+1) STATIS via MExPosition}
\description{All (K+1) STATIS steps are combined in this function. It enables preparation of the data, processing and graphing.}
\usage{mpKPlus1STATIS(data, plus1data, column.design, make.columndesign.nominal = TRUE, 
  row.preprocess = 'None', column.preprocess = 'Center', table.preprocess = 'Sum_PCA', 
  optimization.option = 'STATIS', 
  DESIGN = NULL, make.design.nominal = TRUE, graphs = TRUE)}
\arguments{
  \item{data}{Data Matrix}
  \item{plus1data}{External table}
  \item{column.design}{Column Design for data - used to identifty the tables of the data matrix}
  \item{make.columndesign.nominal}{Boolean option. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
   \item{row.preprocess}{String option: 'None' (default), 'Profile', 'Hellinger', 'Center' \emph{or} 'Center_Hellinger'}
  \item{column.preprocess}{String option: 'None', 'Center' (default), '1Norm', 'Center_1Norm'  \emph{or} 'Z_Score'}
  \item{table.preprocess}{String option: 'None','Num_Columns','Tucker','Sum_PCA' (default), 'RV_Normalization' \emph{or} 'MFA_Normalization'}
  \item{optimization.option}{String option of either 'None', 'Multiable', 'RV_Matrix', 'STATIS' (DEFAULT), \emph{or} 'STATIS_Power1'}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make.design.nominal}{Boolean option. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
  \item{graphs}{Boolean option. If TRUE (default), graphs are displayed}
}
\details{
Computation of (K+1) STATIS. 
}

\value{
Returns a large list of items which are divided into four categories: 
  \item{$Overview}{Overview of Results}
  \item{$InnerProduct}{Results for the Inner Product}
  \item{$Compromise}{Results for the Compromise}
  \item{$Table}{Results for the Tables}
The results for Overview are bundled inside of $Overview.
  \item{$Overview$data}{Data Matrix}
  \item{$Overview$plus1data}{Preprocessed external table}
  \item{$Overview$column.design}{Column Design for dataset}  
  \item{$Overview$row.preprocess}{Row Preprocess Option used}
  \item{$Overview$column.preprocess}{Column Preprocess Option used}
  \item{$Overview$Table.preprocess}{Table Preprocess Option used} 
  \item{$Overview$num.groups}{Number of Groups in dataset}
 The results for InnerProduct are bundled inside of $InnerProduct
  \item{$InnerProduct$S}{Inner Product: Scalar Product Matrices of dataset}
  \item{$InnerProduct$S.star}{Inner Product: Scalar Product Matrices * of dataset}
  \item{$InnerProduct$rvMatrix.star}{Inner Product: RV Matrix *}
  \item{$InnerProduct$C}{Inner Product: C Matrix of S*}
  \item{$InnerProduct$ci}{Inner Product: Contribution of the rows of C*}
  \item{$InnerProduct$cj}{Inner Product: Contribuition of the columns of C*}
  \item{$InnerProduct$eigs}{Inner Product: Eigen Values of C*}
  \item{$InnerProduct$eigs.vector}{Inner Product: Eigen Vectors of C*}
  \item{$InnerProduct$eigs}{Inner Product: Eigen Value of C*}
  \item{$InnerProduct$fi}{Inner Product: Factor Scores of C*}
  \item{$InnerProduct$t}{Inner Product: Percent Variance Explained of C*}
  \item{$InnerProduct$alphaWeights}{Inner Product: Alpha Weights *}
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
  \code{\link{mpKPlus1STATIS}}, \code{\link{mpSTATIS}}
}
\examples{
  #(K+1) STATIS
  data('wines2012')
	data=wines2012$data
	chemical <- wines2012$supplementary
	design=c('NZ','NZ','NZ','NZ','FR','FR','FR','FR','CA','CA','CA','CA')
    
  demo.plus1 <- mpKPlus1STATIS(wines2012$data,chemical,wines2012$table)
}
\keyword{ multivariate}
