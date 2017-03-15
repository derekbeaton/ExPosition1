\name{mpDOACT.STATIS}
\alias{mpDOACT.STATIS}
\encoding{UTF-8}
\title{mpDOACT.STATIS: Function for Dual STATIS (DO-ACT) via MExPosition}
\description{All DO-ACT steps are combined in this function. It enables preparation of the data, processing and graphing.}
\usage{mpDOACT.STATIS(data1, column.design.1, make.columndesign.1.nominal = TRUE, 
  data2, column.design.2, make.columndesign.2.nominal = TRUE, 
  row.preprocess.data1 = 'None', column.preprocess.data1 = 'Center', 
  table.preprocess.data1 = 'Sum_PCA', 
  row.preprocess.data2 = 'None', column.preprocess.data2 = 'Center', 
  table.preprocess.data2 = 'Sum_PCA', 
  DESIGN = NULL, make.design.nominal = TRUE, 
  graphs = TRUE)}
\arguments{
  \item{data1}{Matrix of dataset 1}
  \item{column.design.1}{Column Design for dataset 1 - used to identifty the tables of the data matrix}
  \item{make.columndesign.1.nominal}{Boolean option. If TRUE (default), the matrix will be nominalized}
  \item{data2}{Matrix of dataset 2}
  \item{column.design.2}{Column Design for dataset 2 - used to identifty the tables of the data matrix}
  \item{make.columndesign.2.nominal}{Boolean option. If TRUE (default), the matrix will be nominalized}
  \item{row.preprocess.data1}{String option: 'None' (default), 'Profile', 'Hellinger', 'Center' \emph{or} 'Center_Hellinger'}
  \item{column.preprocess.data1}{String option: 'None', 'Center', '1Norm', 'Center_1Norm' (default) \emph{or} 'Z_Score'}
  \item{table.preprocess.data1}{String option: 'None','Num_Columns','Tucker','Sum_PCA' (default), 'RV_Normalization' \emph{or} 'MFA_Normalization'}
  \item{row.preprocess.data2}{String option: 'None' (default), 'Profile', 'Hellinger', 'Center' \emph{or} 'Center_Hellinger'}
  \item{column.preprocess.data2}{String option: 'None', 'Center', '1Norm', 'Center_1Norm' (default) \emph{or} 'Z_Score'}
  \item{table.preprocess.data2}{String option: 'None','Num_Columns','Tucker','Sum_PCA' (default), 'RV_Normalization' \emph{or} 'MFA_Normalization'}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make.design.nominal}{Boolean option. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
  \item{graphs}{Boolean option. If TRUE (default), graphs are displayed}
}
\details{
Computation of DualSTATIS (DOSTATIS).
}

\value{
Returns a large list of items which are divided into four categories: 
  \item{$Overview}{Overview of Results}
  \item{$InnerProduct}{Results for the Inner Product}
  \item{$Compromise}{Results for the Compromise}
  \item{$Table}{Results for the Tables}
The results for Overview are bundled inside of $Overview.
  \item{$Overview$data1}{Data Matrix for dataset 1}
  \item{$Overview$column.design.1}{Column Design for dataset1}  
  \item{$Overview$row.preprocess.data1}{Row Preprocess Option used for dataset1}
  \item{$Overview$column.preprocess.data1}{Column Preprocess Option used for dataset1}
  \item{$Overview$Table.preprocess.data1}{Table Preprocess Option used for dataset1} 
  \item{$Overview$num.groups.1}{Number of Groups in dataset1}
  \item{$Overview$data2}{Data Matrix for dataset 2}
  \item{$Overview$column.design.2}{Column Design for dataset2}
  \item{$Overview$row.preprocess.data2}{Row Preprocess Option used for dataset2}
  \item{$Overview$column.preprocess.data2}{Column Preprocess Option used for dataset2}
  \item{$Overview$Table.preprocess.data2}{Table Preprocess Option used for dataset2}
  \item{$Overview$num.groups.2}{Number of Groups in dataset 2}
The results for InnerProduct are bundled inside of $InnerProduct
  \item{$InnerProduct$S.1}{Inner Product: Scalar Product Matrices for dataset 1}
  \item{$InnerProduct$S.2}{Inner Product: Scalar Product Matrices for dataset 2}
  \item{$InnerProduct$C}{Inner Product: C Matrix}
  \item{$InnerProduct$RVMatrix}{Inner Product: RV Matrix}
  \item{$InnerProduct$eigs.vector}{Inner Product: Eigen Vectors}
  \item{$InnerProduct$eigs}{Inner Product: Eigen Values}
  \item{$InnerProduct$fi}{Inner Product: Factor Scores}
  \item{$InnerProduct$t}{Inner Product: Percent Variance Explained}
  \item{$InnerProduct$ci}{Inner Product: Contribution of the Rows}
  \item{$InnerProduct$cj}{Inner Product: Contribution of the Columns}
  \item{$InnerProduct$alphaWeights}{Inner Product: Alpha Weights}
  \item{$InnerProduct$betaWeights}{Inner Product: Beta Weights}
The results for the Compromise are bundled inside of $Compromise
  \item{$Compromise$compromiseMatrix.1}{Compromise Matrix for dataset 1}
  \item{$Compromise$compromise.eigs.1}{Compromise: Eigen Values for dataset 1}
  \item{$Compromise$compromise.eigs.vector.1}{Compromise: Eigen Vector for dataset 1}
  \item{$Compromise$compromise.fi.1}{Compromise: Factor Scores for dataset 1}
  \item{$Compromise$compromise.t.1}{Compromise: Percent Variance Explained for dataset 1}
  \item{$Compromise$compromise.ci.1}{Compromise: Contributions of the rows for dataset 1}
  \item{$Compromise$compromise.cj.1}{Compromise: Contributions of the Columns for dataset 1}
  \item{$Compromise$compromiseMatrix.2}{Compromise Matrix for dataset 2}
  \item{$Compromise$compromise.eigs.2}{Compromise: Eigen Values for dataset 2}
  \item{$Compromise$compromise.eigs.vector.2}{Compromise: Eigen Vector for dataset 2}
  \item{$Compromise$compromise.fi.2}{Compromise: Factor Scores for dataset 2}
  \item{$Compromise$compromise.t.2}{Compromise: Percent Variance Explained for dataset 2}
  \item{$Compromise$compromise.ci.2}{Compromise: Contributions of the rows for dataset 2}
  \item{$Compromise$compromise.cj.2}{Compromise: Contributions of the Columns for dataset 2}
The results for the Tables are bundled inside of $Table.
 \item{$Table$m.1}{Table: masses for dataset 1}
  \item{$Table$eigs.1}{Table: Eigen Values for dataset 1}
  \item{$Table$eigs.vector.1}{Table: Eigen Vectors for dataset 1}
  \item{$Table$Q.1}{Table: Loadings for dataset 1}
  \item{$Table$fi.1}{Table: Factor Scores for dataset 1}
  \item{$Table$partial.fi.1}{Table: Partial Factor Scores for dataset 1}
  \item{$Table$partial.fi.array.1}{Table: Arrray of Partial Factor Scores for dataset 1}
  \item{$Table$ci.1}{Table: Contribition of the Rows for dataset 1}
  \item{$Table$cj.1}{Table: Contribution of the Columns for dataset 1}
  \item{$Table$t.1}{Table: Percent Variance Explained for dataset 1}  
   \item{$Table$m.2}{Table: masses for dataset 2}
  \item{$Table$eigs.2}{Table: Eigen Values for dataset 2}
  \item{$Table$eigs.vector.2}{Table: Eigen Vectors for dataset 2}
  \item{$Table$Q.2}{Table: Loadings for dataset 2}
  \item{$Table$fi.2}{Table: Factor Scores for dataset 2}
  \item{$Table$partial.fi.2}{Table: Partial Factor Scores for dataset 2}
  \item{$Table$partial.fi.array.2}{Table: Arrray of Partial Factor Scores for dataset 2}
  \item{$Table$ci.2}{Table: Contribition of the Rows for dataset 2}
  \item{$Table$cj.2}{Table: Contribution of the Columns for dataset 2}
  \item{$Table$t.2}{Table: Percent Variance Explained for dataset 2}
  }
  
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt and Hervé Abdi.
}
\seealso{
  \code{\link{mpSTATIS}}, \code{\link{mpDOACT.STATIS}}
}
\examples{
   #DO-ACT
   data('wines2012')
   design=c('NZ','NZ','NZ','NZ','FR','FR','FR','FR','CA','CA','CA','CA')
   data1 <- wines2012$data
   data2 <- wines2012$data
   design.1 <- wines2012$table
   design.2 <- wines2012$table

   demo.double <- mpDOACT.STATIS(data1=data1,column.design.1=design.1, data2=data2, 
   column.design.2=design.2, DESIGN=design)   
}
\keyword{ multivariate}
