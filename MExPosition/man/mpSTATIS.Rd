\name{mpSTATIS}
\alias{mpSTATIS}
\title{mpSTATIS: STATIS via MExPosition}
\description{All STATIS steps are combined in this function. It enables preprocessing, processing, optimization and supplementary projections which is computed using the STATIS method of analysis.}
\usage{mpSTATIS(data, column.design, make.columndesign.nominal = TRUE, 
  row.design = NULL, make.rowdesign.nominal = FALSE, 
  statis.prepro.option = 'Plain_STATIS', DESIGN = NULL, 
  make.design.nominal = TRUE, graphs = TRUE)}
\arguments{
  \item{data}{Matrix of raw data}
  \item{column.design}{Matrix which identifies the different tables.}
  \item{make.columndesign.nominal}{a boolean. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
  \item{row.design}{Matrix which identifes the different groups.}
  \item{make.rowdesign.nominal}{a boolean. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
  \item{statis.prepro.option}{String option for the STATIS presets. The following options are available: 'Plain_STATIS', 'MFA', 'Sum_PCA', 'Plain_Multitable', 'Plain_ANISOSTATIS' and 'Customization.'}
  \item{graphs}{a boolean. If TRUE (default), graphs are displayed}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make.design.nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, design is a dummy-coded matrix.}
}
\details{
 \code{mpSTATIS} performs STATIS on a set of data matrices measured on the same set of observations. \cr\cr
 
 If statis.prepro.option is set to 'Customization,' the options for row, column, table prepreprocessing and optimization will be selected via the R console. 
}
\value{
Returns a large list of items which are divided into three categories 
  \item{$Overview}{Overview of Results}
  \item{$InnerProduct}{Results for the Inner Product}
  \item{$Table}{Results for the Tables}
The results for Overview are bundled inside of $Overview. 
  \item{$Overview$data}{Data Matrix}
  \item{$Overview$groupmatrix}{Table which indicates the tables}
  \item{$Overview$preprocess.data}{Preprocessed Data Matrix}
  \item{$Overview$num.groups}{Number of Groups}
  \item{$Overview$num.obs}{Number of Observations}
  \item{$Overview$row.preprocess}{Option of row preprocessing selected}
  \item{$Overview$column.preprocess}{Option of column preprocessing selected}
  \item{$Overview$table.preprocess}{Option of table preprocessing selected}
The results for InnerProduct are bundled inside of $InnerProduct
  \item{$InnerProduct$S}{Inner Product: Scalar Product Matrices}
  \item{$InnerProduct$RVMatrix}{Inner Product: RV Matrix}
  \item{$InnerProduct$C}{Inner Product: C Matrix}
  \item{$InnerProduct$eigs.vector}{Inner Product: Eigen Vectors}
  \item{$InnerProduct$eigs}{Inner Product: Eigen Values}
  \item{$InnerProduct$fi}{Inner Product: Factor Scores}
  \item{$InnerProduct$t}{Inner Product: Percent Variance Explained (tau)}
  \item{$InnerProduct$alphaWeights}{Alpha Weights (alpha)}
The results for the Compromise are bundled inside of $Compromise
  \item{$Compromise$compromise}{Compromise Matrix}
  \item{$Compromise$compromise.eigs}{Compromise: Eigen Values}
  \item{$Compromise$compromise.eigs.vector}{Compromise: Eigen Vector}
  \item{$Compromise$compromise.fi}{Compromise: Factor Scores}
  \item{$Compromise$compromise.t}{Compromise: Percent Variance Explained}
  \item{$Compromise$compromise.ci}{Compromise: Contributions of the rows}
  \item{$Compromise$compromise.cj}{Compromise: Contributions of the Columns}
The results for the Tables are bundled inside of $Table.
  \item{$Table$eigs}{Table: Eigen Values}
  \item{$Table$eigs.vector}{Table: Eigen Vectors}
  \item{$Table$Q}{Table: Loadings}
  \item{$Table$fi}{Table: Factor Scores}
  \item{$Table$partial.fi}{Table: Partial Factor Scores}
  \item{$Table$partial.fi.array}{Table: Array of Partial Factor Scores}
  \item{$Table$ci}{Table: Contribution of the rows}
  \item{$Tabl$cj}{Table: Contribution of the columns}
  \item{$Table$t}{Table: Percent of variance explained}
}
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4. \cr \cr
Abdi, H., & Valentin, D. (2007). STATIS. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}. Sage. pp. 955-962.
}
\author{Cherise R. Chin Fatt \email{cherise.chinfatt@utdallas.edu} and Derek Beaton
}
\seealso{
  \code{\link{mpDISTATIS}}
}
\examples{
data('wines2012')
design=c('NZ','NZ','NZ','NZ','FR','FR','FR','FR','CA','CA','CA','CA')
demo.statis.2012 <- mpSTATIS(wines2012$data, column.design = wines2012$table, 
statis.prepro.option = 'Plain_STATIS', DESIGN = design, graphs = TRUE )
}
\keyword{ multivariate }
