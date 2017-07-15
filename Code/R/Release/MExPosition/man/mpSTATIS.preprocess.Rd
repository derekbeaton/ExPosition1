\name{mpSTATIS.preprocess}
\alias{mpSTATIS.preprocess}
\alias{mpSTATIS.preprocess}
\title{mpSTATIS.preprocess: Preprocessing for STATIS}
\description{Combines all preprocessing choices, and prepares the data for STAITS processing.}
\usage{mpSTATIS.preprocess(data, column.design = NULL, row.design = NULL, 
  row.preprocess = 'None', column.preprocess = 'None', table.preprocess = 'None', 
  make.columndesign.nominal = TRUE, make.rowdesign.nominal = TRUE)}
\arguments{
  \item{data}{Data Matrix}
  \item{column.design}{Matrix which identifies the tables.}
  \item{row.design}{Matrix which identifies the groups}
  \item{row.preprocess}{String option for row preprocessing with the following options: 'None' (default), 'Profile', 'Hellinger', 'Center' and 'Center_Hellinger'}
  \item{column.preprocess}{String option for column preprocessing with the following options: 'None' (default), 'Center', '1Norm', 'Center_1Norm' and 'Z_Score'}
  \item{table.preprocess}{String option for table preprocessing with the following options: 'None' (default), 'Num_Columns', 'Tucker', 'Sum_PCA', 'RV_Normalization' and 'MFA_Normalization'}
  \item{make.columndesign.nominal}{a boolean. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
    \item{make.rowdesign.nominal}{a boolean. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
}
\details{
This function calls all the preprocessing functions and consolidates the results. In addition it prepares the group matrix, and gets the data ready for processing. 
}
\value{
  \item{data.preprocessed}{Matrix of the Preprocessed Data}
  \item{num.obs}{Number of Observations}
  \item{col.groups}{Original matrix which was selected in the initial step}
  \item{groupMatrix}{Matrix which identifies the Tables}
  \item{numgroups}{Number of Tables}
  \item{table.ids}{Table IDs}
  \item{row.preprocess}{Option of row preprocessing selected}
  \item{column.preprocess}{Option of column preprocessing selected}
  \item{table.preprocess}{Option of table preprocessing selected}
}  
\references{
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167
}
\author{Cherise R. Chin Fatt \email{cherise.chinfatt@utdallas.edu}
}
\seealso{
  \code{\link{mpSTATIS.rowPreproc}}, \code{\link{mpSTATIS.columnPreproc}}, \code{\link{mpSTATIS.tablePreproc}}
}
\examples{
X <- matrix(1:10,2)
Y<- as.matrix(c('g1','g1','g1','g2','g2'))
row.preprocess='Center'
column.preprocess='Center'
table.preprocess='Sum_PCA'
preproc <-mpSTATIS.preprocess(X, column.design = t(Y), row.preprocess = row.preprocess,
 column.preprocess = column.preprocess, table.preprocess = table.preprocess)
}
\keyword{ multivariate }