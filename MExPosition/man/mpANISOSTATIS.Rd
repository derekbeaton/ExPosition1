\name{mpANISOSTATIS}
\alias{mpANISOSTATIS}
\title{mpANISOSTATIS.core: ANISOSTATIS via MExPositio}
\description{All ANISOSTATIS steps are combined in this function. It enables preparation of the data, processing and graphing.}
\usage{mpANISOSTATIS(data, anisostatis.option = 'ANISOSTATIS_Type1', column.design, 
make.columndesign.nominal = TRUE, DESIGN =NULL, make.design.nominal = TRUE, graphs = TRUE)}
\arguments{
  \item{data}{Data Matrix}
  \item{anisostatis.option}{ANISOSTATIS string ptions: 'ANISOSTATIS_Type1' \emph{or} 'ANISOSTATIS_Type2'}
  \item{column.design}{Matrix used to identify tables of data matrix}
  \item{make.columndesign.nominal}{a boolean. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make.design.nominal}{Boolean option. If TRUE (default), table is a vector that indicates groups (and will be dummy-coded). If FALSE, table is a dummy-coded matrix.}
  \item{graphs}{Boolean option. If TRUE (default), graphs are displayed}
}
\details{
 \code{mpANISOSTATIS} computes Anisotropic STATIS, where the one weight is assigned per variable. 
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
  \item{$Overview$preprocess.data}{Preprocessed data matrix}
  \item{$Overview$num.groups}{Number of Tables}
  \item{$Overview$num.obs}{Number of Observations}
  \item{$Overview$row.preprocess}{Row Preprocess Option used}
  \item{$Overview$column.preprocess}{Column Preprocess Option used}
  \item{$Overview$Table.preprocess}{Table Preprocess Option used}
The results for InnerProduct are bundled inside of $InnerProduct
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
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167. \cr \cr
Abdi, H., Valentin, D., Chollet, S., & Chrea, C. (2007). Analyzing assessors and products in sorting tasks: DISTATIS, theory and applications. \emph{Food Quality and Preference}, 18, 627-640.\cr \cr
Abdi, H., & Valentin, D. (2005). DISTATIS: the analysis of multiple distance matrices. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}. Thousand Oaks (CA): Sage. pp. 284-290. \cr
}
\author{Cherise R. Chin Fatt \email{cherise.chinfatt@utdallas.edu}
}
\seealso{
  \code{\link{mpANISOSTATIS.core}}
}
\examples{
# ANOISTATIS Type 1
  data('wines2012')
	data = wines2012$data
	column.design = wines2012$table
	row.design= c('NZ','NZ','NZ','NZ','FR','FR','FR','FR','CA','CA','CA','CA')
	demo.anisostatis1 <- mpANISOSTATIS(data,anisostatis.option='ANISOSTATIS_Type1',
  column.design = column.design)

# ANISOSTATISType 2
  data('wines2012')
	data = wines2012$data
	column.design = wines2012$table
	row.design = c('NZ','NZ','NZ','NZ','FR','FR','FR','FR','CA','CA','CA','CA')
	demo.anisostatis2 <- mpANISOSTATIS(data,anisostatis.option='ANISOSTATIS_Type2',
  column.design = column.design)
}
\keyword{ multivariate }
