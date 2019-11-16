\name{epMCA}
\alias{epMCA}
\encoding{UTF-8}
\title{
epMCA: Multiple Correspondence Analysis (MCA) via ExPosition.
}
\description{
Multiple Correspondence Analysis (MCA) via ExPosition.
}
\usage{
epMCA(DATA, make_data_nominal = TRUE, DESIGN = NULL, make_design_nominal = TRUE, 
	masses = NULL, weights = NULL, hellinger = FALSE, 
	symmetric = TRUE, correction = c("b"), graphs = TRUE, k = 0)
}
\arguments{
	\item{DATA}{original data to perform a MCA on. This data can be in original formatting (qualitative levels) or in dummy-coded variables.}
  \item{make_data_nominal}{a boolean. If TRUE (default), DATA is recoded as a dummy-coded matrix. If FALSE, DATA is a dummy-coded matrix.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{masses}{a diagonal matrix or column-vector of masses for the row items.}
  \item{weights}{a diagonal matrix or column-vector of weights for the column it}
  \item{hellinger}{a boolean. If FALSE (default), Chi-square distance will be used. If TRUE, Hellinger distance will be used.}
  \item{symmetric}{a boolean. If TRUE symmetric factor scores for rows.}
  \item{correction}{which corrections should be applied? "b" = Benzécri correction, "bg" = Greenacre adjustment to Benzécri correction.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
}
\details{
	\code{epMCA} performs multiple correspondence analysis. Essentially, a CA for categorical data. \cr
	It should be noted that when \code{hellinger} is selected as TRUE, no correction will be performed. Additionally, if you decide to use Hellinger, it is best to set \code{symmetric} to FALSE.
}
\value{
	See \code{\link{coreCA}} for details on what is returned. In addition to the values returned:\cr
	\item{$pdq}{this is the corrected SVD data, if a correction was selected. If no correction was selected, it is uncorrected.}
	\item{$pdq.uncor}{uncorrected SVD data.}		
}
\references{
Abdi, H., and Williams, L.J. (2010). Principal component analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 2, 433-459.\cr
Abdi, H., and Williams, L.J. (2010). Correspondence analysis. In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr
Abdi, H. (2007). Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr
Benzécri, J. P. (1979). Sur le calcul des taux d'inertie dans l'analyse d'un questionnaire. \emph{Cahiers de l'Analyse des Données}, \bold{4}, 377-378.\cr
Greenacre, M. J. (2007). Correspondence Analysis in Practice. \emph{Chapman and Hall}.
}
\author{
Derek Beaton
}

\seealso{
	\code{\link{coreCA}}, \code{\link{epCA}}, \code{\link{mca.eigen.fix}}
}
\examples{
	data(mca.wine)
	mca.wine.res <- epMCA(mca.wine$data)
}
\keyword{ multivariate }
