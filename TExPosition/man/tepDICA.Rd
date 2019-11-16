\name{tepDICA}
\alias{tepDICA}
\encoding{UTF-8}
\title{
Discriminant Correspondence Analysis
}
\description{
Discriminant Correspondence Analysis (DICA) via TExPosition.
}
\usage{
tepDICA(DATA, make_data_nominal = FALSE, DESIGN = NULL, make_design_nominal = TRUE, 
symmetric = TRUE, graphs = TRUE, k = 0)
}
\arguments{
  \item{DATA}{original data to perform a DICA on. Data can be contingency (like CA) or categorical (like MCA).}
  \item{make_data_nominal}{a boolean. If TRUE (default), DATA is recoded as a dummy-coded matrix. If FALSE, DATA is a dummy-coded matrix.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups. Required for DICA.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{symmetric}{a boolean. If TRUE (default) symmetric factor scores for rows.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{tepGraphs}})}
  \item{k}{number of components to return.}
}
\details{
	If you use Hellinger distance, it is best to set \code{symmetric} to FALSE.\cr\cr
	Note: DICA is a special case of PLS-CA (\code{\link{tepPLSCA}}) wherein DATA1 are data and DATA2 are a group-coded disjunctive matrix.
}
\value{
	See \code{\link{epCA}} (and also \code{\link{coreCA}}) for details on what is returned. In addition to the values returned:\cr
	\item{fii}{factor scores computed for supplemental observations}
	\item{dii}{squared distances for supplemental observations}
	\item{rii}{cosines for supplemental observations}
	\item{assign}{a list of assignment data. See \code{\link{fii2fi}} and \code{\link{R2}}}
	\item{lx}{latent variables from DATA1 computed for observations}
	\item{ly}{latent variables from DATA2 computed for observations}		
}
\references{
Abdi, H., and Williams, L.J. (2010). Principal component analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 2, 433-459.\cr
Abdi, H. and Williams, L.J. (2010). Correspondence analysis. In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr
Abdi, H. (2007). Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr
Abdi, H. (2007). Discriminant correspondence analysis. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}. Thousand Oaks (CA): Sage. pp. 270-275. \cr
Pinkham, A.E., Sasson, N.J., Beaton, D., Abdi, H., Kohler, C.G., Penn, D.L. (in press, 2012). Qualitatively distinct factors contribute to elevated rates of paranoia in autism and schizophrenia. \emph{Journal of Abnormal Psychology}, 121, -.\cr
Williams, L.J., Abdi, H., French, R., & Orange, J.B. (2010). A tutorial on Multi-Block Discriminant Correspondence Analysis (MUDICA): A new method for analyzing discourse data from clinical populations. \emph{Journal of Speech Language and Hearing Research}, 53, 1372-1393. \cr
Williams, L.J., Dunlop, J.P., & Abdi, H. (2012). Effect of age on the variability in the production of text-based global inferences. \emph{PLoS One}, 7(5): e36161. doi:10.1371/ journal.pone.0036161 (pp.1-9)
}
\author{
Derek Beaton, Hervé Abdi
}

\seealso{
\code{\link{coreCA}}, \code{\link{epCA}}, \code{\link{epMCA}} \cr
}
\examples{
data(dica.wine)
dica.res <- tepDICA(dica.wine$data,DESIGN=dica.wine$design,make_design_nominal=FALSE)
}
\keyword{ multivariate }
