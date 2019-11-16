\name{tepGPLS}
\alias{tepGPLS}
\encoding{UTF-8}
\title{
	Generalized Partial Least Squares
}
\description{
	Generalized Partial Least Squares (GPLS) via TExPosition. GPLS is to PLS (\code{\link{tepPLS}}) as PCA \code{\link{epPCA}} is to GPCA \code{\link{epGPCA}}.
	The major difference between PLS and GPLS is that GPLS allows the use of weights for the columns of each data set (just like GPCA).
}
\usage{
tepGPLS(DATA1, DATA2, 
center1 = TRUE, scale1 = "SS1",
center2 = TRUE, scale2 = "SS1",
DESIGN = NULL, make_design_nominal = TRUE,
weights1 = NULL, weights2 = NULL,
graphs = TRUE, k = 0)
}
\arguments{
  \item{DATA1}{Data matrix 1 (X)}
  \item{DATA2}{Data matrix 2 (Y)}
  \item{center1}{a boolean, vector, or string to center \code{DATA1}. See \code{\link{expo.scale}} for details.}
  \item{scale1}{a boolean, vector, or string to scale \code{DATA1}. See \code{\link{expo.scale}} for details.}  
  \item{center2}{a boolean, vector, or string to center \code{DATA2}. See \code{\link{expo.scale}} for details.} 
  \item{scale2}{a boolean, vector, or string to scale \code{DATA2}. See \code{\link{expo.scale}} for details.}   
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{weights1}{a weight vector (or diag matrix) for the columns of DATA1.}
  \item{weights2}{a weight vector (or diag matrix) for the columns of DATA2.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{tepGraphs}})}
  \item{k}{number of components to return.}
}
\value{
	See \code{\link{epGPCA}} (and also \code{\link{corePCA}}) for details on what is returned. In addition to the values returned:\cr
	\item{lx}{latent variables from DATA1 computed for observations}
	\item{ly}{latent variables from DATA2 computed for observations}	
	\item{data1.norm}{center and scale information for DATA1}
	\item{data1.norm}{center and scale information for DATA2}
}
\details{
	This implementation of Partial Least Squares is a symmetric analysis. It was first described by Tucker (1958), again by Bookstein (1994), and has gained notoriety in Neuroimaging from McIntosh et al., (1996). This particular implementation allows the user to provide weights for the columns of both \code{DATA1} and \code{DATA2}.
}
\references{
	Tucker, L. R. (1958). An inter-battery method of factor analysis. \emph{Psychometrika}, \emph{23}(2), 111--136. \cr
	Bookstein, F., (1994). Partial least squares: a dose–response model for measurement in the behavioral and brain sciences. \emph{Psycoloquy} \emph{5} (23) \cr
Abdi, H. (2007). Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr
	Krishnan, A., Williams, L. J., McIntosh, A. R., & Abdi, H. (2011). Partial Least Squares (PLS) methods for neuroimaging: A tutorial and review. \emph{NeuroImage}, \emph{56}(\bold{2}), 455 -- 475.\cr
	McIntosh, A. R., & Lobaugh, N. J. (2004). Partial least squares analysis of neuroimaging data: applications and advances. \emph{Neuroimage}, \emph{23}, S250--S263.\cr
}
\author{
Derek Beaton
}

\seealso{
	\code{\link{corePCA}}, \code{\link{epPCA}}, \code{\link{epGPCA}},  \code{\link{tepPLS}}, \code{\link{tepPLSCA}}, \code{\link{tepBADA}}, \code{\link{tepDICA}}
}
\examples{
data(beer.tasting.notes)
data1<-beer.tasting.notes$data[,1:8]
data2<-beer.tasting.notes$data[,9:16]
gpls.res <- tepGPLS(data1,data2)
}
\keyword{ multivariate }
