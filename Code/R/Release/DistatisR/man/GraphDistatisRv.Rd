\name{GraphDistatisRv}
\alias{GraphDistatisRv}
\title{
	Plot maps of the factor scores (from the Rv matrix) of the distance matrices for a \acronym{DISTATIS} analysis
}
\description{
		Plot  maps of the factor scores of the observations for a \acronym{distatis}
analysis. 
The factor scores are obtained from the eigen-decomposition of the
between distance matrices cosine matrix (often a matrix of Rv coefficients).
Note that the factor scores for the first dimension are always positive.
There are used to derive the \eqn{\alpha}{alpha} weights for 
\acronym{DISTATIS}.
}
\usage{
GraphDistatisRv(RvFS, axis1 = 1, axis2 = 2, ZeTitle = "Distatis-Rv Map", 
	participant.colors = NULL, nude = FALSE, RvCtr = NULL)
}
\arguments{
  \item{RvFS}{The factor scores of the distance matrices (\code{$res4Cmat$G} from \code{distatis}).}
  \item{axis1}{The dimension for the horizontal axis of the plots.
    }
  \item{axis2}{The dimension for the vertical axis of the plots.
    }
  \item{ZeTitle}{General title for the plots.
  	}
  \item{participant.colors}{A \eqn{I\time 1}{I*1} matrix  (with \eqn{I} = # participants) 
  	of color names for the observations. If NULL (default), \code{prettyGraphs} chooses. 
    }
  \item{nude}{When \code{nude} is \code{TRUE} the labels for the observations are not plotted (useful when editing the graphs for publication).}
  \item{RvCtr}{Contributions of each participant. If NULL (default), these are computed from RvFS}
}
\details{	Note that, in the current version, the graphs are plotted as R-plots
	and are \emph{not} passed back by the routine.
	So the graphs need to be saved "by hand" from the R graphic windows.
	We plan to improve this in a future version.
}
\value{
\item{constraints}{A set of plot constraints that are returned.}
\item{participant.colors}{A set of colors for the participants are returned.}
}
\references{
		The plots are similar to the graphs described in:
		
	Abdi, H., Valentin, D., O'Toole, A.J., & Edelman, B. (2005).
	 DISTATIS: The analysis of multiple distance matrices. 
	 \emph{Proceedings of the IEEE Computer Society: International Conference on Computer Vision and Pattern Recognition}. 
	 (San Diego, CA, USA). pp. 42-47.

		Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, \bold{4}, 124--167.
		
		 Abdi, H., Dunlop, J.P., & Williams, L.J. (2009). How to compute reliability estimates and display confidence and tolerance intervals for pattern classiffers using the Bootstrap and 3-way multidimensional scaling (DISTATIS). \emph{NeuroImage}, \bold{45}, 89--95.
		
		Abdi, H., & Valentin, D., (2007). Some new and easy ways to describe, compare, and evaluate products and assessors. In D., Valentin, D.Z. Nguyen, L. Pelletier (Eds) 
		\emph{New trends in sensory evaluation of food and non-food products}.
		 Ho Chi Minh (Vietnam): Vietnam National University-Ho chi Minh City Publishing House. pp. 5--18.
		 
The  \eqn{R_V} coefficient is described in 

 Abdi, H. (2007). RV coefficient and congruence coefficient. 
 In N.J. Salkind (Ed.): 
 \emph{Encyclopedia of Measurement and Statistics}. Thousand Oaks (CA): Sage. pp. 849--853.	
 
 Abdi, H. (2010). Congruence: Congruence coefficient, RV coefficient, and Mantel Coefficient. 
 In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): 
 \emph{Encyclopedia of Research Design.} Thousand Oaks (CA): Sage. pp. 222--229.	 
		
		These papers are available from  \url{www.utdallas.edu/~herve}

}
\author{Derek Beaton and Herve Abdi
}

\seealso{\code{\link{GraphDistatisAll}} \code{\link{GraphDistatisCompromise}} \code{\link{GraphDistatisPartial}} \code{\link{GraphDistatisBoot}}  \code{\link{GraphDistatisRv}} \code{\link{distatis}}
}
\examples{
# 1. Load the DistAlgo data set (available from the DistatisR package)
data(DistAlgo)
# DistAlgo is a 6*6*4 Array (faces*faces*Algorithms)
#-----------------------------------------------------------------------------
# 2. Call the DISTATIS routine with the array of distance (DistAlgo) as parameter
DistatisAlgo <- distatis(DistAlgo)
# 3. Plot the compromise map with the labels for the first 2 dimensions
# DistatisAlgo$res4Cmat$G are the factors scores 
#  for the 4 distance matrices (i.e., algorithms)
 GraphDistatisRv(DistatisAlgo$res4Cmat$G,ZeTitle='Rv Mat')
# Et voila!
}
\keyword{DistatisR}