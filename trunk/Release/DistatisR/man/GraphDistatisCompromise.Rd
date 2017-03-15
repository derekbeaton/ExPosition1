\name{GraphDistatisCompromise}
\alias{GraphDistatisCompromise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot maps of the factor scores of the observations for a \acronym{DISTATIS} analysis
}
\description{
	Plot  maps of the factor scores of the observations for a \acronym{distatis}
analysis. \code{GraphDistatis} gives a map of the factor scores for the observations. The labels of the observations are plotted by defaults but can be omitted
(see the \code{nude=TRUE} option).
}
\usage{
GraphDistatisCompromise(FS, axis1 = 1, axis2 = 2, constraints = NULL, item.colors = NULL, 
	ZeTitle = "Distatis-Compromise", nude = FALSE, Ctr = NULL)
}
\arguments{
  \item{FS}{The factor scores of the observations (\code{$res4Splus$F}from \code{distatis}).}
  \item{axis1}{The dimension for the horizontal axis of the plots.}
  \item{axis2}{The dimension for the vertical axis of the plots.}
  \item{constraints}{constraints for the axes}
  \item{item.colors}{A \eqn{I\time 1}{I*1} matrix  (with \eqn{I} = # observations) 
  	of color names for the observations. If NULL (default), \code{prettyGraphs} chooses.}
  \item{ZeTitle}{General title for the plots.}
  \item{nude}{When \code{nude} is \code{TRUE} the labels for the observations are not plotted (useful when editing the graphs for publication).}
  \item{Ctr}{Contributions of each observation. If NULL (default), these are computed from FS}
}
\details{
	Note that, in the current version, the graphs are plotted as R-plots
	and are \emph{not} passed back by the routine.
	So the graphs need to be saved "by hand" from the R graphic windows.
	We plan to improve this in a future version.
}
\value{
\item{constraints}{A set of plot constraints that are returned.}
\item{item.colors}{A set of colors for the observations are returned.}
}
\references{
	The plots are similar to the graphs from 
	
	Abdi, H., Valentin, D., O'Toole, A.J., & Edelman, B. (2005).
	 DISTATIS: The analysis of multiple distance matrices. 
	 \emph{Proceedings of the IEEE Computer Society: International Conference on Computer Vision and Pattern Recognition}. 
	 (San Diego, CA, USA). pp. 42-47.
	 
	 see \url{www.utdallas.edu/~herve}
}
\author{
	Derek Beaton and Herve Abdi
}

\seealso{\code{\link{GraphDistatisAll}} \code{\link{GraphDistatisCompromise}} \code{\link{GraphDistatisPartial}} \code{\link{GraphDistatisBoot}} \code{\link{GraphDistatisRv}} \code{\link{distatis}}
}		
\examples{
# 1. Load the DistAlgo data set (available from the DistatisR package)
data(DistAlgo)
# DistAlgo is a 6*6*4 Array (face*face*Algorithm)
#-----------------------------------------------------------------------------
# 2. Call the DISTATIS routine with the array of distance (DistAlgo) as parameter
DistatisAlgo <- distatis(DistAlgo)
# 3. Plot the compromise map with the labels for the first 2 dimensions
# DistatisAlgo$res4Splus$F are the factors scores for the 6 observations (i.e., faces)
# DistatisAlgo$res4Splus$PartialF are the partial factors scores 
	##(i.e., one set of factor scores per algorithm)
 GraphDistatisCompromise(DistatisAlgo$res4Splus$F)
}
\keyword{DistatisR}
\keyword{mds}
