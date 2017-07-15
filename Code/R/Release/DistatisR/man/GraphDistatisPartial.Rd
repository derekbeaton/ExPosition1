\name{GraphDistatisPartial}
\alias{GraphDistatisPartial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot maps of the factor scores and partial factor scores of the observations for a \acronym{DISTATIS} analysis.
}
\description{
	\code{GraphDistatisPartial} plots  maps of the factor scores of the observations 
	from a \code{\link{distatis}} analysis. \code{GraphDistatisPartial} gives a map of the factors scores of the observations plus partial factor scores, as "seen" by each of the matrices.
}
\usage{
GraphDistatisPartial(FS, PartialFS, axis1 = 1, axis2 = 2, constraints = NULL, 
	item.colors = NULL, participant.colors = NULL, ZeTitle = "Distatis-Partial", 
	Ctr=NULL, color.by.observations = TRUE, nude = FALSE, lines = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FS}{The factor scores of the observations (\code{$res4Splus$F}from \code{distatis}).}
  \item{PartialFS}{The partial factor scores of the observations (\code{$res4Splus$PartialF} from \code{distatis})}
  \item{axis1}{The dimension for the horizontal axis of the plots.}
  \item{axis2}{The dimension for the vertical axis of the plots.}
  \item{constraints}{constraints for the axes}
  \item{item.colors}{A \eqn{I\time 1}{I*1} matrix  (with \eqn{I} = # observations) 
  	of color names for the observations. If NULL (default), \code{prettyGraphs} chooses.}
  \item{participant.colors}{A \eqn{I\time 1}{I*1} matrix  (with \eqn{I} = # participants) 
  	of color names for the observations. If NULL (default), \code{prettyGraphs} chooses. 
    }
  \item{ZeTitle}{General title for the plots.}
  \item{Ctr}{Contributions of each observation. If NULL (default), these are computed from FS}  
  \item{color.by.observations}{if \code{TRUE} (default), the partial factor scores are colored by \code{item.colors}. When \code{FALSE}, \code{participant.colors} are used.}
  \item{nude}{When \code{nude} is \code{TRUE} the labels for the observations are not plotted (useful when editing the graphs for publication).}
  \item{lines}{If \code{TRUE} (default) then lines are drawn between the partial factor score of an observation and the compromise factor score of the observation.}
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
\item{participant.colors}{A set of colors for the participants are returned.}
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
\seealso{\code{\link{GraphDistatisAll}} \code{\link{GraphDistatisCompromise}} \code{\link{GraphDistatisPartial}} \code{\link{GraphDistatisBoot}}  \code{\link{GraphDistatisRv}}  \code{\link{distatis}}
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
 GraphDistatisPartial(DistatisAlgo$res4Splus$F,DistatisAlgo$res4Splus$PartialF)
}
\keyword{DistatisR}
\keyword{mds}

