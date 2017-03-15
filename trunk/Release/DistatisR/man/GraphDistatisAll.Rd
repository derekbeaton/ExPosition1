\name{GraphDistatisAll}
\alias{GraphDistatisAll}
\title{
This function combines the functionality of \code{\link{GraphDistatisCompromise}}, \code{\link{GraphDistatisPartial}}, \code{\link{GraphDistatisBoot}}, and  \code{\link{GraphDistatisRv}}.
}
\description{
This function produces 4 plots: (1) a compromise plot, (2) a partial factor scores plot, (3) a bootstrap confidence intervals plot, and (4) a Rv map.
}
\usage{
GraphDistatisAll(FS, PartialFS, FBoot, RvFS, axis1 = 1, axis2 = 2, constraints = NULL, 
	item.colors = NULL, participant.colors = NULL, ZeTitleBase = NULL, nude = FALSE, 
	Ctr = NULL, RvCtr=NULL, color.by.observations = TRUE, lines = TRUE, 
	lwd = 3.5, ellipses = TRUE, fill = TRUE, fill.alpha = 0.27, percentage = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FS}{The factor scores of the observations (\code{$res4Splus$F}from \code{distatis})}
  \item{PartialFS}{The partial factor scores of the observations (\code{$res4Splus$PartialF} from \code{distatis})}
  \item{FBoot}{is the bootstrapped factor scores array (\code{FBoot} obtained from \code{\link{BootFactorScores}} or \code{\link{BootFromCompromise}})}  
  \item{RvFS}{The factor scores of the distance matrices (\code{$res4Cmat$G} from \code{distatis})}
  \item{axis1}{The dimension for the horizontal axis of the plots.}
  \item{axis2}{The dimension for the vertical axis of the plots.}
  \item{constraints}{constraints for the axes}
  \item{item.colors}{A \eqn{I\time 1}{I*1} matrix  (with \eqn{I} = # observations) 
  	of color names for the observations. If NULL (default), \code{prettyGraphs} chooses.}
  \item{participant.colors}{A \eqn{I\time 1}{I*1} matrix  (with \eqn{I} = # participants) 
  	of color names for the observations. If NULL (default), \code{prettyGraphs} chooses. 
    }
  \item{ZeTitleBase}{General title for the plots.}
  \item{nude}{When \code{nude} is \code{TRUE} the labels for the observations are not plotted (useful when editing the graphs for publication).}
  \item{Ctr}{Contributions of each observation. If NULL (default), these are computed from FS}
  \item{RvCtr}{Contributions of each participant. If NULL (default), these are computed from RvFS}  
  \item{color.by.observations}{if \code{TRUE} (default), the partial factor scores are colored by \code{item.colors}. When \code{FALSE}, \code{participant.colors} are used.}
  \item{lines}{If \code{TRUE} (default) then lines are drawn between the partial factor score of an observation and the compromise factor score of the observation.}
  \item{lwd}{
Thickness of the line plotting the ellipse or hull.
}
  \item{ellipses}{a boolean. When \code{TRUE} will plot ellipses (from \code{car} package). When \code{FALSE} will plot peeled hulls (from \code{prettyGraphs} package).
}
  \item{fill}{when \code{TRUE}, fill in the ellipse with color. Related to ellipses only.}
  \item{fill.alpha}{transparency index when filling in the ellipses.  Related to ellipses only.}
  \item{percentage}{A value to determine the percent coverage of the bootstrap partial factor scores to provide ellipse or hull confidence intervals.}  
}
\value{
\item{constraints}{A set of plot constraints that are returned.}
\item{item.colors}{A set of colors for the observations are returned.}
\item{participant.colors}{A set of colors for the participants are returned.}
}
\author{
Derek Beaton and Herve Abdi
}

\seealso{\code{\link{GraphDistatisAll}} \code{\link{GraphDistatisCompromise}} \code{\link{GraphDistatisPartial}} \code{\link{GraphDistatisBoot}}  \code{\link{GraphDistatisRv}} \code{\link{distatis}} 
}
\examples{
# 1. Load the Sort data set from the SortingBeer example  (available from the DistatisR package)
data(SortingBeer)
# Provide an 8 beers by 10 assessors results of a sorting task
#-----------------------------------------------------------------------------
# 2. Create the set of distance matrices (one distance matrix per assessor)
#    (ues the function DistanceFromSort)
DistanceCube <- DistanceFromSort(Sort)

#-----------------------------------------------------------------------------
# 3. Call the DISTATIS routine with the cube of distance as parameter
testDistatis <- distatis(DistanceCube)
# The factor scores for the beers are in
# testDistatis$res4Splus$F
# the partial factor score for the beers for the assessors are in 
#  testDistatis$res4Splus$PartialF
# 
# 4. Get the bootstraped factor scores (with default 1000 iterations)
BootF <- BootFactorScores(testDistatis$res4Splus$PartialF)
#-----------------------------------------------------------------------------
# 5. Create the Graphics with GraphDistatisAll
#
GraphDistatisAll(testDistatis$res4Splus$F,testDistatis$res4Splus$PartialF,
	BootF,testDistatis$res4Cmat$G)
}
\keyword{ distatis }
\keyword{ mds }
