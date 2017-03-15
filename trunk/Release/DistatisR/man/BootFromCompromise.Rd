\name{BootFromCompromise}
%\alias{BootFactorScores}
\alias{BootFromCompromise}
%\alias{distatis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute observation Bootstrap replicates of the factor scores
	 from bootstrapped compromises
%
}
\description{
	\code{BootFactorSCores} computes Bootstrap replicates of the factor scores
	of the observations from bootstrapped compromises.
	The input is obtained from the same input as the \code{distatis} function,
	the output is a 3-way array of dimensions 
	"number of observations by number of factors by number of replicates." 
	The output is typically used to plot confidence intervals 
	(i.e., ellipsoids or convex hulls) 
	or to compute \eqn{t}-like statistic called \emph{bootstrap ratios}.
% Description of what the function does.
}
\usage{
BootFromCompromise(LeCube2Distance, 
                   niter = 1000, Norm = "MFA", 
                   Distance = TRUE, RV = TRUE, 
                   nfact2keep = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LeCube2Distance}{The array of distance used to call \code{distatis}
}
  \item{niter}{The number of bootstrap iterations (default = 1000)
}
  \item{Norm}{should be the same as for the original call to \code{distatis}
}
  \item{Distance}{should be the same as for the original call to \code{distatis}
}
  \item{RV}{should be the same as for the original call to \code{distatis}
}
  \item{nfact2keep}{number of factors to keep for the results
}
}

\details{
	To compute a bootstrapped sample a set of \eqn{K} distance matrices is 
	selected with replacement from the original set of \eqn{K} distance matrices.
	A \code{distatis} compromise is then computed
	and projected on the factor space of the original solution
	to obtain the bootstrapped factor scores.
	This approach is also called \emph{total boostrap} by Lebart 
	(2007, see also Chateau and Lebart 1996, 
	see also Abdi \emph{et al}., 2009 for an example). 
	Compared to the partial bootstrap (see help for \code{BootFactorScores})
	It has the desadvantage of being
	slow espcially for large data sets but
	recent work (Cadoret & Husson, 2012) 
	suggests that partial boostrap  (i.e., computed from the partial factor scores)
	could lead to optimistic bootstrap estimates when
	the number of distance matrices is large and that
	it is preferable to use instead
	the \emph{total boostrap}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	the output is a 3-way array of dimensions 
	"number of observations by number of factors by number of replicates."	
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
	 Abdi, H., & Valentin, D., (2007).
	  Some new and easy ways to describe, compare, and evaluate products and assessors. 
	  In D., Valentin, D.Z. Nguyen, L. Pelletier (Eds) 
	  \emph{New trends in sensory evaluation of food and non-food products}. 
	  Ho Chi Minh (Vietnam): Vietnam National University-Ho chi Minh City Publishing House. pp. 5-18.
	 
		 Abdi, H., Dunlop, J.P., & Williams, L.J. (2009). How to compute reliability estimates and display confidence and tolerance intervals for pattern classiffers using the Bootstrap and 3-way multidimensional scaling (DISTATIS). \emph{NeuroImage}, \bold{45}, 89--95.
		 		 
		Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, \bold{4}, 124--167.

These papers are available from  \url{www.utdallas.edu/~herve}

Additional references:

Cadoret, M.,   Husson, F. (2012)
Construction and evaluation of confidence ellipses applied at sensory data.
\emph{Food Quality and Preference},  \bold{28},  106--115.

Chateau, F., & Lebart, L. (1996). Assessing sample variability in the visualization
techniques related to principal component analysis: Bootstrap and alternative
simulation methods. In A. Prats (Ed.),\emph{Proceedings of COMPSTAT 2006.}
Heidelberg: Physica Verlag.

Lebart, L. (2007). Which bootstrap for principal axes methods? 
In \emph{Selected contributions in data analysis and classification,
	 COMPSTAT 2006}. 
Heidelberg: Springer Verlag.


%% ~put references to the literature/web site here ~
}
\author{Herve Abdi
}
%\note{
%%   further note
%}

% Other sections like Warning with \section{Warning }{....} \

\seealso{\code{\link{BootFactorScores}}
	     \code{\link{GraphDistatisBoot}} 
%  See Also as \code{\link{help}}, \
}
\examples{
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
# 1. Load the Sort data set from the SortingBeer example 
#    (available from the DistatisR package)
data(SortingBeer)
# Provide the "8 beers by 10 assessors" results of a sorting task
#-----------------------------------------------------------------------------
# 2. Create the set of distance matrices (one distance matrix per assessor)
#    (uses the function DistanceFromSort)
DistanceCube <- DistanceFromSort(Sort)

#-----------------------------------------------------------------------------
# 3. Call the distatis function with the cube of distance as parameter
testDistatis <- distatis(DistanceCube)
# The factor scores for the beers are in
# testDistatis$res4Splus$F
# the partial factor scores for the beers for the assessors are in 
#  testDistatis$res4Splus$PartialF
# 
# 4. Get the bootstraped factor scores (with default 1000 iterations)
#    Here we use the "total bootstrap"
 F_fullBoot <- BootFromCompromise(DistanceCube,niter=1000) 
}
\keyword{sample}
\keyword{bootstrap}
