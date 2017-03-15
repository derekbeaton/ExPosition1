\name{mmds}
\alias{mmds}
\title{mmds Metric (classical) Multidimensional Scaling (a.k.a Principal Coordinate Analysis) of a (Euclidean) Distance Matric}
\description{
	Perform an MMDS of a (Euclidean) distance matrix measured between a set of weighted objects.
	
	MMDS Give factor scores that make it possible to 
	draw a map of the objects such that the distances between objects on the 
	map best approximate the original distances between objects. 
	
	\bold{Method:} Transform the distance matrix into a (double centered) 
	covariance matrix which is then analyze via its eigen-decomposition.
	The factor score of each dimension are scaled such that their variance
	(i.e., the sum ot their weighted squared factor scores) is
	equal to the eigen-value of the corresponding dimension.
	Note that if the \code{masses} vector is absent, equal masses (i.e. 1 divided by number of objects) 
	are used. 
}


\usage{mmds(DistanceMatrix,masses=NULL)
	}
	\arguments{
  \item{DistanceMatrix}{.
 A squared (assumed to be Euclidean) distance matrix
}
  \item{masses}{A vector of masses (i.e.,
  	non negative numbers with a sum of 1) 
  	of same dimensionality as number of rows of 
  	\code{DistanceMatrix}.}
  }
  %\details{}
 \value{
 	Sends back a list
 	\item{LeF}{factor scores for the objects.}
 	\item{eigenvalues}{the eigenvalues for the factor scores (ie.a variance).}
 	\item{tau}{the percentage of explained variance of each dimension. }
 	\item{Contributions}{give the proporion of explained variance by an object for a dimension.}
 } 
 \references{
 	The procedure and references are detailled in:
 	Abdi, H. (2007). Metric multidimensional scaling. 
 	In N.J. Salkind (Ed.): 
 	\emph{Encyclopedia of Measurement and Statistics}. Thousand Oaks (CA): Sage. pp. 598--605.
 	
 		(Paper available from  \url{www.utdallas.edu/~herve}).
 	}
  
\author{Herve Abdi}
\seealso{ \code{\link{GraphDistatisCompromise}} \code{\link{distatis}}
		}
\examples{
# An example of MDS from Abdi (2007)
# Discriminability of Brain States
# Table 1.
# 1. Get the distance matrix
D <- matrix(c(
0.00, 3.47, 1.79, 3.00, 2.67, 2.58, 2.22, 3.08,
3.47, 0.00, 3.39, 2.18, 2.86, 2.69, 2.89, 2.62,
1.79, 3.39, 0.00, 2.18, 2.34, 2.09, 2.31, 2.88,
3.00, 2.18, 2.18, 0.00, 1.73, 1.55, 1.23, 2.07,
2.67, 2.86, 2.34, 1.73, 0.00, 1.44, 1.29, 2.38,
2.58, 2.69, 2.09, 1.55, 1.44, 0.00, 1.19, 2.15,
2.22, 2.89, 2.31, 1.23, 1.29, 1.19, 0.00, 2.07,
3.08, 2.62, 2.88, 2.07, 2.38, 2.15, 2.07, 0.00),
ncol = 8, byrow=TRUE)
rownames(D) <- c('Face','House','Cat','Chair','Shoe','Scissors','Bottle','Scramble')
colnames(D) <- rownames(D)
# 2. Call mmds
BrainRes <- mmds(D)
# Note that compared to Abdi (2007)
# the factor scores of mmds are equal to F / sqrt(nrow(D))
# the eigenvalues of mmds are equal to \Lambda *{1/nrow(D)}
# (ie., the normalization differs but the results are proportional)
# 3. Now a pretty plot with the prettyPlot function from prettyGraphs
prettyPlot(BrainRes$FactorScore, 
           display_names = TRUE,
           display_points = TRUE,
           contributionCircles = TRUE,
          contributions = BrainRes$Contributions)
# 4. et Voila!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{DistatisR}
% \keyword{ kwd2 }% __ONLY ONE__ keyword per line

