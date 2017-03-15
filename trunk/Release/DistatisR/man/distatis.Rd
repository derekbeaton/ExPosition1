\name{distatis}
\alias{distatis}
\alias{DiSTATIS}
\alias{CovSTATIS}
\alias{covstatis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{\code{distatis} 3-Way MDS based on the \acronym{STATIS} optimization 
	procedure
%%  ~~function to do ... ~~
}
\description{ Implements the \acronym{DISTATIS} method which
	a 3-way generalization of metric multidimensional scaling (\emph{a.k.a.}
	classical MDS or principal coordinate analysis).
	 \code{distatis}
	takes a set of \eqn{K} distance matrices  describing
	a set of \eqn{I} observations and computes (1) a set of factor scores
	that describes the similarity structure of the distance matrices
	(e.g., what distance matrices describe the observations 
	in the same way, what distance matrices
	differ from each other) (2) a set of factor scores 
	(called the \emph{compromise} factor scores) for the observations
	that best describes the similarity structure of the observations and (3)
	partial factor scores that show how each individual distance matrix "sees" the
	compromise space.  
	\code{distatis} computes the compromise as an optimum
	linear combination of the cross-product matrices associated to each distance matrix.
	\code{distatis} 
	can also be applied to a set of covariance matrices. 
%  what the function does. 
}
\usage{
distatis(LeCube2Distance, Norm = "MFA", 
         Distance = TRUE, RV = TRUE, 
         nfact2keep = 3, 
         compact = FALSE)
}
%   Maybe also 'usage' for other objects documented here.
\arguments{
  \item{LeCube2Distance}{an "observations \eqn{\times}{*} observations  \eqn{\times}{*} 
  	distance matrices" array of dimensions \eqn{I\times I \times K}{I*I*K}.
  	Each of the \eqn{K} "slices" is a \eqn{I\times I}{I*I} 
  	square distance (or covariance) matrix describing the
  	\eqn{I} observations.
    }
  \item{Norm}{Type of normalization used for each cross-product matrix derived from
  	the distance (or covariance) matrices. 
  	Current options are \code{NONE} (do nothing)
  	or \code{MFA} (\code{default}
  	) that normalizes each matrix so that 
  	its first eigenvalue is equal to one.
    }
  \item{Distance}{if TRUE (\code{default}) 
  	the matrices are distance matrices,
  	if FALSE they are covariance matrices.
    }
  \item{RV}{TRUE (\code{default}) we use the \eqn{R_V}{Rv} 
  	coefficient to compute the \eqn{\alpha}{weights},
  	FALSE we use the matrix scalar product
    }
  \item{nfact2keep}{Numner of factors to keep for the computation
  	of the factor scores of the observations.
    }
  \item{compact}{if FALSE we provide detailled output, if TRUE
  	we send back only the \eqn{\alpha}{alpha} weights 
  	(this option is used to make the bootstrap routine
  	 \code{\link{BootFromCompromise}}
  	more efficient).
}
}
\details{\acronym{DISTATIS} is part of the \acronym{STATIS} family.
	It is often used to analyze the results of sorting tasks.
	
}
\value{
%  Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
% ...
\code{distatis} sends back the results \emph{via} two lists:
 \code{res.Cmat}
and \code{res.Splus}.
Note that
items with a * are the only ones sent back when using the \code{compact = TRUE} option

\item{res.Cmat}{Results for the between distance matrices analysis.}
\itemize{
	\item \code{res.Cmat$C} 
	The \eqn{I\times I}{I*I} 
	\bold{C} matrix of scalar products (or \eqn{R_V}{Rv} between distance matrices). 
	\item \code{res.Cmat$vectors} The eigenvectors of the \bold{C} matrix
	\item \code{res.Cmat$alpha} * The \eqn{\alpha}{alpha} weights
	\item \code{res.Cmat$value}  The eigenvalues of the \bold{C} matrix
	\item \code{res.CmatG} The factor scores for the \bold{C} matrix
}
\item{res.Splus}{Results for the between observation analysis.}
\itemize{
	\item \code{res.Splus$SCP} an \eqn{I\times I\times K}{I*I*K} array. Contains the
	 (normalized if needed) cross product matrices corresponding to the distance matrices.
	\item \code{res.Splus$Splus} * The compromise (linear combination of the SCP's')
	\item \code{res.Splus$ProjectionMatrix} The projection matrix used to compute
	    factor scores and partial factor scores.
	\item \code{res.Splus$F} The factor scores for the observations.
    \item \code{res.Splus$PartialF} an \eqn{I\times \code{nf2keep} \times K}{I*nf2keep*K} 
       array. 
       Contains the partial factors
           for the distance matrices.
	} 
}
\references{
		Abdi, H., Valentin, D., O'Toole, A.J., & Edelman, B. (2005).
	 DISTATIS: The analysis of multiple distance matrices. 
	 \emph{Proceedings of the IEEE Computer Society: 
	 	International Conference on Computer Vision and Pattern Recognition}. 
	 (San Diego, CA, USA). pp. 42--47.
		 
		 Abdi, H., Valentin, D., Chollet, S., & Chrea, C. (2007). 
		 Analyzing assessors and products in sorting tasks: 
		 DISTATIS, theory and applications. 
		 \emph{Food Quality and Preference}, \bold{18}, 627--640.
		 
		 Abdi, H., Dunlop, J.P., & Williams, L.J. (2009). 
		 How to compute reliability estimates and 
		 display confidence and tolerance intervals 
		 for pattern classiffers using the Bootstrap 
		 and 3-way multidimensional scaling (DISTATIS). 
		 \emph{NeuroImage}, \bold{45}, 89--95.
		 		 
		Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). 
		STATIS and DISTATIS: 
		Optimum multi-table principal component analysis 
		and three way metric multidimensional scaling. 
		\emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 
		\bold{4}, 124--167.
		
		 
The  \eqn{R_V} coefficient is described in 

 Abdi, H. (2007). RV coefficient and congruence coefficient. 
 In N.J. Salkind (Ed.): 
 \emph{Encyclopedia of Measurement and Statistics}. 
 Thousand Oaks (CA): Sage. pp. 849--853.	
 
 Abdi, H. (2010). Congruence: Congruence coefficient,
  RV coefficient, and Mantel Coefficient. 
 In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): 
 \emph{Encyclopedia of Research Design.} 
 Thousand Oaks (CA): Sage. pp. 222--229.	 
		
		(These papers are available from  \url{www.utdallas.edu/~herve})
}
\author{Herve Abdi
}
%\note{
% More  notes
%}

%  other sections like Warning with \section{Warning }{....} 

\seealso{
	\code{\link{GraphDistatisAll}} 
	\code{\link{GraphDistatisBoot}} 
	\code{\link{GraphDistatisCompromise}} 
	\code{\link{GraphDistatisPartial}} 	
	\code{\link{GraphDistatisRv}} 
	\code{\link{DistanceFromSort}}
	\code{\link{BootFactorScores}}
	\code{\link{BootFromCompromise}}
% See Also as \code{\link{help}}, 
}
\examples{
# 1. Load the DistAlgo data set (available from the DistatisR package)
data(DistAlgo)
# DistAlgo is a 6*6*4 Array (face*face*Algorithm)
#-----------------------------------------------------------------------------
# 2. Call the DISTATIS routine with the array of distance (DistAlgo) as parameter
DistatisAlgo <- distatis(DistAlgo)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distatis}
\keyword{mds}% __ONLY ONE__ keyword per line
