% File Chi2Dist.Rd
\name{Chi2Dist}
\alias{Chi2Dist}
\title{
\eqn{\chi^2}{chi2} distance between the rows of 
a rectangular matrix.	
}
\description{
	Computesthe  \eqn{I\times I}{I*I} matrix  \bold{D} which is the \eqn{\chi^2}{chi2} 
	distance matrix between the rows of 
an \eqn{I\times J}{I*J} rectangular matrix \bold{X} (with non-negative elements), 
and provides the \eqn{I\times 1}{I*1} 
\bold{m}  vector of mass (where the mass of a row is the sum of the entries of
this row divided by the grand total of the matrix).
When the distance matrix and 
the associated vector of masses are
used as input to the function \code{\link{mmds}} the
results will give the factor scores
of the correspondence analysis of the  matrix \bold{X}.
The \code{function} is used by the function 
\code{Chi2DistanceFromSort} that computes
the \eqn{\chi^2}{chi2} distance for the results of a sorting task.
}
\usage{Chi2Dist(X)
	}
\arguments{
  \item{X}{A rectangle matrix with non-negative elements}
}
\value{
 	Sends back a list
 	\item{$Distance}{the squared  \eqn{\chi^2}{chi2} distance matrix computed the rows of matrix \bold{X}.}
 	\item{masses}{the vector of masses of the rows of of matrix \bold{X}.}
 } 

\references{
 	The procedure and references are detailled in (Paper available from  \url{www.utdallas.edu/~herve}):
 	Abdi, H. (2007). Distance. 
 	In N.J. Salkind (Ed.): 
 	\emph{Encyclopedia of Measurement and Statistics}. Thousand Oaks (CA): Sage. pp. 304--308.
 	
 	And in:\cr
 	Abdi, H., & Valentin, D. (2006).
\emph{Mathematiques pour les Sciences Cognitives (Mathematics for Cognitive Sciences).}
Grenoble: PUG.

See also  (for the example):

Abdi, H., & Williams, L.J. (2010). Principal component analysis. 
\emph{Wiley Interdisciplinary Reviews: Computational Statistics},\bold{2}, 433--459.


 	}
\author{Herve Abdi}
\seealso{ \code{\link{Chi2DistanceFromSort}} 
	\code{\link{distatis}}
	\code{\link{mmds}}
		}
\examples{
# Here is a data matrix from Abdi & Williams (2012)
# page 449, Table 15. Punctuation of 6 French authors
Punctuation = matrix(c(
  7836,   13112,   6026,
 53655,   102383, 42413, 
115615,   184541, 59226, 
161926,   340479, 62754, 
 38177,   105101, 12670, 
 46371,    58367, 14299),
     ncol =3,byrow = TRUE)
colnames(Punctuation) <-c('Period','Comma','Other')
rownames(Punctuation) <-c('Rousseau','Chateaubriand',
                   'Hugo','Zola','Proust','Giroudoux') 
# 1. Get the Chi2 distance matrix
#     between the rows of Punctuation 
Dres <- Chi2Dist(Punctuation)
# check that the mds of the Chi2 distance matrix
# with CA-masses gives the CA factor scores for I 
# 2. Use function mmds from DistatisR 
#
testmds <- mmds(Dres$Distance,masses=Dres$masses)
# Print the MDS factor scores from mmds
print('Factor Scores from mds')
print(testmds$FactorScores)
print('It matches CA on X (see Abdi & Williams, 2010. Table 16, p. 449)')
# Et voila!
}

\keyword{DistatisR}  