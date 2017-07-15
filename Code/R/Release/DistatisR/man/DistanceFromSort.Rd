\name{DistanceFromSort}
\alias{DistanceFromSort}
\title{Creates a 3-dimensional distance array from the results of a sorting task.}
\description{
	Takes the results from a (plain) sorting task
	where \eqn{K} assessors sort \eqn{I} observations into (mutually exclusive)
	groups (i.e., one object is in one an only one group).
	\code{DistanceFromSort} creates an
	\eqn{I \times I \times K}{I*I*K} array of distance in which
	each of the \eqn{k} "slices" stores the (sorting) distance
	matrix of the \eqn{k}th assessor. 
	In one of these distance matrices, a value of 0 at the intersection of a row
	and a column means that the object represented by the row and the object
	represented by the column were sorted together (i.e., they are a distance of 0),
	and a vaue of 1 means these two objects were put into different groups.
	
	The ouput ot the function \code{DistanceFromSort}
	is used as input for the function \code{\link{distatis}}. 
}
\usage{
DistanceFromSort(X)
}
%  'usage' for other objects documented here.
\arguments{
  \item{X}{gives the results of a sorting task (see example below) as a 
  	objects (row) by assessors (columns) matrix.
}
}
\details{The input should have assessors as columns and observations as rows
	(see example below)
}
\value{
		\code{DistanceFromSort} returns a 
		 \eqn{I\times I \times K}{I*I*K} 
	array of distances
%   Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
% ...
}
\references{See examples in

 Abdi, H., Valentin, D., Chollet, S., & Chrea, C. (2007). 
	Analyzing assessors and products in sorting tasks: 
	DISTATIS, theory and applications. 
        \emph{Food Quality and Preference}, \bold{18}, 627--640.

 Abdi, H., & Valentin, D., (2007). 
 Some new and easy ways to describe, compare,
 and evaluate products and assessors. 
 In D., Valentin, D.Z. Nguyen, L. Pelletier (Eds) 
 \emph{New trends in sensory evaluation of food and non-food products.}
 Ho Chi Minh (Vietnam): Vietnam National 
 University-Ho chi Minh City Publishing House. pp. 5--18.
 
		These papers are available from  \url{www.utdallas.edu/~herve}
}
\author{Herve Abdi
}
%\note{
% if any 
%}

%  other sections like Warning with \section{Warning }{....} 

\seealso{\code{\link{distatis}}
}
\examples{
#  1. Get the data from the 2007 sorting example
#      this is the eay they look from Table 1 of 
#      Abdi et al. (2007).
#                       Assessors
#                  1 2 3 4 5 6 7 8 9 10
# Beer        Sex  f m f f m m m m f m
#            -----------------------------                         
#Affligen          1 4 3 4 1 1 2 2 1 3
#Budweiser         4 5 2 5 2 3 1 1 4 3
#Buckler_Blonde    3 1 2 3 2 4 3 1 1 2
#Killian           4 2 3 3 1 1 1 2 1 4
#St. Landelin      1 5 3 5 2 1 1 2 1 3
#Buckler_Highland  2 3 1 1 3 5 4 4 3 1
#Fruit Defendu     1 4 3 4 1 1 2 2 2 4
#EKU28             5 2 4 2 4 2 5 3 4 5

#	
# 1.1. Create the
#     Name of the Beers
BeerName <- c('Affligen', 'Budweiser','Buckler Blonde',
              'Killian','St.Landelin','Buckler Highland',
              'Fruit Defendu','EKU28')
# 1.2. Create the name of the Assessors 
#      (F are females, M are males)
Juges <- c('F1','M2', 'F3', 'F4', 'M5', 'M6', 'M7', 'M8', 'F9', 'M10')

# 1.3. Get the sorting data
SortData <- c(1, 4, 3, 4, 1, 1, 2, 2, 1, 3,
              4, 5, 2, 5, 2, 3, 1, 1, 4, 3,
              3, 1, 2, 3, 2, 4, 3, 1, 1, 2,
              4, 2, 3, 3, 1, 1, 1, 2, 1, 4,
              1, 5, 3, 5, 2, 1, 1, 2, 1, 3,
              2, 3, 1, 1, 3, 5, 4, 4, 3, 1,
              1, 4, 3, 4, 1, 1, 2, 2, 2, 4,
              5, 2, 4, 2, 4, 2, 5, 3, 4, 5)
# 1.4 Create a data frame            
Sort <- matrix(SortData,ncol = 10, byrow= TRUE, dimnames = list(BeerName, Juges))
# 
#-----------------------------------------------------------------------------
# 2. Create the set of distance matrices (one distance matrix per assessor)
#    (use the function DistanceFromSort)
DistanceCube <- DistanceFromSort(Sort)
#-----------------------------------------------------------------------------
# 3. Call the DISTATIS routine with the cube of distance 
#       obtained from DistanceFromSort as a parameter for the distatis function
testDistatis <- distatis(DistanceCube)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{DistatisR}

