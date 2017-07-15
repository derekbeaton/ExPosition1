\name{DistatisR-package}
\alias{DistatisR-package}
\alias{DistatisR}
\alias{DiSTATISR}
\docType{package}
\title{\code{DistatisR:}
DISTATIS Three Way Metric Multidimensional Scaling
}
\description{\code{DistatisR }package
	implements three way multidimensional scaling: DISTATIS and COVSTATIS.
	
Analyses sets of distance (or covariance) matrices collected on the same set of observations
}


\details{
\tabular{ll}{
Package: \tab DistatisR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-07-03\cr
License: \tab GPL-2\cr
Depends: \tab prettyGraphs (>= 2.0.0), car
}
The example shown here comes from Abdi \emph{et al.} (2007),
\code{distatis} paper on the sorting task.
}
\author{Derek Beaton [aut, com, ctb], Cherise Chin Fatt [ctb], & Herve Abdi [aut, cre]
	
Maintainer: Derek Beaton <exposition.software@gmail.com>
}
\references{%
Note: these papers are available from \url{www.utdallas.edu/~herve}

Abdi, H., Valentin, D., O'Toole, A.J., & Edelman, B. (2005). DISTATIS: 
The analysis of multiple distance matrices. 
\emph{Proceedings of the IEEE Computer Society: International Conference on Computer Vision and Pattern Recognition.} 
(San Diego, CA, USA). pp. 42-47.
	
Abdi, H., Valentin, D., Chollet, S., & Chrea, C. (2007).
 Analyzing assessors and products in sorting tasks: DISTATIS, 
 theory and applications. 
 \emph{Food Quality and Preference}, \bold{18}, 627--640.	
	
Abdi, H., & Valentin, D., (2007). 
Some new and easy ways to describe, compare, 
and evaluate products and assessors. 
In D., Valentin, D.Z. Nguyen, L. Pelletier (Eds): 
\emph{New trends in sensory evaluation of food and non-food products}. 
Ho Chi Minh (Vietnam): Vietnam National University & 
Ho Chi Minh City Publishing House. pp. 5--18.	
	
Abdi, H., Dunlop, J.P., & Williams, L.J. (2009).
 How to compute reliability estimates 
 and display confidence and tolerance 
 intervals for pattern classiffers using 
 the Bootstrap and 3-way multidimensional scaling (DISTATIS). 
 \emph{NeuroImage}, \bold{45}, 89--95.

	
Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012).
 STATIS and DISTATIS: Optimum multi-table 
 principal component analysis and three way metric multidimensional scaling. 
 \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, \bold{4}, 124--167.
 
Chollet, S., Valentin, D., & Abdi, H. 
(in press, 2013). The free sorting task. 
In. P.V. Tomasco & G. Ares (Eds), 
\emph{Novel Techniques in Sensory Characterization 
and Consumer Profiling.} Boca Raton: Taylor and Francis.

Valentin, D., Chollet, S., Nestrud, M., & Abdi, H. (in press, 2013). 
Sorting and similarity methodologies. 
In. S. Kemp, S., J. Hort, & T. Hollowood (Eds.), 
\emph{Descriptive Analysis in Sensory Evaluation}.
 London: Wiley-Blackwell.
   
}

% if other standard keywords, one per line, from file KEYWORDS in the R documentation 
%
\keyword{package}
\seealso{
% Optional links to other man pages, e.g. 
% \code{\link{}}
\code{\link{distatis}}
\code{\link{BootFactorScores}}
\code{\link{BootFromCompromise}}	
\code{\link{DistanceFromSort}}
\code{\link{distatis}}
\code{\link{GraphDistatisAll}}
\code{\link{GraphDistatisBoot}}
\code{\link{GraphDistatisCompromise}}
\code{\link{GraphDistatisPartial}}
\code{\link{GraphDistatisRv}}
\code{\link{mmds}}
\code{\link[prettyGraphs:prettyGraphs-package]{prettyGraphs}} 
}

 \examples{
# Here we use the sorting task from Abdi et al, 2007 paper.
# where 10 Assessors sorted 8 beers

#-----------------------------------------------------------------------------
#  1. Get the data from the 2007 sorting example
#      this is the way they look from Table 1 of 
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
#     (alternatively we could have read a csv file)   
# 1.5 Example of how to read a csv filw
# Sort <- read.table("BeeerSortingTask.csv", header=TRUE, 
#   sep=",", na.strings="NA", dec=".", row.names=1, strip.white=TRUE)

#-----------------------------------------------------------------------------
# 2. Create the set of distance matrices (one distance matrix per assessor)
#    (uses the function DistanceFromSort)
DistanceCube <- DistanceFromSort(Sort)
#-----------------------------------------------------------------------------
# 3. Call the DISTATIS routine with the cube of distance as parameter
testDistatis <- distatis(DistanceCube)
# The factor scores for the beers are in
# testDistatis$res4Splus$F
# the factor scores for the assessors are in (RV matrice)
#  testDistatis$res4Cmat$G

#-----------------------------------------------------------------------------
# 4. Inferences on the beers obtained via bootstrap
#    here we use two different bootstraps:
#    1. Bootstrap on factors (very fast but could be too liberal 
#         when the number of assessors is very large)
#    2. Complete bootstrap obtained by computing sets of compromises
#       and projecting them (could be significantly longer because a lot
#       of computations is required)
# 
# 4.1 Get the bootstrap factor scores (with default 1000 iterations)
BootF <- BootFactorScores(testDistatis$res4Splus$PartialF)
#
# 4.2 Get the boostrap from full bootstrap (default niter = 1000)
 F_fullBoot <- BootFromCompromise(DistanceCube,niter=1000) 
 

#-----------------------------------------------------------------------------
# 5. Create the Graphics
# 5.1 an Rv map
 rv.graph.out <- GraphDistatisRv(testDistatis$res4Cmat$G) 
# 5.2 a compromise plot
 compromise.graph.out <- GraphDistatisCompromise(testDistatis$res4Splus$F) 
# 5.3 a partial factor score plot
 partial.scores.graph.out <- 
 	GraphDistatisPartial(testDistatis$res4Splus$F,testDistatis$res4Splus$PartialF)
# 5.4 a bootstrap confidence interval plot 
 #5.4.1 with ellipses
 boot.graph.out.ell <- GraphDistatisBoot(testDistatis$res4Splus$F,BootF)  
 #or 
 # boot.graph.out <- GraphDistatisBoot(testDistatis$res4Splus$F,F_fullBoot)  
 #5.4.2 with hulls
 boot.graph.out.hull <- GraphDistatisBoot(testDistatis$res4Splus$F,BootF,ellipses=FALSE)  
 #or 
 # boot.graph.out <- GraphDistatisBoot(testDistatis$res4Splus$F,F_fullBoot,ellipses=FALSE)
#5.5 all the plots at once
all.plots.out <- 
	GraphDistatisAll(testDistatis$res4Splus$F,testDistatis$res4Splus$PartialF,
		BootF,testDistatis$res4Cmat$G)  
 

}
