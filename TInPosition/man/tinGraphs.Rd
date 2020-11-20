\name{tinGraphs}
\alias{tinGraphs}
\title{
tinGraphs: TInPosition plotting function
}
\description{
	TInPosition plotting function which is an interface to \code{\link{prettyGraphs}}.
}
\usage{
tinGraphs(res, DESIGN = NULL, x_axis = NULL, y_axis = NULL, inference.info = NULL, 
	color.by.boots = TRUE, boot.cols = c("plum4", "darkseagreen", "firebrick3"), 
	fi.col = NULL, fi.pch = NULL, fii.col = NULL, fii.pch = NULL, 
	fj.col = NULL, fj.pch = NULL, col.offset = NULL, 
	constraints = NULL, xlab = NULL, ylab = NULL, main = NULL, 
	bootstrapBars = TRUE, correlationPlotter = TRUE, 
	showHulls = 0.95)
}
\arguments{
  \item{res}{results from TExPosition}
  \item{DESIGN}{A design matrix to apply colors (by pallete selection) to row items}
  \item{x_axis}{which component should be on the x axis?}
  \item{y_axis}{which component should be on the y axis?}
  \item{inference.info}{Inference data as output by TInPosition (of class tinpoOutput).}
  \item{color.by.boots}{a boolean. If TRUE, items are colored by bootstrap ratio test. Items larger than \code{critical.value} are colored 'plum4' on the horizontal component, 'darkseagreen' on the vertical component, or 'firebrick3' if the item is significant on both components (to be visualized). If FALSE, the color of the items will be used.}
  \item{boot.cols}{vector of colors: \code{c(horizontal component color, vertical component color, color when item is significant on both)}.}
  \item{fi.col}{A matrix of colors for the group items. If NULL, colors will be selected.}
  \item{fi.pch}{A matrix of pch values for the group items. If NULL, pch values are all 21.}    
  \item{fii.col}{A matrix of colors for the row items (observations). If NULL, colors will be selected.}
  \item{fii.pch}{A matrix of pch values for the row items (observations). If NULL, pch values are all 21.}  
  \item{fj.col}{A matrix of colors for the column items. If NULL, colors will be selected.}
  \item{fj.pch}{A matrix of pch values for the column items. If NULL, pch values are all 21.}
  \item{col.offset}{A numeric offset value. Is passed to \code{\link{createColorVectorsByDesign}}.}
  \item{constraints}{Plot constraints as returned from \code{\link{prettyPlot}}. If NULL, constraints are selected.}        
  \item{xlab}{x axis label}
  \item{ylab}{y axis label}
  \item{main}{main label for the graph window}
  \item{bootstrapBars}{a boolean. If TRUE (default), bootstrap ratio bar plots will be created.}
  \item{correlationPlotter}{a boolean. If TRUE (default), a correlation circle plot will be created. Applies to PCA family of methods (CA is excluded for now).}
  \item{showHulls}{a value between 0 and 1 to make a peeled hull at that percentage. All values outside of 0-1 will not plot any hulls.}  
}
\details{
tinGraphs is an interface between \code{\link{TInPosition}} and \code{\link{prettyGraphs}}. 
}
\author{
Derek Beaton
}
\seealso{
 \code{\link{prettyGraphs}},  \code{\link{inGraphs}}
}
\examples{
	#this is for TExPosition's iris data
	data(ep.iris)
	data <- ep.iris$data
	design <- ep.iris$design
	bada.iris <- 
		tepBADA.inference.battery(
		data,DESIGN=design,
		make_design_nominal=FALSE,test.iters=50,graphs=FALSE)		
	tinGraphs(bada.iris,x_axis=1,y_axis=2)
}
\keyword{ multivariate }
\keyword{ graphs }
\keyword{ misc }