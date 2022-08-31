\name{tepGraphs}
\alias{tepGraphs}
\title{
tepGraphs: TExPosition plotting function
}
\description{
	TExPosition plotting function which is an interface to \code{\link{prettyGraphs}}.
}

\usage{
tepGraphs(res, x_axis = 1, y_axis = 2, 
	tepPlotInfo = NULL, DESIGN = NULL, 
	fi.col = NULL, fi.pch = NULL, fii.col = NULL, fii.pch = NULL, 
	fj.col = NULL, fj.pch = NULL, col.offset = NULL, 
	constraints = NULL, lv.constraints = NULL,
	xlab = NULL, ylab = NULL, main = NULL, 
	lvPlots = TRUE, lvAgainst = TRUE,
	contributionPlots = TRUE, correlationPlotter = TRUE, 
	showHulls = 1, graphs = TRUE)
}

\arguments{
  \item{res}{results from TExPosition}
  \item{x_axis}{which component should be on the x axis?}
  \item{y_axis}{which component should be on the y axis?}
  \item{tepPlotInfo}{A list (\code{$Plotting.Data}) from \code{tepGraphs} or \code{TExPosition}.}      
  \item{DESIGN}{A design matrix to apply colors (by pallete selection) to row items}
  \item{fi.col}{A matrix of colors for the group items. If NULL, colors will be selected.}
  \item{fi.pch}{A matrix of pch values for the group items. If NULL, pch values are all 21.}    
  \item{fii.col}{A matrix of colors for the row items (observations). If NULL, colors will be selected.}
  \item{fii.pch}{A matrix of pch values for the row items (observations). If NULL, pch values are all 21.}  
  \item{fj.col}{A matrix of colors for the column items. If NULL, colors will be selected.}
  \item{fj.pch}{A matrix of pch values for the column items. If NULL, pch values are all 21.}
  \item{col.offset}{A numeric offset value. Is passed to \code{\link{createColorVectorsByDesign}}.}
  \item{constraints}{Plot constraints as returned from \code{\link{prettyPlot}}. If NULL, constraints are selected.}        
  \item{lv.constraints}{Plot constraints for latent variables. If NULL, constraints are selected.}          
  \item{xlab}{x axis label}
  \item{ylab}{y axis label}
  \item{main}{main label for the graph window}
  \item{lvPlots}{a boolean. If TRUE, latent variables (X, Y) are plotted. If FALSE, latent variables are not plotted.}
  \item{lvAgainst}{a boolean. If TRUE, latent variables (X, Y) are plotted against each other. If FALSE, latent variables are plotted like factor scores.}
  \item{contributionPlots}{a boolean. If TRUE (default), contribution bar plots will be created.}
  \item{correlationPlotter}{a boolean. If TRUE (default), a correlation circle plot will be created. Applies to PCA family of methods (CA is excluded for now).}
  \item{showHulls}{a value between 0 and 1 to make a peeled hull at that percentage. All values outside of 0-1 will not plot any hulls.}  
 \item{graphs}{a boolean. If TRUE, graphs are created. If FALSE, only data associated to plotting (e.g., constraints, colors) are returned.}  
}
\details{
tepGraphs is an interface between \code{\link{TExPosition}} and \code{\link{prettyGraphs}}. 
}
\value{
	The following items are bundled inside of $Plotting.Data:\cr
\item{$fii.col}{the colors that are associated to the individuals (row items; $fii).}	
\item{$fii.pch}{the pch values associated to the individuals (row items; $fii).}
\item{$fi.col}{the colors that are associated to the groups ($fi).}
\item{$fi.pch}{the pch values associated to the groups ($fi).}
\item{$fj.col}{the colors that are associated to the column items ($fj).}
\item{$fj.pch}{the pch values associated to the column items ($fj).}
\item{$constraints}{axis constraints for the plots (determines end points of the plots).}
}
\author{
Derek Beaton
}

\seealso{
 \code{\link{prettyGraphs}}
}
\examples{
	#this is for TExPosition's iris data
	data(ep.iris)
	bada.iris <- tepBADA(ep.iris$data,DESIGN=ep.iris$design,
	  make_design_nominal=FALSE,graphs=FALSE)
	#there are only 2 components, not 3.		
	bada.iris.plotting.data <- tepGraphs(bada.iris,x_axis=1,y_axis=2)
}
\keyword{ multivariate }
\keyword{ graphs }
\keyword{ misc }
