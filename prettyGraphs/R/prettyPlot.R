#' prettyPlot
#' 
#' prettyPlot creates simple, crisp, publication-style quality graphics for
#' multivariate analyses.
#' 
#' All items after contributions (i.e., axes onward) are parameters for very
#' fine-grained detail. They are quite powerful but in most cases not required.
#' 
#' @param data_matrix A set of data you would like to plot on 2 dimensions
#' (e.g., a scatter plot). Can be original data or factor scores or anything
#' with at least 2 columns.
#' @param x_axis Which axis is the x-axis? Default is 1.
#' @param y_axis Which axis is the y-axis? Default is 2.
#' @param col any acceptable color format. A single-column matrix of colors for
#' each data point. A single value can be used.
#' @param pch A single-column matrix of pch for each data point. A single value
#' can be used. Indicates which point style to use for each item. See
#' \code{\link{par}}.
#' @param cex a single column of continuous values. A single value can be used.
#' Indicates the size of the points. See \code{\link{par}}. Used
#' multiplicatively for points with \code{contributions}.
#' @param text.cex A replacement for the cex parameter in text() (to avoid
#' collisions with cex). Used additively for points with \code{contributions}.
#' See \code{\link{par}} and \code{\link{text}}.
#' @param pos integer between 1-4. Determines position of text for points. See
#' \code{\link{par}}.
#' @param xlab A label to be placed along the x-axis.
#' @param ylab A label to be placed along the y-axis.
#' @param main A title to be placed at the top of the graph.
#' @param display_names boolean. If TRUE, the names of the points will be
#' displayed on the plot.
#' @param display_points boolean. If TRUE, the points will be displayed on the
#' plot.
#' @param constraints A list that contains the plot constraints. Default is
#' NULL (constraints are computed each time).
#' @param contributionCircles boolean. If TRUE, items plotted will vary in
#' size, dependent on amount of contribution to the variance.
#' @param contributions A matrix of contribution values for data_matrix (should
#' also be the same size & dimensionality as data_matrix)
#' @param axes boolean. If TRUE, a new set of axes are plotted.
#' @param fg.line.width integer. Determines thickness of foreground (default:
#' solid) axis lines, see \code{\link{points}} and \code{lwd} under
#' \code{\link{par}}.
#' @param fg.type character or string Determines type of points for foreground
#' (default: solid) axis lines, see \code{\link{points}} and \code{type} under
#' \code{\link{par}}.
#' @param fg.col any acceptable color format. Determines color for foreground
#' (default: solid) axis lines, see \code{\link{points}} and \code{col} under
#' \code{\link{par}}.
#' @param bg.line.width integer. Determines thickness of background (default:
#' dashed) axis lines, see \code{\link{abline}} and \code{lwd} under
#' \code{\link{par}}.
#' @param bg.lty integer. Determines type of background (default: dashed) axis
#' lines, see \code{\link{abline}} and \code{lty} under \code{\link{par}}.
#' @param bg.col any acceptable color format. Determines color of background
#' (default: dashed) axis lines, see \code{\link{abline}} and \code{col} under
#' \code{\link{par}}.
#' @param flip boolean. If TRUE, exchanges bg and col for all applicable pch
#' values.
#' @param asp numeric. Aspect ratio, see \code{\link{par}}.
#' @param findBounds boolean. If TRUE, finds the min and max of each plotted
#' axes. If FALSE, the largest value is used for all constraints.
#' @param dev.new boolean. If TRUE, \code{\link{dev.new}} is called internally
#' to create new device. If FALSE, a device must already be open.
#' @param new.plot boolean. If TRUE, \code{\link{plot}} is called for a new
#' plot on the current device. If FALSE, items will be overlayed much like
#' \code{\link{points}}.
#' @return Returns a three item list:
#' 
#' \item{col}{A matrix of colors where each element is the color for each item
#' plotted.} \item{pch}{A matrix of pch values where each element is the pch
#' number for each item plotted.} \item{constraints}{A list (from
#' \code{\link{minmaxHelper}}) of the plot constraints (i.e., min and max for
#' axes)}
#' @author Derek Beaton
#' @keywords graphs multivariate
#' @export prettyPlot
prettyPlot <-
function(data_matrix,x_axis=1,y_axis=2,col=NULL,pch=NULL,cex=NULL,text.cex=NULL,pos=3,xlab="",ylab="",main="",display_names=TRUE,display_points=TRUE,constraints=NULL,contributionCircles=FALSE,contributions=NULL,axes=TRUE,fg.line.width=3,fg.type="l",fg.col="black",bg.line.width=1.5,bg.lty=3,bg.col = "black",flip=FALSE,asp=1,findBounds=TRUE,dev.new=TRUE,new.plot=TRUE){
	
	#I want to always send back colors and constraints.
	#I need a different type of checker here...
	if(is.null(col)){
		col <- colorVectorIsNull(data_matrix)$oc
	}
	col <- as.matrix(col)
	if(length(col)==1){
		col <- as.matrix(rep(col,nrow(data_matrix)))
	}else if(nrow(col)!=nrow(data_matrix)){
		col <- colorVectorIsNull(data_matrix)$oc
	}
	
	if(is.null(pch)){
		pch <- as.matrix(rep(21,nrow(data_matrix)))
	}
	pch <- as.matrix(pch)
	if(length(pch)==1){
		pch <- as.matrix(rep(pch,nrow(data_matrix)))
	}else if(nrow(pch)!=nrow(data_matrix)){
		pch <- as.matrix(rep(21,nrow(data_matrix)))
	}
	
	if(is.null(cex)){
		cex <- as.matrix(rep(1,nrow(data_matrix)))
	}
	cex <- as.matrix(cex)
	if(length(cex)==1){
		cex <- as.matrix(rep(cex,nrow(data_matrix)))
	}else if(nrow(cex)!=nrow(data_matrix)){
		cex <- as.matrix(rep(1,nrow(data_matrix)))
	}
	
	if(is.null(text.cex)){
		text.cex <- as.matrix(rep(0.8,nrow(data_matrix)))
	}
	
	text.cex <- as.matrix(text.cex)	
	if(length(text.cex)==1){
		text.cex <- as.matrix(rep(text.cex,nrow(data_matrix)))
	}else if(nrow(text.cex)!=nrow(data_matrix)){
		text.cex <- as.matrix(rep(0.8,nrow(data_matrix)))
	}
	
	#I only need constraints if I am making a new window.
	check.constraints <- minmaxHelper(data_matrix,data_matrix,x_axis,y_axis,findBounds=findBounds)	
	if(!is.null(constraints)){
	#if it is not null
		if(("minx" %in% names(constraints)) && ("maxx" %in% names(constraints)) && ("miny" %in% names(constraints)) && ("maxy" %in% names(constraints))){
			#and if it meets criteria, use it. This way, if FALSE, it should also go here.
			check.constraints <- list(minx=constraints$minx,miny=constraints$miny,maxx=constraints$maxx,maxy=constraints$maxy)
		}
	}
	constraints <- check.constraints	
	
	
	if(	(display_names==FALSE && display_points==FALSE) ){
		#For your health!
		print("Sorry, but you cannot have display_points and display_names set to FALSE. Nothing would have been plotted!")
		display_points<-TRUE
	}
	
	#this assumes you already have a device ready to go.
	if(dev.new){
		dev.new()	
	}
	
	if(new.plot){
		plot(c(0,0),c(0,0),type="n",col="white",axes=FALSE,xlab=xlab,ylab=ylab,ylim=c(constraints$miny,constraints$maxy),xlim=c(constraints$minx,constraints$maxx),main=main,asp=asp)
	}
			
	#make a new plot on a device.
	if(axes){		
		#determine axis points
		axis_list <- determineAxesPosition(constraints)		
		#plot the axes
		makeAxes(axis_list,fg.line.width= fg.line.width,fg.type= fg.type,fg.col= fg.col,bg.line.width= bg.line.width,bg.lty=bg.lty,bg.col = bg.col)
	}	
	#am I displaying points?
	if(display_points){
		plotPoints(data_matrix,col,x_axis,y_axis,cex=cex,pch=pch,contributionCircles=contributionCircles,contributions=contributions,flip=flip)
	}
	#am I displaying names?
	if(display_names){
		plotText(data_matrix,col,x_axis,y_axis,pos=pos,text.cex=text.cex,contributionCircles=contributionCircles,contributions=contributions)
	}		
	
	# return(list(col=col,pch=pch,constraints=constraints))
	invisible(list(col=col,pch=pch,constraints=constraints))
}
