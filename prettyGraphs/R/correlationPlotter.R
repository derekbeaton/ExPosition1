#' correlationPlotter
#' 
#' Computes and plots a correlation circle (used in multivariate analyses).
#' Correlation is computed between measured items and components (factors,
#' dimensions, principal axes, etc...).
#' 
#' 
#' @param data_matrix A set of data (i.e., original measures and observations)
#' @param factor_scores One set of factor scores that were computed from the
#' original data matrix.
#' @param x_axis Which axis is the x-axis? Default is 1.
#' @param y_axis Which axis is the y-axis? Default is 2.
#' @param col A single-column matrix of colors for each data point.
#' @param pch A single-column matrix of pch for each data point. Indicates
#' which point style to use for each item. See \code{\link{par}}.
#' @param xlab A label to be placed along the x-axis.
#' @param ylab A label to be placed along the y-axis.
#' @param main A title to be placed at the top of the graph.
#' @param axis.lwd numeric. Line width for the axes.
#' @param circle.lwd numeric. Line width for the circle.
#' @param circle.col color for the circle
#' @param asp numeric. Aspect ratio (see \code{asp} in \code{\link{par}}).
#' @param dev.new boolean. If TRUE, \code{\link{dev.new}} is called internally
#' to create new device. If FALSE, a device must already be open.
#' @author Derek Beaton
#' @keywords graphs multivariate
#' @export correlationPlotter
correlationPlotter <-
function(data_matrix,factor_scores,x_axis=1,y_axis=2,
	col=NULL,pch=NULL,xlab="",ylab="",
	main="",axis.lwd=3,circle.lwd=3,circle.col="#00000040",
	asp=1,dev.new=TRUE){
		
	if(nrow(data_matrix)==nrow(factor_scores)){
		loadings <- cor(data_matrix,factor_scores)
	}
	else if(ncol(data_matrix)==nrow(factor_scores)){
		loadings <- cor(t(data_matrix),factor_scores)
	}else{
		print("Dimension mismatch. Please check data_matrix and factor_scores.")
		return(NULL)	
	}
	loadings <- replace(loadings,is.na(loadings),0)

	if(dev.new){
		dev.new()
	}
	
	#if(!is.null(xlab) && !is.null(ylab)){
		plotCircle(xlab=xlab,ylab=ylab,main=main,asp=asp,
		axis.lwd=axis.lwd,circle.lwd=circle.lwd,circle.col=circle.col)	
	# }else{
		# plotCircle(xlab=paste("Component ",x_axis,sep=""),ylab=paste("Component ",y_axis,sep=""),main=main,asp=asp,
		# axis.lwd=axis.lwd,circle.lwd=circle.lwd,circle.col=circle.col)
	# }
	
	if(is.null(col)){
		col <- colorVectorIsNull(loadings)$oc
	}
	if(is.null(pch)){
		pch <- as.matrix(rep(21,nrow(loadings)))
	}	


	os <- cbind(rep(0,nrow(loadings)+1),rep(0,nrow(loadings)+1))
	new.mat <- matrix(0,(nrow(loadings)*2)+1,2)
	new.mat[seq(1,nrow(new.mat),2),] <- os
	new.mat[seq(2,nrow(new.mat),2),] <- loadings[,c(x_axis,y_axis)]
	points(new.mat,type="l",col="black")
	
	prettyPlot(loadings,col=col,display_names=TRUE,display_points=TRUE,
		pch=pch,x_axis=x_axis,y_axis=y_axis,axes=FALSE,dev.new=FALSE,new.plot=FALSE)
	
}
