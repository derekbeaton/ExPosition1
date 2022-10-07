#' peeledHull
#' 
#' Computes and plots a peeled hull around a set of points onto a current
#' graphics device.
#' 
#' 
#' @param data_matrix A set of data you would like to plot on 2 dimensions
#' (e.g., a scatter plot). Can be original data or factor scores or anything
#' with at least 2 columns.
#' @param x_axis Which axis is the x-axis? Default is 1.
#' @param y_axis Which axis is the y-axis? Default is 2.
#' @param percentage The percentage of points that should be enveloped by the
#' hull.
#' @param col The color of the hull (see col in plot()).
#' @param lwd The thickness of the hull line (see lwd in plot())
#' @param lty The line type (see lty in plot()).
#' @note The code for this function was (barely) adapted from Michael
#' Greenacre's book on correspondence analysis. All credit for this code should
#' go to Michael Greenacre; I only turned it into a function. The original code
#' can be found at http://carme-n.org/?sec=code2
#' @author Derek Beaton turned Michael Greenacre's code into a function.
#' @references Greenacre, M. J. (2007). Correspondence Analysis in Practice.
#' \emph{Chapman and Hall}.
#' @keywords graphs multivariate
#' @export peeledHull
peeledHull <-
function(data_matrix,x_axis=1,y_axis=2,percentage=1,col="black",lwd=3,lty=1){
	nsim <- length(data_matrix[,x_axis])
	#I would prefer to use while(TRUE){}, rather than repeat.
	repeat{
		hpts <- chull(data_matrix[,c(x_axis,y_axis)])
		#if data_matrix[-hpts,] is only 1 row, npts is NULL.
		npts <- nrow(data_matrix[-hpts,c(x_axis,y_axis)])
		if((npts/nsim < percentage) || is.null(npts)){ 
			break 
		}
		data_matrix <- data_matrix[-hpts,]
	}
	hpts <- c(hpts,hpts[1])
	lines(data_matrix[hpts,c(x_axis, y_axis)], lwd=(lwd*1.5), lty=lty,col="black")
	lines(data_matrix[hpts,c(x_axis, y_axis)], lwd=lwd, lty=lty,col=col)
	invisible()
}
