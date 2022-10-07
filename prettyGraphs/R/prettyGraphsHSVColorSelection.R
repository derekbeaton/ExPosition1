#wonderful stuff from here: http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/




#' Algorithmically select HSV colors.
#' 
#' This function uses an HSV color selection algorithm to create HSV color
#' palletes.
#' 
#' 
#' @param n.colors number of colors needed
#' @param offset numeric (decimal/percentage). This number decides the distance
#' between indices for color selection. If NULL, the golden ratio is selected.
#' @param h numeric. The initial hue (see \code{\link{hsv}}).
#' @param s numeric. The initial saturation (see \code{\link{hsv}}).
#' @param v numeric. The initial value (see \code{\link{hsv}}).
#' @return \item{a matrix}{a matrix of colors are returned.}
#' @author Derek Beaton\cr\cr HSV selection from here:\cr
#' http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
#' @seealso \code{\link{prettyGraphsColors}},
#' \code{\link{prettyGraphsColorSelection}}
#' @keywords graphs
#' @export prettyGraphsHSVColorSelection
prettyGraphsHSVColorSelection <- function(n.colors=1,offset=NULL,h=13,s=0.75,v=0.75){

	if(is.null(offset)){
		offset <- abs((1-sqrt(5))/2)
	}
	these.cols <- c()
	for(i in 1:n.colors){
		h <- h + offset
		h <- h %% 1
		these.cols <- c(these.cols,hsv(h,s,v))
	}
	return(as.matrix(these.cols))
}

#plot(1:ncolors,1:ncolors,col=these.cols,cex=3,pch=20)
