#' pause
#' 
#' A replication of MatLab pause function.
#' 
#' 
#' @usage pause(x = 0)
#' @param x optional. If x>0 a call is made to \code{\link{Sys.sleep}}. Else,
#' execution pauses until a key is entered.
#' @author Derek Beaton (but the pase of which is provided by Phillipe Brosjean
#' from the R mailing list.)
#' @references
#' 
#' Copied from:\cr https://stat.ethz.ch/pipermail/r-help/2001-November/
#' @keywords misc
#' @export pause
pause <-
function (x=0) { 
	if(x > 0){
		Sys.sleep(x)
	}else{
		cat("Hit <enter> to continue...")
		readline()
		invisible()
	}
}
