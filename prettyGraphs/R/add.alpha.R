##This function is stolen from here: http://lamages.blogspot.com/2013/04/how-to-change-alpha-value-of-colours-in.html
##With some slight modifcations.


#' add.alpha
#' 
#' A function to add alpha levels to RGB colors.
#' 
#' 
#' @param col color from colors()
#' @param alpha alpha level (between 0 and 1)
#' @note This code was created by Markus Gesmann. Derek Beaton included the
#' code in prettyGraphs because it is a versatile function and used in
#' prettyGraphs. See:
#' https://magesblog.com/post/2013-04-30-how-to-change-alpha-value-of-colours-in/
#' @author Markus Gesmann
#' @keywords misc
#' @export add.alpha
add.alpha <- function(col, alpha=0.65){
	apply(
		sapply(col, col2rgb)/255, 
		2, 
		function(x){
			rgb(x[1], x[2], x[3], alpha=alpha)
		}
	)  
}
