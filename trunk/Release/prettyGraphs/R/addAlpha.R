##This function is stolen from here: http://lamages.blogspot.com/2013/04/how-to-change-alpha-value-of-colours-in.html
##With some slight modifcations.
add.alpha <- function(col, alpha=0.65){
	apply(
		sapply(col, col2rgb)/255, 
		2, 
		function(x){
			rgb(x[1], x[2], x[3], alpha=alpha)
		}
	)  
}
