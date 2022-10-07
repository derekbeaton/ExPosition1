#http://stackoverflow.com/questions/3789968/generate-a-list-of-primes-in-r-up-to-a-certain-number


#' Algorithmically select colors from prettyGraphs
#' 
#' This function uses prime numbers to select colors from
#' \code{\link{prettyGraphsColors}}.
#' 
#' 
#' @param n.colors number of colors needed
#' @param offset numeric. Should be a prime number, if it is not, the closest
#' prime is selected. This number decides the distance between indices for
#' color selection.
#' @param starting.color numeric. This is the starting location (e.g., color)
#' in a matrix of \code{\link{prettyGraphsColors}}.
#' @return \item{a matrix}{a matrix of colors are returned.}
#' @author Derek Beaton\cr\cr prime number selection from here:\cr
#' http://stackoverflow.com/questions/3789968/generate-a-list-of-primes-in-r-up-to-a-certain-number
#' @seealso \code{\link{prettyGraphsColors}},
#' \code{\link{prettyGraphsHSVColorSelection}}
#' @keywords graphs
#' @export prettyGraphsColorSelection
prettyGraphsColorSelection <- function(n.colors=1,offset=NULL,starting.color=163){
	if(is.null(offset)){
		offset <- 19
	}
	##stolen
	primest <- function(n){
	    p <- 2:n
	    i <- 1
	    while (p[i] <= sqrt(n)) {
	        p <-  p[p %% p[i] != 0 | p==p[i]]
	        i <- i+1
	    }
	    return(p[length(p)])
	}	
	##stolen
	getPrime <- function(offset){
		div <- 2:floor(sqrt(offset))
		if(!any(offset %% div == 0)) {return(offset)}
		else{primest(offset)}				
	}	
	offset<-getPrime(offset)
	the.colors <- prettyGraphsColors()
	if(round(starting.color) < 0 || round(starting.color) > length(the.colors)){
		starting.color <- 163
	}	
	#the.seq <- round(seq(starting.color,length(the.colors)*n.colors,offset) %% length(the.colors))
	return(as.matrix(the.colors[(seq(1,n.colors*offset,offset) + (starting.color-1)) %% length(the.colors)]))
	
}
