#' A stopping mechanism if resampling will take too long.
#' 
#' This function asks the user if they want to continue with resampling if the
#' total time for resampling takes more than 10 minutes. It also provides an
#' estimate of how long resampling takes. This function is required for
#' \code{InPosition} and \code{TInPosition} and we do not recommend others use
#' it.
#' 
#' 
#' @param cycle.time Is the subtraction of two calls to \code{proc.time}.
#' @note If computation time is expected to take more than 10 minutes and
#' \code{interactive()} is TRUE, this asks the user if they would like to
#' continue. If 'Y', looping continues. If 'N', it stops.
#' 
#' If computation time is expected to takre more than 10 minutes and
#' \code{interactive()} is FALSE, the function will proceed as is and perform
#' inference tests.
#' 
#' A progress bar is provided so the user can see how long the tests will take.
#' 
#' See inference battery functions for details.
#' @author Derek Beaton
#' @export continueResampling
continueResampling <- function(cycle.time){
	iter.time <- cycle.time/60 #seconds for iters into minutes.	
	print(paste("It is estimated that your iterations will take",round(iter.time,digits=2),"minutes.",sep=" "))
	if(interactive()){
		valueCaptured <- ""
		if(iter.time > 10){ #greater than 10 minutes.
			while(tolower(valueCaptured) != "n" && tolower(valueCaptured) != "y"){
				valueCaptured <- readline("Do you want to proceed: Y/N")	
			}
			# if(tolower(valueCaptured)=="y"){
				# print("Proceeding with resample-based tests. Please take note of the progress bar.")
				# return(TRUE)
			# }
			if(tolower(valueCaptured)=="n"){
				print(paste("Resample-based tests exiting. Skipping tests.",sep=" "))
				return(FALSE)
			}
		}
		return(TRUE)
	}else{
		print("R is not in interactive() mode. Resample-based tests will be conducted. Please take note of the progress bar.")
		return(TRUE)
	}
}
