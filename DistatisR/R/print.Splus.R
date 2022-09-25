#' Print S+ matrix results
#'
#' Print S+ matrix results.
#'
#'
#' @param x a list that contains items to make into the Splus class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton
#' @keywords print
#' @keywords internal
#' @export
print.Splus <-
function (x,...) {


	res.Splus <- x
	if (!inherits(res.Splus, "Splus")) stop ("no convenient data")
	cat("**Results for S+ Matrix**\n")
	cat("*The results are available in the following objects:\n\n")

	if(res.Splus$compact){
		#list(Splus=Splus)
		res <- array("", c(1, 2), list(1:1, c("name", "description")))
		res[1,] <- c("$Splus","S+ Matrix")
	}else{
		res <- array("", c(11, 2), list(1:11, c("name", "description")))
		res[1,] <- c("$SCP","The 3D array of the cross product matrices")
		res[2,] <- c("$eigValues","The Eigenvalues of the Compromise")
		res[3,] <- c("$eigVectors","The Eigenvectors of the Compromise")
		res[4,] <- c("$tau","The Percentage of Explained Variance per Eigenvalue")
		res[5,] <- c("$F","Factor scores for the compromise")
		res[6,] <- c("$ctr","The contribution matrix for the compromise")
		res[7,] <- c("$cos2","The squared cosines matrix for the compromise")
		res[8,] <- c("$d2","The squared Euclidean distances for the compromise")
		res[9,] <- c("$PartialF","The Partial Factor Scores for the compromise")
		res[10,] <- c("$ProjectionMatrix", "The Projection Matrix")
		res[11,] <- c("$Splus","The S+ Matrix (i.e., the compromise)")
	}
	print(res)
}
