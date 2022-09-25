#' Print C matrix results
#'
#' Print C matrix results.
#'
#' @param x a list that contains items to make into the Cmat class.
#' @param ... inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton
#' @keywords print
#' @keywords internal
#' @export
print.Cmat <- function (x,...) {
	res.Cmat <- x
	if (!inherits(res.Cmat, "Cmat")) stop ("no convenient data")
	cat("**Results related to the C matrix**\n")
	cat("*The results are available in the following objects:\n\n")

	if(res.Cmat$compact){
		#list(alpha=alpha)
		res <- array("", c(1, 2), list(1:1, c("name", "description")))
		res[1,] <- c("$alpha","alpha weights")
	}else{
		#list(C = C, eigVector = eigC$vector, eigValues = eigC$values, tau = eigC$tau, G = eigC$G, alpha=alpha)
		res <- array("", c(9, 2), list(1:9, c("name", "description")))
		res[1,] <- c("$C","The C matrix")
		res[2,] <- c("$eigVector","Eigenvectors of C")
		res[3,] <- c("$eigValues","Eigenvalues of C")
		res[4,] <- c("$tau", "Explained variance for components")
		res[5,] <- c("$G","The factor score matrix for C")
		res[6,] <- c("$ctr","The contribution matrix for C")
		res[7,] <- c("$cos2","The squared cosines matrix for the C")
		res[8,] <- c("$d2","The squared Euclidean distances for the C")
		res[9,] <- c("$alpha","The alpha weights")
	}

	print(res)
}
