#' Print results from TInPosition R2 Permutation Test
#' 
#' Print R2 permutation test results from the TInPosition.
#' 
#' 
#' @param x an list that contains items to make into the tinpoR2 class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton and Cherise Chin-Fatt
#' @keywords print
print.tinpoR2 <-
function (x,...) {

  res.tinpoR2 <- x
  if (!inherits(res.tinpoR2, "tinpoR2")) stop ("no convenient data")
  cat("**TInPosition R2 permutation test data**\n")
  cat("*Contains the following objects:\n\n")
  res <- array("", c(3, 2), list(1:3, c("name", "description")))
  
  res[1,] <- c("$p.val","p-value of R2 permutation test.")
  res[2,] <- c("$r2.perm","The distribution of permuted R2 values.")
  res[3,] <- c("$r2","the observed r2 value.")  
  
  print(res)

}
