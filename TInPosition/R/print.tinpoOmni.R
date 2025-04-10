#' Print results from TInPosition Omnibus Permutation Test
#' 
#' Print Omnibus permutation test results from the TInPosition.
#' 
#' 
#' @param x an list that contains items to make into the tinpoOmni class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton and Cherise Chin-Fatt
#' @keywords print
#' @export print.tinpoOmni
print.tinpoOmni <-
function (x,...) {

  res.tinpoOmni <- x
  if (!inherits(res.tinpoOmni, "tinpoOmni")) stop ("no convenient data")
  cat("**TInPosition Omnibus Test data**\n")
  cat("*Contains the following objects:\n\n")
  res <- array("", c(3, 2), list(1:3, c("name", "description")))
  
  res[1,] <- c("$p.val","p-value associated to omnibus permutation test.")
  res[2,] <- c("$inertia.perm","The distribution of permuted inertia values.")
  res[3,] <- c("$inertia","The observed inertia value.")  
  
  print(res)

}
