#' Print results from InPosition Bootstraps
#' 
#' Print bootstrap results from the InPosition.
#' 
#' 
#' @param x an list that contains items to make into the inpoBoot class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton and Cherise Chin-Fatt
#' @keywords print
#' @export print.inpoBoot
print.inpoBoot <-
function (x,...) {

  res.inpoBoot <- x
  if (!inherits(res.inpoBoot, "inpoBoot")) stop ("no convenient data")
  cat("**InPosition Bootstrap output data**\n")
  cat("*Contains the following objects:\n\n")
  res <- array("", c(2, 2), list(1:2, c("name", "description")))
  
  res[1,] <- c("$tests","Data for bootstrap ratio (BSR) tests.")
  res[2,] <- c("$boots","The array (rows, columns, depth) of bootstrap resampled & projected data.")
  
  print(res)

}
