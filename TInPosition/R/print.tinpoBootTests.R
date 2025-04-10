#' Print results from TInPosition Bootstrap Ratio Tests
#' 
#' Print bootstrap ratio tests results from the TInPosition.
#' 
#' 
#' @param x an list that contains items to make into the tinpoBootTests class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton and Cherise Chin-Fatt
#' @keywords print
print.tinpoBootTests <-
function (x,...) {

  res.tinpoBootTests <- x
  if (!inherits(res.tinpoBootTests, "tinpoBootTests")) stop ("no convenient data")
  cat("**TInPosition Bootstrap Ratio Tests data**\n")
  cat("*Contains the following objects:\n\n")
  res <- array("", c(3, 2), list(1:3, c("name", "description")))
  
  res[1,] <- c("$sig.boot.ratios","Items with significant bootstrap ratios (labeled as TRUE).")
  res[2,] <- c("$boot.ratios","The bootstrap ratio (BSR) of items.")
  res[3,] <- c("$critical.value","The critical value used for the BSR test.")  
  
  print(res)

}
