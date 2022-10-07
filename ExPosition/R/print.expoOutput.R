#' Print results from ExPosition
#' 
#' Print results from the ExPosition. Includes results from a given method and
#' epGraphs.
#' 
#' 
#' @usage \method{printexpoOutput}(x,\dots{})
#' @param x an list that contains items to make into the expoOutput class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton and Cherise Chin-Fatt
#' @seealso \code{\link{epPCA}}, \code{\link{epGraphs}}
#' @keywords print
#' @export print.expoOutput
print.expoOutput <-
function (x,...) {

  res.expoOutput <- x
  if (!inherits(res.expoOutput, "expoOutput")) stop ("no convenient data")
  cat("**ExPosition output data**\n")
  cat("*Contains the following objects:\n\n")
  res <- array("", c(2, 2), list(1:2, c("name", "description")))
  
  res[1,] <- c("$ExPosition.Data","All ExPosition classes output (data, factor scores, contributions, etc...)")
  res[2,] <- c("$Plotting.Data","All ExPosition & prettyGraphs plotting data (constraints, colors, etc...)")
  
  print(res)

}
