#' Print TExPosition results
#' 
#' Print TExPosition results.
#' 
#' 
#' @param x an list that contains items to make into the texpoOutput class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton, Cherise Chin-Fatt
#' @keywords print
#' @export print.texpoOutput
print.texpoOutput <-
function (x,...) {

  res.texpoOutput <- x
  if (!inherits(res.texpoOutput, "texpoOutput")) stop ("no convenient data")
  cat("**TExPosition output data**\n")
  cat("*Contains the following objects:\n\n")
  res <- array("", c(2, 2), list(1:2, c("name", "description")))
  
  res[1,] <- c("$TExPosition.Data","All TExPosition classes output (data, factor scores, contributions, etc...)")
  res[2,] <- c("$Plotting.Data","All TExPosition & prettyGraphs plotting data (constraints, colors, etc...)")
  
  print(res)

}
