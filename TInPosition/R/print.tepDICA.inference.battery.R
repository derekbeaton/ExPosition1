#' Print tepDICA.inference.battery results
#' 
#' Print tepDICA Inference results.
#' 
#' 
#' @param x an list that contains items to make into the
#' tepDICA.inference.battery class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton, Cherise Chin-Fatt
#' @keywords print
#' @export print.tepDICA.inference.battery
print.tepDICA.inference.battery <-
function (x,...) {

  res.tepDICA.inference.battery <- x
  if (!inherits(res.tepDICA.inference.battery, "tepDICA.inference.battery")) stop ("no convenient data")
  cat("**Results for Discriminant Correspondence Analysis (DICA) Inference Battery**\n")
  cat ("Permutation was performed on ", ncol(res.tepDICA.inference.battery$components$eigs.perm),
       "components and bootstrap performed on ", nrow(res.tepDICA.inference.battery$fj.boot.data$tests$boot.ratios), " variables and", nrow(res.tepDICA.inference.battery$fi.boot.data$tests$boot.ratios), " groups\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(5, 2), list(1:5, c("name", "description")))
  
  res[1,] <- c("$components","p-values ($p.vals) and permutations ($eigs.perm) for each component.")
  res[2,] <- c("$boot.data","A list with bootstrap data and tests.")
  res[3,] <- c("$omni","Omnibus test of inertia: p-value ($p.val) and permuted inertia ($inertia.perm).")     
  res[4,] <- c("$r2","Test of R-squared: p-value ($p.val) and permuted R2 ($r2.perm)")     
  res[5,] <- c("$loo.data","Leave One Out (LOO) cross validation data.")         
  
  print(res)
}
