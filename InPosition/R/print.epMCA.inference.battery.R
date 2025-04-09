#' Print Multiple Correspondence Analysis (MCA) Inference results
#' 
#' Print Multiple Correspondence Analysis (MCA) Inference results.
#' 
#' 
#' @param x an list that contains items to make into the
#' epMCA.inference.battery class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton and Cherise Chin-Fatt
#' @keywords print
print.epMCA.inference.battery <-
function (x,...) {

  res.epMCA.inference.battery <- x
  if (!inherits(res.epMCA.inference.battery, "epMCA.inference.battery")) stop ("no convenient data")
  cat("**Results for Multiple Correspondence Analysis**\n")
  cat ("Permutation was performed on ", ncol(res.epMCA.inference.battery$components$eigs.perm),
       "components and bootstrap performed on ", nrow(res.epMCA.inference.battery$fj.boots$tests$boot.ratios), " variables\n")
  cat("*The results are available in the following objects:\n\n")
  res <- array("", c(3, 2), list(1:3, c("name", "description")))
  
  res[1,] <- c("$components","p-values ($p.vals) and permutations ($eigs.perm) for each component.")
  res[2,] <- c("$fj.boots","A list with bootstrap data and tests.")
  res[3,] <- c("$omni","Omnibus test of inertia (only works when MCA is corrected): p-value ($p.val) and permuted inertia ($inertia.perm).")  
  
  print(res)

}
