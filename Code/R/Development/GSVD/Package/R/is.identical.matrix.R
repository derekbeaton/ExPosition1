is.identical.matrix <- function(x,tol=.Machine$double.eps){

  x <- as.matrix(x)
  x[abs(x) < tol] <- 0

  if(length(unique(c(x)))==1){
    return(TRUE)
  }else{
    return(FALSE)
  }

}
