is.empty.matrix <- function(x,tol=.Machine$double.eps){

  x <- as.matrix(x)
  x[abs(x) < tol] <- 0

  if(sum(x)==0){
    return(TRUE)
  }else{
    return(FALSE)
  }

}
